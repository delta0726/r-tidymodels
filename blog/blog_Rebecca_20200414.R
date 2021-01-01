# Title     : Rebecca's Blog
# Objective : TODO
# Created by: Owner
# Created on: 2020/05/26
# URL       : http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/



# ＜ポイント＞
# - tidymodelsのworkflowを使ってパラメータチューニングを含むモデル構築プロセスを示している
# - 最終モデルの作成に向けてのプロセスが秀逸



# load the relevant tidymodels libraries
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(dials)
library(mlbench)


#%% 準備 ------------------------------------

# データロード
data(PimaIndiansDiabetes)
diabetes_orig <- PimaIndiansDiabetes


# データ確認
# --- Y: diabetes (pos/neg)
diabetes_orig %>% as_tibble()
diabetes_orig %>% glimpse()


# Plot
diabetes_orig %>%
  ggplot(aes(x = triceps)) +
    geom_histogram()


# データ加工
# --- Replace 0 to NA
diabetes_clean <-
  diabetes_orig %>%
    mutate_at(vars(triceps, glucose, pressure, insulin, mass),
              function(.var) {
                if_else(condition = (.var == 0), # if true (i.e. the entry is 0)
                        true = as.numeric(NA),  # replace the value with NA
                        false = .var # otherwise leave it as it is
                        )
              })


# 確認
diabetes_clean %>% as_tibble()




#%% モデル用データ ------------------------------------

# データ分割
set.seed(234589)
diabetes_split <- diabetes_clean %>% initial_split(prop = 3/4)
diabetes_train <- diabetes_split %>% training()
diabetes_test  <- diabetes_split %>% testing()

diabetes_split %>% training() %>% as_tibble()
diabetes_split %>% analysis() %>% as_tibble()




#%% 前処理レシピの設定 ------------------------------------

# レシピ定義
# --- workflowにいれるためprep()はしない
# --- step_normalize: Zscoreに変換
# --- step_knnimpute: k近傍法を用いた欠損値補完
diabetes_recipe <-
  recipe(diabetes ~ pregnant + glucose + pressure + triceps +
           insulin + mass + pedigree + age,
         data = diabetes_clean) %>%
  step_normalize(all_numeric()) %>%
  step_knnimpute(all_predictors())


# 確認
# --- prep()を実行していないので、前処理後データはまだ作成されていない
# --- 以下のデータが全く同じものを返すため、未実行であることが確認できる
diabetes_recipe$template %>% head()
diabetes_clean %>% head()


# レシピの実行
# --- juice()でレシピ作成に使用したデータセットを抽出
# --- bake()で新たなデータセットにレシピを適用することも可能
diabetes_recipe %>%
  prep(diabetes_train) %>%
  juice() %>%
  print()



#%% モデリング ------------------------------------

# モデル構築
# --- ランダムフォレスト
# --- mtryをチューニング
rf_model <-
  rand_forest() %>%
    set_args(mtry = tune()) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")



#%% ワークフローの設定 ------------------------------------

# set the workflow
rf_workflow <-
  workflow() %>%
    add_recipe(diabetes_recipe) %>%
    add_model(rf_model)


# 確認
rf_workflow %>% print()
rf_workflow %>% names()
rf_workflow %>% glimpse()



#%% ハイパーパラメータのチューニング ------------------------------------

# クロスバリデーション用のデータ作成
# --- 訓練データを使用
diabetes_cv <- diabetes_train %>% vfold_cv()
diabetes_cv %>% print()


# チューニング範囲の設定
rf_grid <- expand.grid(mtry = c(3, 4, 5))
rf_grid %>% print()


# チューニング
# --- モデルのmtryをチューニング
# --- tune_grid()でグリッドサーチを実行
# --- metric_set()を用いることで{yardstick}のメトリックをパッケージ化した関数を作成して評価に使用
rf_tune_results <-
  rf_workflow %>%
    tune_grid(resamples = diabetes_cv,
              grid = rf_grid,
              metrics = metric_set(accuracy, roc_auc)
              )


# 確認
# --- バリデーションデータの.metric列にチューニング結果が出力される
rf_tune_results %>% print()
rf_tune_results$.metrics[[1]]


# チューニング結果の取得
# --- Fold間で平均や標準誤差を算出
rf_tune_results %>% collect_metrics()


# 計算証明
# --- collect_metrics()
rf_tune_results %>%
  select(id, .metrics) %>%
  unnest() %>%
  group_by(mtry, .metric) %>%
  summarize(mean = mean(.estimate, na.rm = TRUE),
            n    = n(),
            std_err = plotrix::std.error(.estimate, na.rm = TRUE)) %>%
  ungroup()



# ベストパラメータの選択
# --- メトリックを選択して最良パラメータを選択
param_final <-
  rf_tune_results %>%
    select_best(metric = "accuracy")


# 確認
param_final %>% print()



#%% モデルの完成 ------------------------------------

# 現在のワークフロー
rf_workflow %>% print()


# ワークフローの完成
# --- tune::finalize_workflow()でパラメータを更新
rf_workflow <-
  rf_workflow %>%
    finalize_workflow(param_final)


# 完成後のワークフロー
# --- mtryがtune()から実数に変更されている
rf_workflow %>% print()



#%% 予測 ------------------------------------

# 予測データの作成
# --- tune::last_fit()にrsplitオブジェクトを渡すとテストデータを使用して予測する
rf_fit <-
  rf_workflow %>%
    last_fit(split = diabetes_split)


# 確認
rf_fit %>% print()
rf_fit$.predictions %>% print()


# メトリックの抽出
test_performance <- rf_fit %>% collect_metrics()
test_performance %>% print()


# 予測データの抽出
test_predictions <- rf_fit %>% collect_predictions()
test_predictions %>% print()


# 予測データの抽出
# --- {purrr}を用いてもcollect_predictions()と同様の操作可能
test_predictions_2 <- rf_fit %>% pull(.predictions)
test_predictions_2


#%% 評価 ------------------------------------

# 混合行列の取得
test_predictions %>%
  conf_mat(truth = diabetes, estimate = .pred_class)


# プロット
test_predictions %>%
  ggplot() +
  geom_density(aes(x = .pred_pos, fill = diabetes),
               alpha = 0.5)




#%% 最終モデルの作成 ------------------------------------


# 完全なデータセットでモデルを訓練
# --- 最終的にはテストデータではなく全体のデータセットでモデルを学習
# --- workflow::fit()を使用
final_model <- rf_workflow %>% fit(diabetes_clean)
final_model %>% print()


# 予測用の新しいデータ
new_woman <- tribble(~pregnant, ~glucose, ~pressure, ~triceps, ~insulin, ~mass, ~pedigree, ~age,
                     2, 95, 70, 31, 102, 28.2, 0.67, 47)
new_woman



# 予測
final_model %>%
  predict(new_data = new_woman)



#%% 変数重要度分析 ------------------------------------

# 最終モデルの学習結果を取得
ranger_obj <- final_model %>% pull_workflow_fit() %>% .$fit


# 確認
ranger_obj %>% print()
ranger_obj %>% names()


# 変数重要度を取得
ranger_obj$variable.importance
ranger_obj$variable.importance %>% barplot()