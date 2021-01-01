# Title     : Tutorial on tidymodels for Machine Learning
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/25
# URL       : https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/



# ライブラリのロード
library("conflicted")
library("tidyverse")
library("tidyquant")
library("tidymodels")
library("ggrepel")
library("corrplot")
library("DataExplorer")
library("doFuture")
library("tictoc")


# Conflict Setting
conflict_prefer("filter", "dplyr")


# Plot Theme
ggplot2::theme_set(theme_tq())



# データロード
data("diamonds")



#%% EDA Process -----------------------------------------

# データ概要
diamonds %>% as_tibble()
diamonds %>% glimpse()


# 相関プロット
diamonds %>%
    sample_n(2000) %>%
    mutate_if(is.factor, as.numeric) %>%
    select(price, everything()) %>%
    cor %>%
    {.[order(abs(.[, 1]), decreasing = TRUE),
       order(abs(.[, 1]), decreasing = TRUE)]} %>%
    corrplot(method = "number", type = "upper", mar = c(0, 0, 1.5, 0),
             title = "Correlations between price and various features of diamonds")



#%% データ準備 -------------------------------------------------------

# データ分割
# --- 訓練データの割合を少なくしている
set.seed(1243)
dia_split <- diamonds %>% initial_split(prop = .1, strata = price)
dia_train <- dia_split %>% training()
dia_test  <- dia_split %>% testing()


# 確認
dim(dia_train)
dim(dia_test)


# CVデータの作成
dia_vfold <- dia_train %>% vfold_cv(v = 3, repeats = 1, strata = price)
dia_vfold %>% print()


# CVデータを分割
# --- 分析データと評価データ
# --- training()とtesting()を実行しているのと同じ
dia_vfold %>%
  mutate(df_ana = map(splits, analysis),
         df_ass = map(splits, assessment))



#%% プロット --------------------------------------------------------

# 散布図
# --- Y: Price  X: carat
# --- 非線形な関係であることを確認
qplot(carat, price, data = dia_train) +
    scale_y_continuous(trans = log_trans(), labels = function(x) round(x, -2)) +
    geom_smooth(method = "lm", formula = "y ~ poly(x, 4)") +
    labs(title = "Nonlinear relationship between price and carat of diamonds",
         subtitle = "The degree of the polynomial is a potential tuning parameter")


# ヒストグラム
# --- 各ファクターともに歪んでいる
# --- 全て正の値なので対数の適用が可能
# --- 対数を取ると歪みが小さくなる傾向
dia_train %>% plot_histogram()
dia_train %>% mutate_if(is.numeric, log) %>% plot_histogram()



#%% 前処理 --------------------------------------------------------

# レシピ定義
# --- prep()まで実行
dia_rec <-
    recipe(price ~ ., data = dia_train) %>%
        step_log(all_outcomes()) %>%
        step_normalize(all_predictors(), -all_nominal()) %>%
        step_dummy(all_nominal()) %>%
        step_poly(carat, degree = 2) %>%
        prep()


# 前処理の確認
# --- juice()はレシピで設定したデータを抽出する
# --- 数値データの対数化とZスコア化が確認できる
# --- 名義データがダミー変数になっていることが確認できる
# --- caratが2次のpolyとなっていることが確認できる
dia_juiced <- dia_rec %>% juice()
dia_juiced %>% glimpse()
diamonds %>% glimpse()


# Check Data
dia_juiced[, c("depth", "table", "price", "carat_poly_1", "carat_poly_2")] %>%
  plot_histogram()




#%% 線形回帰モデル ---------------------------------

# ＜ポイント＞
# - Listed DataFrameにおけるモデルワークフローの書き方を確認する
# - チューニングには{Tune}が必要であることを再確認する


# モデル定義
# --- 線形回帰モデル
lm_model <-
    linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")



# 単純な学習 ---------------------------------------------

# 学習
lm_fit1 <- lm_model %>% fit(price ~ ., data = dia_juiced)
lm_fit1


# モデル評価
# --- 全体レベル
lm_fit1$fit %>% glance() %>% t()


# モデル評価
# --- ファクターレベル
lm_fit1 %>%
  tidy() %>%
  arrange(desc(abs(statistic)))


# モデル評価
# --- レコードレベル
lm_predicted <-
  lm_fit1$fit %>%
        augment(data = dia_juiced) %>%
        rowid_to_column()


# データ確認
# --- 推定統計量など
lm_predicted %>%
  select(rowid, price, .fitted:.std.resid)


# プロット
ggplot(lm_predicted, aes(.fitted, price)) +
    geom_point(alpha = .2) +
    ggrepel::geom_label_repel(aes(label = rowid),
                              data = filter(lm_predicted, abs(.resid) > 2)) +
    labs(title = "Actual vs. Predicted Price of Diamonds")



# CVで学習 ---------------------------------------------

# CVデータの作成
dia_vfold %>% print()


# モデルワークフロー
# --- リサンプリングをtibbleで行うイメージをつかむ
# --- {tune}を使ったほうが効果的なチューニングが可能
lm_fit2 <-
  dia_vfold %>%
    # データ分割
    mutate(df_ana = map(splits, analysis),
           df_ass = map(splits, assessment)) %>%
    # レシピ作成＆適用
    mutate(recipe = map(df_ana, ~prep(dia_rec, training = .x)),
           df_ana = map(recipe, juice),
           df_ass = map2(recipe, df_ass, ~bake(.x, new_data = .y))) %>%
    # 学習
    mutate(model_fit  = map(df_ana, ~fit(lm_model, price ~ ., data = .x))) %>%
    # 予測
    mutate(model_pred = map2(model_fit, df_ass, ~predict(.x, new_data = .y)))


# 確認
lm_fit2 %>% print()
lm_fit2 %>% select(id, recipe:model_pred)


# AssessmentデータからPriceの実績値と予測値を取得
lm_preds <-
    lm_fit2 %>%
    mutate(res = map2(df_ass, model_pred, ~data.frame(price = .x$price,
                                                      .pred = .y$.pred))) %>%
    select(id, res) %>%
    tidyr::unnest(res) %>%
    group_by(id)

# 確認
lm_preds

# モデルパフォーマンスの評価
# --- CVのfold間でばらつきが生じるものかを確認
# --- インプリケーションはあまり得ることができないかも
lm_preds %>%
  metrics(truth = price, estimate = .pred)




#%% ランダムフォレスト ---------------------------------

# モデルのチューニング設定 ------------------------

# モデル構築
# --- Tuning: mtry
rf_model <-
    rand_forest(mtry = tune()) %>%
    set_mode("regression") %>%
    set_engine("ranger", num.threads = parallel::detectCores())


# モデルパラメータの確認
rf_model %>% parameters()


# {dial}のmtryのチューニング設定
mtry()


# チューニング範囲の更新
rf_model %>%
    parameters() %>%
    update(mtry = mtry(c(1L, 5L)))


# パラメータ確認
rf_model %>%
    parameters() %>%
    finalize(x = select(juice(prep(dia_rec)), -price)) %>%
    pull("object")



# レシピのチューニング設定 ------------------------

#%% Tuning Recipe
# --- tuning: degree of poly
dia_rec2 <-
  recipe(price ~ ., data = dia_train) %>%
    step_log(all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    step_dummy(all_nominal()) %>%
    step_poly(carat, degree = tune())


# パラメータ確認
dia_rec2 %>%
    parameters() %>%
    pull("object")


# ワークフロー設定 -----------------------------

# ワークフロー定義
rf_wflow <-
    workflow() %>%
    add_model(rf_model) %>%
    add_recipe(dia_rec2)


# チューニング範囲の設定
rf_param <-
  rf_wflow %>%
    parameters() %>%
    update(mtry = mtry(range = c(3L, 5L)),
           degree = degree_int(range = c(2L, 4L)))


# 確認
rf_param %>% print()


# パターンリストの作成
rf_grid <- rf_param %>% grid_regular(levels = 3)
rf_grid %>% print()



# チューニング -----------------------------------------------------

# コアの取得
# --- 最大数-1
all_cores <- parallel::detectCores(logical = FALSE) - 1


# 並列設定
registerDoFuture()
cl <- makeCluster(all_cores)
plan(future::cluster, workers = cl)



# チューニング
# --- 並列あり(進捗表示なし)
tic()
rf_search <-
  rf_wflow %>%
    tune_grid(grid = rf_grid,
              resamples = dia_vfold,
              param_info = rf_param,
              control = control_grid(verbose = TRUE, allow_par = TRUE))
toc()

# チューニング
# --- 並列なし
tic()
rf_search <-
  rf_wflow %>%
    tune_grid(grid = rf_grid,
              resamples = dia_vfold,
              param_info = rf_param,
              control = control_grid(verbose = TRUE, allow_par = FALSE))
toc()


# 確認
rf_search %>% print()


# プロット
rf_search %>%
    autoplot(metric = "rmse") +
    labs(title = "Results of Grid Search for Two Tuning Parameters of a Random Forest")



# チューニング結果の評価 -----------------------------------------------------

# 上位パフォーマンスを確認
# --- RMSE
rf_search %>% show_best("rmse", n = 9)


# 最上位パフォーマンスのパラメータ取得
rf_search %>% select_best(metric = "rmse")



# 標準誤差の小さいモデルを取得
rf_search %>% select_by_one_std_err( mtry, degree, metric = "rmse")



# 最終モデルの構築 --------------------------------------------------------

# 最終パラメータの確定
rf_param_final <-
  rf_search %>%
    select_by_one_std_err(mtry, degree, metric = "rmse")


# 最終モデルの構築
rf_wflow_final <-
  rf_wflow %>%
    finalize_workflow(rf_param_final)


# 確認
rf_wflow_final %>% print()


# 学習
rf_wflow_final_fit <-
  rf_wflow_final %>%
    fit(data = dia_train)


# レシピの取得
dia_rec3 <- rf_wflow_final_fit %>% pull_workflow_prepped_recipe()
dia_rec3 %>% print()


# 学習済モデルの取得
rf_final_fit <- rf_wflow_final_fit %>% pull_workflow_fit()
rf_final_fit %>% print()


#
dia_test_pred <-
  rf_final_fit %>%
    predict(new_data = bake(dia_rec3, dia_test))

rf_final_fit %>% print()