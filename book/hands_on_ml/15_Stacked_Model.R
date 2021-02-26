# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 15 Stacked Models
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/7
# URL       : https://bradleyboehmke.github.io/HOML/stacking.html
#           : https://koalaverse.github.io/homlr/notebooks/15-stacking-models.nb.html
# **********************************************************************************


# ＜ポイント＞
# - これまで個々の学習器をトレーニングする方法を学んだ
# - スタッキングでは、新しい学習アルゴリズムをトレーニングして、いくつかの基本学習者の予測を組み合わせる
# - 最初に、利用可能なトレーニングデータを使用して基本学習者をトレーニングする
# - --- 次にSuperLearnerをトレーニングして、基本学習者の予測に基づいて最終的な予測を行います
#  - スタックされた学習器は個別の基本学習器よりも優れている傾向がある



# ＜目次＞
# 15.1 準備
# 15.2 アイデア
# 15.3 既存モデルのスタッキング
# 15.4 グリッド検索のスタッキング
# 15.5 Auto Machine Learning




# 15.1 準備 ----------------------------------------------

library(tidyverse)
library(tidyquant)
library(magrittr)
library(rsample)
library(recipes)
library(h2o)
library(compareDF)


# H2O起動
h2o.init()


## 15.1.1  データ準備 ----------------

# データロード
ames <- AmesHousing::make_ames()


# データ概要
ames %>% as_tibble()
ames %>% glimpse()



## 15.1.2  前処理 ----------------

# データ分割
set.seed(123) 
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# データ確認
ames_train %>% dim()
ames_test %>% dim()


# レシピ作成
# --- 0.5%以下のグループをotherと表記する
blueprint <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_other(all_nominal(), threshold = 0.005)


# 前処理の確認
blueprint %>%
  prep(training = ames_train, retain = TRUE) %>%
  juice() %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  map(levels)



## 15.1.3  H2Oデータの作成 ----------------

# 訓練データ作成
train_h2o <-
  blueprint %>%
    prep(training = ames_train, retain = TRUE) %>%
    juice() %>%
    as.h2o()


# テストデータ作成
test_h2o <-
  blueprint %>%
    prep(training = ames_train) %>%
    bake(new_data = ames_test) %>%
    as.h2o()


# モデルのラベル作成
Y <- "Sale_Price"
X <- ames_train %>% names() %>% setdiff(Y)
X %>% print()



# 15.2 アイデア ----------------------------------------------

# 15.2.1 共通アンサンブルメソッド

# ＜ポイント＞
# - 複数の学習アルゴリズムをアンサンブルすることで、個別アルゴリズムのどれよりも優れた予測パフォーマンスを実現する
#   --- ランダムフォレストやGBMなど、最近人気のある学習アルゴリズムは内部的にはアンサンブルを使用
#   --- ｢バギング｣と｢ランダムフォレスト｣は多くの決定木から予測を平均化することでバリアンスを減らす（並列的）
#   --- ｢GBM｣は浅い木を順番に組み合わせるアプローチ（直列的）


# 15.2.2 Super Learner Algorithm

# ＜ポイント＞
# - Super Learner Algorithmは３つのフェーズで構成されている
#   --- フェーズ1： アンサンブルをセットする
#   --- フェーズ2： アンサンブルを訓練する
#   --- フェーズ3： 新しいデータで予測する
# - 予測値が基本学習者間で類似しているほど、それらを組み合わせる利点は少なくなる（他のアンサンブルと同じ）



# 15.2.3 利用可能なライブラリ





# 15.3 既存モデルのスタッキング ----------------------------------------------

# ＜ポイント＞
# - 個々の基本学習者モデルを個別にトレーニングしてから、それらを一緒にスタックする
# - 次のアルゴリズムで最高の予測精度を提供する最適なハイパーパラメーターを見つけたとします。



# 15.3.1 個別モデルの作成 ----------------------------------------------

# ＜個別モデルのポイント＞
# - 乱数シードを固定する
# - 同じトレーニングセットでトレーニングする
# - 同じ数のCVフォールドで交差検証を行う。
# - すべてのモデルは、同じ観測が使用されるように同じフォールド割り当てを使用する必要があります
#   --- fold_assignment = "Modulo"
# - すべてのモデルからの相互検証された予測値は保持しておかなければならない
#   --- keep_cross_validation_predictions = TRUE


# GLM
# --- 正則化回帰ベース学習器
best_glm <-
  h2o.glm(x = X,
          y = Y,
          training_frame = train_h2o,
          alpha = 0.1,
          remove_collinear_columns = TRUE,
          nfolds = 10,
          fold_assignment = "Modulo",
          keep_cross_validation_predictions = TRUE,
          seed = 123
          )


# ランダムフォレスト
best_rf <-
  h2o.randomForest(x = X,
                   y = Y,
                   training_frame = train_h2o,
                   ntrees = 1000,
                   mtries = 20,
                   max_depth = 30,
                   min_rows = 1,
                   sample_rate = 0.8,
                   nfolds = 10,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 123,
                   stopping_rounds = 50,
                   stopping_metric = "RMSE",
                   stopping_tolerance = 0
                   )

# GBMベース学習器
best_gbm <-
  h2o.gbm(x = X,
          y = Y,
          training_frame = train_h2o,
          ntrees = 5000,
          learn_rate = 0.01,
          max_depth = 7,
          min_rows = 5,
          sample_rate = 0.8,
          nfolds = 10,
          fold_assignment = "Modulo",
          keep_cross_validation_predictions = TRUE,
          seed = 123,
          stopping_rounds = 50,
          stopping_metric = "RMSE",
          stopping_tolerance = 0
          )




# 15.3.2 アンサンブルモデルの作成 ----------------------------------------------

# アンサンブルモデルを訓練する
# --- ここではdrf(Distributed Random Forest)をメタラーナーとして適用する
ensemble_tree <-
  h2o.stackedEnsemble(x = X,
                      y = Y,
                      training_frame = train_h2o,
                      model_id = "my_tree_ensemble",
                      base_models = list(best_glm, best_rf, best_gbm),
                      metalearner_algorithm = "drf"
                      )


# 確認
ensemble_tree %>% print()



# 15.3.3 パフォーマンス評価 ----------------------------------------------

# ＜ポイント＞
# - このアンサンブルは基本学習者のCV結果に基づいて構築されている
# - 独自の相互検証結果がないため、テストデータを使用してモデル精度を結果を比較する
#   --- このモデルではRMSEは改善していない


# 関数定義
# --- RMSEを取得
get_rmse <- function(model) {
  results <- h2o.performance(model, newdata = test_h2o)
  results@metrics$RMSE
}


# 各モデルのRMSE
# GLM : 41484.
# RF  : 23418.
# GBM : 21220.
list(best_glm, best_rf, best_gbm) %>% map_dbl(get_rmse) %>% as.tibble()


# アンサンブルモデルのRMSE
# Ensemble : 23052.01
ensemble_tree %>% h2o.performance(newdata = test_h2o)@metrics$RMSE



# 15.3.4 予測値の相関 ----------------------------------------------

# ＜ポイント＞
# - 予測値の相関を見ることで、モデル類似性を確認することができる
#   --- RFとGBMは相関係数が0.991と非常に高い


# 予測値の相関分析
# --- 各モデルの相関が高いので今回はアンサンブルのメリットは大きくなさそう
data.frame(
  GLM_pred = best_glm@model$cross_validation_holdout_predictions_frame_id$name %>% h2o.getFrame() %>% as.vector(),
  RF_pred  = best_rf@model$cross_validation_holdout_predictions_frame_id$name %>% h2o.getFrame() %>% as.vector(),
  GBM_pred = best_gbm@model$cross_validation_holdout_predictions_frame_id$name %>% h2o.getFrame() %>% as.vector()
) %>% cor()




# 15.4 グリッドサーチのスタッキング ----------------------------------------------

# ＜ポイント＞
# - 別のアンサンブルアプローチは、同じ基本学習者から生成された複数のモデルをスタックすることに焦点を当てています
#   --- RFとGBMは相関係数が0.991と非常に高い


# グリッド検索
# --- GBMのパラメータ
hyper_grid <-
  list(max_depth = c(1, 3, 5),
       min_rows = c(1, 5, 10),
       learn_rate = c(0.01, 0.05, 0.1),
       learn_rate_annealing = c(0.99, 1),
       sample_rate = c(0.5, 0.75, 1),
       col_sample_rate = c(0.8, 0.9, 1)
       )


# パターン数の確認
hyper_grid %>% as_tibble()


# グリッドサーチの範囲を特定
search_criteria <- list(
  strategy = "RandomDiscrete",
  max_models = 25
)


# Build random grid search
random_grid <-
  h2o.grid(algorithm = "gbm",
           grid_id = "gbm_grid",
           x = X,
           y = Y,
           training_frame = train_h2o,
           hyper_params = hyper_grid,
           search_criteria = search_criteria,
           ntrees = 5000,
           stopping_metric = "RMSE",
           stopping_rounds = 10,
           stopping_tolerance = 0,
           nfolds = 10,
           fold_assignment = "Modulo",
           keep_cross_validation_predictions = TRUE,
           seed = 123
           )


# 7.最良モデルを探す----------------------------------------------

# 結果のソード
# --- RMSEの小さい順
h2o.getGrid(
  grid_id = "gbm_grid",
  sort_by = "rmse"
)


# 最良モデルのパフォーマンスを確認
random_grid@model_ids[[1]] %>%
  h2o.getModel() %>%
  h2o.performance(newdata = test_h2o)



# 8.サーチ結果からモデル統合 ----------------------------------------------

# グリッド検索の結果からアンサンブルモデルを構築
# --- base_modelsにグリッドサーチの結果を投入
# --- アルゴリズムはGBM(Gradient Boosting Model)を使用
ensemble <-
  h2o.stackedEnsemble(x = X,
                      y = Y,
                      training_frame = train_h2o,
                      model_id = "ensemble_gbm_grid",
                      base_models = random_grid@model_ids,
                      metalearner_algorithm = "gbm")

# Eval ensemble performance on a test set
ensemble %>% h2o.performance(newdata = test_h2o)




# 15.5 Auto Machine Learning ----------------------------------------------

# ＜ポイント＞
# - AutoMLは、複数の基本学習器を対象に自動検索を実行し、結果のモデルをスタックする
#   --- 基本学習器のグリッド検索によく似ている
#   --- 単一の基本学習者のパラメーター検索ではなく、異なる基本学習者のハイパーパラメーター設定全体を検索したい
# - AutoMLはプログラミングや個別処理の時間から解放してくれる
#   --- 上級ユーザーにも役立つツール
#   --- ユーザーは｢特徴量収集｣｢モデル解釈｣｢モデルデプロイ｣に時間をとることができるようになる（非常に重要）


# ＜AutoMLの処理＞
# - AutoMLは内部で以下の作業を行っている
#   --- 特徴量エンジニアリング
#   --- モデル検証手順
#   --- モデル選択
#   --- ハイパーパラメーター最適化


# ＜使用する基本学習器＞
# - random forest
# - an extremely-randomized forest
# - a random grid of GBMs
# - a random grid of DNNs
# - stacked ensemble


# AutoML
# --- 実行時間：2時間(60*120) を60秒に変更
auto_ml <-
  h2o.automl(x = X,
             y = Y,
             training_frame = train_h2o,
             nfolds = 5,
             max_runtime_secs = 60,
             max_models = 50,
             keep_cross_validation_predictions = TRUE,
             sort_metric = "RMSE",
             seed = 123,
             stopping_rounds = 50,
             stopping_metric = "RMSE",
             stopping_tolerance = 0)


# モデルパフォーマンスの確認
# --- Learder Boardより取得
auto_ml@leaderboard %>%
  as.data.frame() %>%
  dplyr::select(model_id, rmse) %>%
  dplyr::slice(1:25)
