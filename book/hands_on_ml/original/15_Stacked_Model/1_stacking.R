# ********************************************************************************
# Title   : 積み上げモデル
# Chapter : 6
# Memo    : モデルのスタッキング
# URL     : https://bradleyboehmke.github.io/HOML/regularized-regression.html
# Support : https://koalaverse.github.io/homlr/
# H2O     : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(magrittr)
library(rsample)
library(recipes)
library(h2o)
library(compareDF)


# H2O起動
h2o.init()


# 0. メモ ----------------------------------------------


# ＜ポイント＞
#・複数の学習器をアンサンブルにする方法を学ぶ



# 1. 準備 ----------------------------------------------

# データ取得
ames <- read_csv("data/ames.csv")


# データ分割
set.seed(123) 
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# データ確認
ames_train %>% dim()
ames_test %>% dim()



# 2. 前処理 ----------------------------------------------

# レシピ作成
blueprint <- 
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_other(all_nominal(), threshold = 0.005)


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




# 3. 個別モデルの作成 ----------------------------------------------

# ＜ポイント＞
#・同じトレーニングセットでトレーニングする
#・同じ数のCVフォールドでトレーニング
#・同じフォールド割り当てを使用して、同じ観測値を使用する
#  --- fold_assignment = "Modulo"
#・すべてのモデルからの相互検証された予測を保存する
#　--- keep_cross_validation_predictions = TRUE



# 正則化回帰ベース学習器。
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

# ランダムフォレストベース学習器
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

# XGBoostベース学習器
# --- Ma cor Linux Only 
# best_xgb <- 
#   h2o.xgboost(x = X, 
#               y = Y, 
#               training_frame = train_h2o, 
#               ntrees = 5000, 
#               learn_rate = 0.05,
#               max_depth = 3, 
#               min_rows = 3, 
#               sample_rate = 0.8, 
#               categorical_encoding = "Enum", 
#               nfolds = 10, 
#               fold_assignment = "Modulo", q
#               keep_cross_validation_predictions = TRUE, 
#               seed = 123, 
#               stopping_rounds = 50, 
#               stopping_metric = "RMSE", 
#               stopping_tolerance = 0
#               )




# 4. アンサンブルモデルの作成 ----------------------------------------------


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



# 5. パフォーマンス評価 ----------------------------------------------

# ラッパー関数定義
# --- RMSEを取得
get_rmse <- function(model) {
  results <- h2o.performance(model, newdata = test_h2o)
  results@metrics$RMSE
}


# 各モデルのRMSE
list(best_glm, best_rf, best_gbm) %>%map_dbl(get_rmse)


# アンサンブルモデルのRNSE
ensemble_tree@model$training_metrics@metrics$RMSE


# 予測値の相関分析
# --- 各モデルの相関が高いので今回はアンサンブルのメリットは大きくなさそう
data.frame(
  GLM_pred = best_glm@model$cross_validation_holdout_predictions_frame_id$name %>% h2o.getFrame() %>% as.vector(),
  RF_pred  = best_rf@model$cross_validation_holdout_predictions_frame_id$name %>% h2o.getFrame() %>% as.vector(),
  GBM_pred = best_gbm@model$cross_validation_holdout_predictions_frame_id$name %>% h2o.getFrame() %>% as.vector()
) %>% cor()



# 6.グリッドサーチ----------------------------------------------

# グリッド検索
# --- パターン生成
hyper_grid <-
  list(max_depth = c(1, 3, 5),
       min_rows = c(1, 5, 10),
       learn_rate = c(0.01, 0.05, 0.1),
       learn_rate_annealing = c(0.99, 1),
       sample_rate = c(0.5, 0.75, 1),
       col_sample_rate = c(0.8, 0.9, 1)
       )

# パターン数の確認
hyper_grid %>% expand.grid() %>% dim()

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



