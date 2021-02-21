# ********************************************************************************
# Title   : 積み上げアンサンブル
# Memo    : H2Oのドキュメントより
# H2O     : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/stacked-ensembles.html#id3
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


# 1.準備 ----------------------------------------------------------

# データ取得
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test  <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")


# 変数ラベルを設定
y <- "response"
x <- train %>% names() %>% setdiff(y)


# Yをファクターに変換
train[,y] <- train[,y] %>% as.factor()
test[,y]  <- test[,y] %>% as.factor()


# データ確認
train %>% as.data.frame() %>% glimpse()
train %>% dim()
test %>% dim()




# 2.グリッドサーチの設定 ----------------------------------------------------------

# ハイパーパラメターの設定
# --- GBM
hyper_params <- 
  list(learn_rate = c(0.01, 0.03),　
       max_depth = c(3, 4, 5, 6, 9),　
       sample_rate =  c(0.7, 0.8, 0.9, 1.0),　
       col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8))


# パターン数の確認
hyper_params %>% expand.grid() %>% dim()


# 検索基準
search_criteria <- 
  list(strategy = "RandomDiscrete", 
       max_models = 3, 
       seed = 1)

# クロスバリデーション
nfolds <- 5


# 3.グリッドサーチの実行 ----------------------------------------------------------

gbm_grid <-
  h2o.grid(algorithm = "gbm",
           grid_id = "gbm_grid_binomial",
           x = x,
           y = y,
           training_frame = train,
           ntrees = 10,
           seed = 1,
           nfolds = nfolds,
           fold_assignment = "Modulo",
           keep_cross_validation_predictions = TRUE,
           hyper_params = hyper_params,
           search_criteria = search_criteria)



# 4.サーチ結果からアンサンブルを作成 ------------------------------------------------

# Train a stacked ensemble using the GBM grid
ensemble <- 
  h2o.stackedEnsemble(x = x,
                      y = y,
                      training_frame = train,
                      model_id = "ensemble_gbm_grid_binomial",
                      base_models = gbm_grid@model_ids)



# 5.パフォーマンス評価 -----------------------------------------------

# 予測結果の取得
perf <- ensemble %>% h2o.performance(newdata = test)

# Compare to base learner performance on the test set
.getauc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(gbm_grid@model_ids, .getauc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)

