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




# 2.個別モデルの学習 ----------------------------------------------------------

# クロスバリデーション
nfolds <- 5


# GBM (Gradient Boosting Machine)
# --- 訓練 + クロスバリデーション
my_gbm <- 
  h2o.gbm(x = x,
          y = y,
          training_frame = train,
          distribution = "bernoulli",
          ntrees = 10,
          max_depth = 3,
          min_rows = 2,
          learn_rate = 0.2,
          nfolds = nfolds,
          fold_assignment = "Modulo",
          keep_cross_validation_predictions = TRUE,
          seed = 1)

# DRF (Distributed Random Forest)
# --- 訓練 + クロスバリデーション
my_rf <- 
  h2o.randomForest(x = x,
                   y = y,
                   training_frame = train,
                   ntrees = 50,
                   nfolds = nfolds,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 1)



# 3.アンサンブルモデルの学習 ----------------------------------------------------------

# 個別モデルをもとにアンサンブルモデルを構築
# --- metalearner_algorithmでアンサンブルのアルゴリズムを指定
ensemble <- 
  h2o.stackedEnsemble(x = x,
                      y = y,
                      training_frame = train,
                      model_id = "my_ensemble_binomial",
                      base_models = list(my_gbm, my_rf), 
                      metalearner_algorithm = "AUTO")




# 4.パフォーマンス比較 ----------------------------------------------------------

# パフォーマンス評価
# --- テストデータを用いて検証
perf <- ensemble %>% h2o.performance(newdata = test)
perf %>% print()


# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
perf_rf_test <- h2o.performance(my_rf, newdata = test)

baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))


# Generate predictions on a test set (if neccessary)
pred <- ensemble %>% h2o.predict(newdata = test)

