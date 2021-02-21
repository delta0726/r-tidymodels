# ********************************************************************************
# Title   : 積み上げモデル
# Chapter : 6
# Memo    : H2OのAutoML
# URL     : https://bradleyboehmke.github.io/HOML/regularized-regression.html
# Support : https://bradleyboehmke.github.io/HOML/stacking.html#automated-machine-learning
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


# ＜H2OのAutoML対応モデル＞
#・Distributed Random Forest(DRF)
#・Gradiatnt Boosting Model(GBM)
#・Deep Learningt(DL)
#・Distributed Random Forest(DRF)



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


# 3. AutoML ----------------------------------------------

# 実行フラグ
flg <- 1


# AutoML
# --- 60 * 120 を 10 に変更
if (flg == 1){
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
}


# 4. モデル評価 ----------------------------------------------

if (flg == 1){
  auto_ml@leaderboard %>% 
    as.data.frame() %>%
    dplyr::select(model_id, rmse) %>%
    dplyr::slice(1:25)  
}

