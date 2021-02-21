# ********************************************************************************
# Title   : Gradient Boosting
# Chapter : 12
# URL     : https://bradleyboehmke.github.io/HOML/gbm.html
# Support : https://koalaverse.github.io/homlr/notebooks/10-bagging.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(recipes)
library(xgboost)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞
#・バギングは低バイアスで高分散のモデルにより効果的
#・ブースティングは高バイアスで低分散のモデルにより効果的
#・最も強力なアンサンブルアルゴリズムで予測精度は最高級とされる



# 1. データ準備 ----------------------------------------------


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



# 2.前処理  ----------------------------------------------

# レシピ作成
# --- 数値の特徴量だけを抽出
xgb_recipe <- 
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_integer(all_nominal()) %>%
    prep(training = ames_train, retain = TRUE)

# データ作成
xgb_prep <- xgb_recipe %>% juice()
xgb_prep %>% glimpse()



# 3.モデル構築 ----------------------------------------

# データ定義
# --- 行列とベクトルで取得
X <- as.matrix(xgb_prep[setdiff(names(xgb_prep), "Sale_Price")])
Y <- xgb_prep$Sale_Price


set.seed(123)
ames_xgb <- 
  xgb.cv(data = X,
         label = Y,
         nrounds = 6000,
         objective = "reg:linear",
         early_stopping_rounds = 50, 
         nfold = 10,
         params = list(
           eta = 0.1,
           max_depth = 3,
           min_child_weight = 3,
           subsample = 0.8,
           colsample_bytree = 1.0),
         verbose = 0)  

# minimum test CV RMSE
ames_xgb$evaluation_log$test_rmse_mean %>% min()



# hyperparameter grid
hyper_grid <- 
  expand.grid(eta = 0.01,
              max_depth = 3, 
              min_child_weight = 3,
              subsample = 0.5, 
              colsample_bytree = 0.5,
              gamma = c(0, 1, 10, 100, 1000),
              lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
              alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
              rmse = 0,          # a place to dump RMSE results
              trees = 0          # a place to dump required number of trees
              )

# grid search
for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "reg:linear",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$rmse[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

# results
hyper_grid %>%
  filter(rmse > 0) %>%
  arrange(rmse) %>%
  glimpse()

# optimal parameter list
params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 3944,
  objective = "reg:linear",
  verbose = 0
)



# 重要度分析 -----------------------------------------------

# 変数重要度
xgb.fit.final %>% vip::vip() 


