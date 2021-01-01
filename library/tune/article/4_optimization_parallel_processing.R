# Title     : Optimizations and Parallel Processing
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/9
# URL       : https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/



# ＜ポイント＞
# - {doFuture}と組み合わせることで並列処理が可能となる
# - 対象：tune_grid(), tune_bayes(), fit_resamples()



library(tidyverse)
library(tidymodels)
library(doParallel)
library(doFuture)
library(tictoc)



# 前処理 -----------------------------------------------------

# データ分割
# --- 訓練データの割合を少なくしている
set.seed(1243)
dia_split <- diamonds %>% initial_split(prop = .1, strata = price)
dia_train <- dia_split %>% training()
dia_test  <- dia_split %>% testing()


# バリデーションデータの作成
dia_vfold <- dia_train %>% vfold_cv(v = 5, repeats = 1, strata = price)
dia_vfold %>% print()



# チューニング準備 -----------------------------------------------------

# モデル構築
# --- mtryをチューニング
# --- 並列処理をオンにしてよい（チューニングの並列処理と両立できる）
rf_model <-
    rand_forest(mtry = tune()) %>%
    set_mode("regression") %>%
    set_engine("ranger", num.threads = parallel::detectCores())


# レシピ定義
# --- prep()まで実行
dia_rec2 <-
  recipe(price ~ ., data = dia_train) %>%
    step_log(all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    step_dummy(all_nominal()) %>%
    step_poly(carat, degree = tune())


# ワークフロー定義
rf_wflow <-
    workflow() %>%
    add_model(rf_model) %>%
    add_recipe(dia_rec2)


# チューニング範囲の設定
rf_param <-
  rf_wflow %>%
    parameters() %>%
    update(mtry = mtry(range = c(3L, 7L)),
           degree = degree_int(range = c(2L, 6L)))


# 確認
rf_param %>% print()


# パターンリストの作成
rf_grid <- rf_param %>% grid_regular(levels = 5)
rf_grid %>% print()



# チューニング -----------------------------------------------------

# コアの取得
# --- 最大数-1
all_cores <- parallel::detectCores(logical = FALSE) - 1


# 並列処理の設定
registerDoFuture()
cl <- makeCluster(all_cores)
plan(future::cluster, workers = cl)



# チューニング
# --- 並列あり(進捗表示なし)
tic()
rf_search1 <-
  rf_wflow %>%
    tune_grid(grid = rf_grid,
              resamples = dia_vfold,
              param_info = rf_param,
              control = control_grid(verbose = TRUE, allow_par = TRUE))
toc()

## CLEANUP
parallel::stopCluster(cl)


# チューニング
# --- 並列なし
tic()
rf_search2 <-
  rf_wflow %>%
    tune_grid(grid = rf_grid,
              resamples = dia_vfold,
              param_info = rf_param,
              control = control_grid(verbose = TRUE, allow_par = FALSE))
toc()


# 確認
rf_search1 %>% print()
rf_search2 %>% print()