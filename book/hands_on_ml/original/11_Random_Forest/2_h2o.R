# ********************************************************************************
# Title   : ランダムフォレスト
# Chapter : 11
# URL     : https://bradleyboehmke.github.io/HOML/random-forest.html
# Support : https://koalaverse.github.io/homlr/notebooks/11-random-forests.nb.html
# H2O     : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/drf.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(ranger)
library(h2o)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)
library(ggRandomForests)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞
#・rangerでのグリッドサーチはパラメータが大きくなるとパターン分だけ計算コストも増える
#  --- デカルトサーチという
#・H2Oはデカルトサーチに加えて｢ランダムグリッドサーチ｣や｢アーリーストッピング｣ができる



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


# 特徴量の数
n_features <- ames_train %>% names() %>% setdiff("Sale_Price") %>% length()
n_features



# 2.H2Oの準備 ----------------------------------------------

# H2O起動
h2o.no_progress()
h2o.init(max_mem_size = "5g")


# H2Oオブジェクトに変換
train_h2o <- ames_train %>% as.h2o()

# YとXの列名を定義
response   <- "Sale_Price"
predictors <- ames_train %>% colnames() %>% setdiff(response)


# 3.パラメータ固定の学習 ----------------------------------------------

# モデル構築
# --- n_feature = 80
# --- n_tree = 800
h2o_rf1 <- 
  h2o.randomForest(x = predictors, 
                   y = response,
                   training_frame = train_h2o, 
                   ntrees = n_features * 10,
                   seed = 123
                   )

# 確認
h2o_rf1




# 4.グリッドサーチ ----------------------------------------------

# パターン設定
hyper_grid <- list(
  mtries = floor(n_features * c(.05, .15, .25, .333, .4)),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)


# パターン数
# --- 240パターン
hyper_grid %>% expand.grid() %>% dim()


# サーチ条件の設定
# --- アーリーストッピング
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*5      # or stop search after 5 min.
)


# グリッドサーチ
# --- DRF専用の関数ではない
# --- デカルトグリッドとランダムグリッドの両方をサポート
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors, 
  y = response, 
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = n_features * 10,
  seed = 123,
  stopping_metric = "RMSE",   
  stopping_rounds = 10,           # stop if last 10 trees added 
  stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)



# 4.結果の確認 ----------------------------------------------

# グリッドサーチの結果取得
random_grid_perf <- h2o.getGrid(
  grid_id = "rf_random_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)

# 確認
result <- random_grid_perf@summary_table %>% as.data.frame()
result %>% print()
result %>% nrow()




