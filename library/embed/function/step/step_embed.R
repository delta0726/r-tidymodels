# ******************************************************************************
# Title     : step_embed
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/30
# URL       : https://embed.tidymodels.org/reference/step_embed.html
# ******************************************************************************


# ＜ポイント＞



# ＜構文＞
# step_embed(
#  recipe,
#  ...,
#  role = "predictor",
#  trained = FALSE,
#  outcome = NULL,
#  predictors = NULL,
#  num_terms = 2,
#  hidden_units = 0,
#  options = embed_control(),
#  mapping = NULL,
#  history = NULL,
#  skip = FALSE,
#  id = rand_id("lencode_bayes")
#)

# embed_control(
#   loss = "mse",
#   metrics = NULL,
#   optimizer = "sgd",
#   epochs = 20,
#   validation_split = 0,
#   batch_size = 32,
#   verbose = 0,
#   callbacks = NULL
# )



# ＜目次＞
# 0 アルゴリズム
# 1 準備
# 2 データ準備
# 3 レシピ適用



# 0 アルゴリズム -------------------------------------------------

# ＜プロセス＞
# - 因子レベルは最初に新しい変数にランダムに割り当てられる
# - 各変数はNNで使用され、新しい列へのレベルの割当と、結果を予測モデルの推定の両方を最適化する


# ＜ポイント＞
# - step_embedの呼び出しごとに1つのモデルが作成される
# - ステップに与えられたすべての項は、同じモデルで推定およびエンコードされる
# - Tensorflowの性質上、このステップを使用して再現性のある結果を得るのは難しい
# - tensorflowモデルは、foreachなどによる並列処理を適用できない点に注意


# ＜ラベルが数値の場合＞
# 線形活性化関数が最後の層で使用され、softmaxが因子の結果（任意の数のレベル）に使用されます

#keras_model_sequential() %>%
#  layer_embedding(
#    input_dim = num_factor_levels_x + 1,
#    output_dim = num_terms,
#    input_length = 1
#  ) %>%
#  layer_flatten() %>%
#  layer_dense(units = 1, activation = 'linear')


# ＜ラベルがカテゴリカルの場合＞

#keras_model_sequential() %>%
#  layer_embedding(
#    input_dim = num_factor_levels_x + 1,
#    output_dim = num_terms,
#    input_length = 1
#   ) %>%
#  layer_flatten() %>%
#  layer_dense(units = hidden_units, activation = "relu") %>%
#  layer_dense(units = num_factor_levels_y, activation = 'softmax')



# 1 準備 ---------------------------------------------------------

library(reticulate)
library(tidyverse)
library(tidymodels)
library(embed)
library(modeldata)
library(keras)
library(tensorflow)


# 仮想環境の選択
use_condaenv("C:/Users/Owner/Anaconda3/envs/r-reticulate", required = TRUE)
py_config()



# 2 データ準備 ------------------------------------------------------

# データロード
data(okc)


# データ確認
okc %>% print()
okc %>% glimpse()


# データ分割
okc_split <- okc %>% initial_split()
okc_tr <- okc_split %>% training()
okc_te <- okc_split %>% testing()



# 3 レシピ適用 ------------------------------------------------------

# レシピ作成
rec <-
  recipe(Class ~ age + location, data = okc_tr) %>%
    step_embed(location, outcome = vars(Class),
               options = embed_control(epochs = 10)) %>%
    prep()


# データ確認
rec %>% juice()


# 元データの確認
# --- ラベル：カテゴリカルデータ
# --- 特徴量：数値/カテゴリカルデータ
okc_tr %>% select(Class, age, location)



# レシピ適用
rec %>% bake(okc_te)


# レシピ詳細
rec %>% tidy()

