# ******************************************************************************
# Title     : step_feature_hash
# Objective : TODO
# Created by: Owner
# Created on: 2020/11/26
# URL       : https://embed.tidymodels.org/reference/step_feature_hash.html
#           : https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html
# ******************************************************************************

# ＜ポイント＞
# - ハッシュ関数を用いてカテゴリ変数の数値変換を行う
# - カテゴリデータの変換の際に、元のカテゴリ数より少ない特徴量でエンコーディングすることが可能
#   --- One-hot Encodingは変換後の特徴量とカテゴリ水準の数が等しい（メモリ問題）
# - 予め特徴量の数を決めておき、ハッシュ関数を用いて水準ごとにフラグを立てる場所を決定
#   --- 1つの特徴量に複数の｢1｣が割り当てられることがある


# ＜構文＞
# step_feature_hash(
#  recipe,
#  ...,
#  role = "predictor",
#  trained = FALSE,
#  num_hash = 2^6,
#  preserve = FALSE,
#  columns = NULL,
#  skip = FALSE,
#  id = rand_id("feature_hash")
#)


# ＜注意＞
# - {tensorflow}が必要



# ＜目次＞
# 1 準備
# 2 レシピ適用
# 3 結果確認


# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(embed)
library(modeldata)


# データ準備
data(okc)


# データ確認
okc %>% as_tibble()
okc %>% glimpse()



# 2 レシピ適用 ---------------------------------------------------------

# 元データの確認
# --- ラベル：カテゴリカルデータ
# --- 特徴量：数値/カテゴリカルデータ
okc %>% select(Class, age, location)


# レシピ作成
rec <-
  recipe(Class ~ age + location, data = okc) %>%
  step_feature_hash(location, num_hash = 2^6, preserve = TRUE) %>%
  prep()


# How many of the 135 locations ended up in each hash column?
results <- rec %>% juice()




# 3 結果確認 ---------------------------------------------------------

# データ確認
results %>% glimpse()
2^6


results <-
  rec %>%
    juice(starts_with("location")) %>%
    distinct()


results %>%
  select(contains("hash")) %>%
  apply( 2, sum)
