# ******************************************************************************
# Title     : step_lencode_glm
# Objective : TODO
# Created by: Owner
# Created on: 2020/11/26
# URL       : https://embed.tidymodels.org/reference/step_lencode_glm.html
# ******************************************************************************


# ＜ポイント＞
# - カテゴリカルデータの特徴量を一般化線形モデル用いて数値変換する


# ＜構文＞
# step_lencode_glm(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  outcome = NULL,
#  mapping = NULL,
#  skip = FALSE,
#  id = rand_id("lencode_glm")
#)



# ＜ポイント＞
# 0 アルゴリズム
# 1 準備
# 2 レシピ適用
# 3 データ確認



# 0 アルゴリズム --------------------------------------------------

# ＜ポイント＞
# - ラベル(Y)に対して特徴量(X)で一般化線形モデルを適用してエンコーディングする
# - 係数は切片なしモデルを使用して作成



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



# 2 レシピ適用 ------------------------------------------------------

# 対象列の確認
# --- 数値列
okc %>% select(Class, age, location)
okc %>% select(Class, age, location) %>% glimpse()


# レシピ作成
rec <-
  recipe(Class ~ age + location, data = okc) %>%
    step_lencode_glm(location, outcome = vars(Class)) %>%
    prep()


# レシピ確認
# --- prep()の適用前
rec %>% print()
rec %>% summary() %>% as.data.frame()
rec %>% tidy(number = 1)



# 3 データ確認 --------------------------------------------------------

# レシピ適用
okc_rec <- rec %>% juice()


# データ確認
okc_rec %>% select(Class, age, location)
okc_rec %>% select(Class, age, location) %>% glimpse()



# プロット作成
okc_rec$location %>% hist()