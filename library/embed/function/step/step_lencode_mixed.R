# ******************************************************************************
# Title     : step_lencode_mixed
# Objective : TODO
# Created by: Owner
# Created on: 2020/11/26
# URL       : https://embed.tidymodels.org/reference/step_lencode_mixed.html
# ******************************************************************************


# ＜ポイント＞
# - カテゴリカルデータの特徴量を一般化線形混合モデル用いて数値変換する


# ＜構文＞
# step_lencode_mixed(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  outcome = NULL,
#  options = list(verbose = 0),
#  mapping = NULL,
#  skip = FALSE,
#  id = rand_id("lencode_mixed")
#)



# ＜目次＞
# 0 アルゴリズム
# 1 準備
# 2 レシピ適用
# 3 データ確認


# 0 アルゴリズム --------------------------------------------------

# ＜ポイント＞
# - 係数は切片なしモデルを使用して作成

# lmer(outcome ~ 1 + (1 | predictor), data = data, ...)



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
okc %>% select(Class, age, location)
okc %>% select(Class, age, location) %>% glimpse()


# レシピ作成
rec <-
  recipe(Class ~ age + location, data = okc) %>%
    step_lencode_mixed(location, outcome = vars(Class))


# レシピ確認
# --- prep()の適用前
rec %>% print()


# レシピ完成
rec <- rec %>% prep(training = okc)
rec %>% print()
rec %>% summary() %>% as.data.frame()
rec %>% tidy(number = 1)


# 3 データ確認 --------------------------------------------------------

# レシピ適用
rec_baked <- rec %>% juice()


# データ確認
rec_baked %>% select(Class, age, location)
rec_baked %>% select(Class, age, location) %>% glimpse()


# レシピ詳細情報
rec %>% tidy(number = 1)


X <-
  okc %>%
    select(Class, age, location) %>%
    bind_cols(rec_baked)