# ******************************************************************************
# Title     : step_lencode_bayes
# Objective : TODO
# Created by: Owner
# Created on: 2020/11/25
# URL       : https://embed.tidymodels.org/reference/step_lencode_bayes.html
# ******************************************************************************


# ＜ポイント＞
# - ベイズ一般化線形モデル用いてカテゴリ変数の数値変換を行う



# ＜構文＞
# step_lencode_bayes(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  outcome = NULL,
#  options = list(seed = sample.int(10^5, 1)),
#  verbose = FALSE,
#  mapping = NULL,
#  skip = FALSE,
#  id = rand_id("lencode_bayes")
#)



# ＜ポイント＞
# 0 アルゴリズム
# 1 準備
# 2 レシピ適用
# 3 データ確認


# 0 アルゴリズム -------------------------------------------------

# ＜ポイント＞
# - 係数は切片なしモデルを使用して作成される

# stan_glmer(outcome ~ (1 | predictor), data = data, ...)



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

# 対象データの確認
okc %>% select(Class, age, location)
okc %>% select(Class, age, location) %>% glimpse()


# レシピ作成
# --- 速度が遅いのでverboseをTRUEにしている
rec <-
  recipe(Class ~ age + location, data = okc) %>%
    step_lencode_bayes(location, outcome = vars(Class), verbose = TRUE) %>%
    prep()



# 3 データ確認 --------------------------------------------------------

# データ確認
rec %>% juice()


# レシピ概要
rec %>% tidy(number = 1)