# Title     : step_unknown
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/27
# URL       : https://recipes.tidymodels.org/reference/step_unknown.html


# ＜ポイント＞
# - 因子レベルの欠損値を「unknown」に割り当てるレシピステップの仕様を作成します。


# ＜構文＞
# step_unknown(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  new_level = "unknown",
#  objects = NULL,
#  skip = FALSE,
#  id = rand_id("unknown")
#)





# 1 準備 ----------------------------------------------------

library(tidyverse)
library(tidymodels)
library(modeldata)
library(skimr)
library(gridExtra)


# データロード
data("okc")


# データ確認
okc %>% glimpse()
okc %>% skim()




# 2 ファクターの代入 ----------------------------------------------------

# レシピ
# --- diet    : 文字列（欠損値あり）
# --- location: 文字列（欠損値なし）
rec <-
  recipe(~ diet + location, data = okc) %>%
  step_unknown(diet, new_level = "unknown diet") %>%
  step_unknown(location, new_level = "unknown location") %>%
  prep()


# レシピ確認
rec %>% tidy()
rec %>% tidy(number = 1)



table(juice(rec)$diet, okc$diet, useNA = "always") %>%
  as.data.frame() %>%
  dplyr::filter(Freq > 0)


