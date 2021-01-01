# Title     : step_shuffle
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/25
# URL       : https://recipes.tidymodels.org/reference/step_shuffle.html


# ＜ポイント＞
# - 指定した列をランダムにシャッフルする
#   ---


# ＜構文＞
# step_shuffle(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  columns = NULL,
#  skip = FALSE,
#  id = rand_id("shuffle")
#)





# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(recipes)
library(modeldata)



# 確認
integers <- data.frame(A = 1:10, B = 11:20, C = 21:30)
integers %>% print()



# 2.特定列のシャッフル -----------------------------------------------

# レシピ作成
rec <-
  recipe(~ A + B + C, data = integers) %>%
    step_shuffle(A, B) %>%
    prep(training = integers)


# レシピ適用
set.seed(5377)
rec %>% bake(integers)



rec %>% tidy()
rec %>% tidy(number = 1)



# 3.全体のシャッフル -----------------------------------------------

# レシピ作成
# --- 全ての説明変数
rec <-
  recipe(~ A + B + C, data = integers) %>%
    step_shuffle(all_predictors()) %>%
    prep(training = integers)


# レシピ適用
set.seed(5377)
rec %>% bake(integers)


# レシピ確認
rec %>% tidy()
rec %>% tidy(number = 1)
