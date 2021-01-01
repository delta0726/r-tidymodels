# Title     : extract_recipe / extract_model
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/16
# URL       : https://tune.tidymodels.org/reference/extract_recipe.html


# ＜ポイント＞
# - 要確認、なぜか機能しない




library(tidyverse)
library(tidymodels)


# レシピ定義
rec <-
  recipe(mpg ~ cyl + disp, mtcars) %>%
    step_log(recipe, disp)


# モデル定義
model <-
  linear_reg() %>%
  set_engine("lm")


# ワークフロー定義
wf_rec <-
  workflow() %>%
    add_model(model) %>%
    add_recipe(rec)


# 確認
wf_rec %>% print()


# 学習
fitted <- wf_rec %>% fit(mtcars)
fitted %>% print()
fitted %>% class()
fitted$trained


# レシピの抽出
fitted %>% extract_recipe()


# モデルの抽出
fitted %>% extract_model()
