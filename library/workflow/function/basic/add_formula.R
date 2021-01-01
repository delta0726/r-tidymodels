# Title     : add_formula
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/10
# URL       : https://workflows.tidymodels.org/reference/add_formula.html


# ＜ポイント＞
# - ワークフローにフォーミュラを追加する
# - ワークフローには｢フォーミュラ｣か｢レシピ｣のいずれかが追加できる


# ＜構文＞
# add_formula(x, formula, ..., blueprint = NULL)
# remove_formula(x)
# update_formula(x, formula, ..., blueprint = NULL)



library(tidyverse)
library(tidymodels)


# ワークフロー設定
# --- フォーミュラ追加
workflow <-
  workflow() %>%
    add_formula(mpg ~ cyl)


# 確認
workflow %>% print()


# フォーミュラ削除
workflow %>%
  remove_formula()


# フォーミュラ更新
workflow %>%
  update_formula(mpg ~ disp)

