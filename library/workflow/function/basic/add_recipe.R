# Title     : add_recipe / remove_recipe / update_recipe
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/10
# URL       : https://workflows.tidymodels.org/reference/add_recipe.html


# ＜ポイント＞
# - ワークフローにRecipeオブジェクトを追加する
#   --- prep()
# - ワークフローには｢フォーミュラ｣か｢レシピ｣のいずれかが追加できる


# ＜構文＞
# add_recipe(x, recipe, ..., blueprint = NULL)
# remove_recipe(x)
# update_recipe(x, recipe, ..., blueprint = NULL)



library(tidyverse)
library(tidymodels)



#%% レシピ準備 -----------------------------------------------

# レシピ定義
# --- prep()は不要： juice()を使うわけではないので
recipe <-
  recipe(mpg ~ cyl, mtcars) %>%
    step_log(cyl)

# 確認
recipe %>% print()
recipe %>% class()
recipe %>% prep() %>% class()



#%% ワークフローにレシピ追加 -----------------------------------------------

# ワークフロー設定
# --- レシピ追加
workflow <-
  workflow() %>%
    add_recipe(recipe)

# 確認
# --- PreprocessorにRecipeが追加される
workflow %>% print()


#%% ワークフローからレシピ削除/更新 -----------------------------------------------

# フォーミュラ削除
# --- PreprocessorからRecipeが削除される
workflow %>% remove_recipe()


# レシピ更新
# --- step_*を削除
workflow %>% update_recipe(recipe(mpg ~ cyl, mtcars))



#%% 実験：prepしたrecipeを追加 -----------------------------------------------

# レシピをprep()
recipe_prep <- recipe %>% prep()


# 確認
# --- prep()をすることで"template"が更新され、"tr_info""orig_lvls""last_term_info"が追加される
recipe %>% names()
recipe_prep %>% names()


# ワークフローを設定
# --- 一応エラーは出ない
workflow <-
  workflow() %>%
    add_recipe(recipe_prep)
