# Title     : fit
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/10
# URL       : https://workflows.tidymodels.org/reference/fit-workflow.html


# ＜ポイント＞
# - ワークフローに基づいて学習(フィッティング)を行う


# ＜構文＞
# fit(object, data, ..., control = control_workflow())



library(parsnip)
library(recipes)



#%% フォーミュラによる定義 ------------------------------------------------

# モデル定義
model <-
  linear_reg() %>%
    set_engine("lm")


# ワークフロー定義
wf <-
  workflow() %>%
    add_model(model) %>%
    add_formula(mpg ~ cyl + log(disp))


# 学習
wf %>% fit(mtcars)



#%% レシピによる定義 ------------------------------------------------

# レシピ定義
rec <-
  recipe(mpg ~ cyl + disp, mtcars) %>%
    step_log(recipe, disp)


# ワークフロー定義
wf_rec <-
    workflow() %>%
    add_model(model) %>%
    add_recipe(rec)


# 学習
wf_rec %>% fit(mtcars)

