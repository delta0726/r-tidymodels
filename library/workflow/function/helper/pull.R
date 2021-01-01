# Title     : pull_*
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/10
# URL       : https://workflows.tidymodels.org/reference/workflow-extractors.html


# ＜ポイント＞
# - ワークフローの学習済モデルに基づいて予測データを作成する


# ＜構文＞
# pull_workflow_preprocessor(x)
# pull_workflow_spec(x)
# pull_workflow_fit(x)
# pull_workflow_mold(x)
# pull_workflow_prepped_recipe(x)


library(parsnip)
library(recipes)
library(Hmisc)


# モデル定義
model <-
  linear_reg() %>%
    set_engine("lm")

# レシピ定義
recipe <-
  recipe(mpg ~ cyl + disp, training) %>%
    step_log(disp)


# ワークフロー定義
wf_formula <- workflow() %>% add_model(model) %>% add_formula(mpg ~ cyl + log(disp))
wf_recipe  <- workflow() %>% add_model(model) %>% add_recipe(recipe)


# 学習
fit_formula <- wf_formula %>% fit(mtcars)
fit_recipe  <- wf_recipe  %>% fit(mtcars)


# プロセッサの出力
# --- Preprocessor: Formula or Recipe
wf_formula %>% pull_workflow_preprocessor()
wf_recipe  %>% pull_workflow_preprocessor()


# parsnipモデルを出力
wf_formula %>% pull_workflow_spec()
wf_recipe  %>% pull_workflow_spec()


# モデル推定結果を出力
fit_formula %>% pull_workflow_fit()
fit_recipe  %>% pull_workflow_fit()


# 学習結果を出力
fit_formula %>% pull_workflow_mold()
fit_recipe  %>% pull_workflow_mold()


# 完成後のレシピの出力
fit_recipe  %>% pull_workflow_prepped_recipe()