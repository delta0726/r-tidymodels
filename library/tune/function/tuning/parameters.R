# Title     : parameters
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/08
# URL       : https://tune.tidymodels.org/reference/parameters.workflow.html



# ＜ポイント＞
# - tune()を指定したパラメータ一覧が表示される
# - 汎用のdials::parameter()を{tune}にも移植したもの
# - ｢recipe｣｢model_spec｣｢workflow｣の各オブジェクトからパラメータを抽出する
# - チューニングは｢モデリング｣と｢レシピ｣で行われる



library(tidyverse)
library(tidymodels)



#%% レシピからパラメータを取得 -------------------------------

# tune()でid指定していない場合
# --- 引数(ハイパーパラメータ)が表示される
recipe(mpg ~ ., data = mtcars) %>%
  step_knnimpute(all_predictors(), neighbors = tune()) %>%
  step_pca(all_predictors(), num_comp = tune()) %>%
  parameters()


# tune()でid指定している場合
# --- 指定した名前が表示される
recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = tune("disp df")) %>%
  step_ns(wt, deg_free = tune("wt df")) %>%
  parameters()


# パラメータがない場合
# --- 0行のtibble
# --- エラーにはならない
recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  parameters()#> Collection of 0 parameters for tuning



#%% モデルからパラメータを取得 -------------------------------

# 例1
boost_tree(trees = tune(), min_n = tune()) %>%
  set_engine("xgboost") %>%
  parameters()


# 例2
boost_tree(trees = tune(), min_n = tune()) %>%
  set_engine("C5.0", rules = TRUE) %>%
  parameters()



#%% ワークフローからパラメータを取得 -------------------------------

# モデリング
# --- チューニングあり(mtry, min_n)
rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
    set_engine("ranger") %>%
    set_mode("classification")


# レシピ設定
rf_recipe <-
  recipe(Species ~ ., data = iris) %>%
    step_normalize(all_predictors())


# ワークフロー設定
rf_workflow <-
  workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(rf_recipe)


# パラメータ取得
# --- ワークフローから取得
rf_workflow %>% parameters()


# パラメータ取得
# --- mドエルから取得
rf_mod %>% parameters()

