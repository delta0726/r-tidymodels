# Title     : predict
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/27
# URL       : https://workflows.tidymodels.org/reference/predict-workflow.html


# ＜ポイント＞
# - ワークフローの学習済モデルに基づいて予測データを作成する
# - モードに応じてtype引数で出力データを選択することができる
# - ワークフローで前処理を登録しているので、関数にデータを渡す際は常に前処理前のデータを渡す



# ＜構文＞
# predict(object, new_data, type = NULL, opts = list(), ...)
#
# - new_data: 予測子を含むデータフレーム(Recipesによる前処理前)
# - type    : "numeric", "class", "prob", "conf_int", "pred_int",
#           : "quantile", or "raw"
# - opts    : type="raw"の場合に使用される引数リスト


# ＜type＞
# - 回帰問題 : "numeric", "conf_int", "pred_int"
# - 分類問題 : "class", "prob"
# - 生存問題 : "quantile"


# ＜参考＞
# - 信頼区間と予測区間を混同しちゃダメ
#  http://takehiko-i-hayashi.hatenablog.com/entry/20110204/1296773267



# 1 準備 --------------------------------------------------------------

library(tidyverse)
library(tidymodels)



# ****** 回帰問題 *****************************

# データ確認
mtcars %>% as_tibble()
mtcars %>% glimpse()


# データ分割
training <- mtcars[1:20,]
testing <- mtcars[21:32,]


# 確認
training %>% dim()
testing %>% dim()



# ****** 分類問題 *****************************

# データロード
data("bivariate")


# 確認
bivariate_train %>% as_tibble
bivariate_test %>% as_tibble



# 2 ワークフロー定義 ------------------------------------------------

# モデルに使用するデータ
# --- dispを対数変換
training %>% select(mpg, cyl, disp)
training %>% ggplot(aes(x = disp)) + geom_histogram()
training %>% ggplot(aes(x = log(disp))) + geom_histogram()


# モデル定義
model <-
linear_reg() %>%
    set_engine("lm")


# レシピ定義
recipe <-
  recipe(mpg ~ cyl + disp, training) %>%
    step_log(disp)


# ワークフロー定義
workflow <-
  workflow() %>%
    add_model(model) %>%
    add_recipe(recipe)


# 確認
workflow %>% print()



# 3 学習 ------------------------------------------------

# 学習
fit_workflow <-
  workflow %>%
    fit(training)



# 4 予測: regression ---------------------------------------

# 予測
fit_workflow %>% predict(new_data = testing)


# type引数の変更
fit_workflow %>% predict(new_data = testing, type = "numeric")
fit_workflow %>% predict(new_data = testing, type = "conf_int")
fit_workflow %>% predict(new_data = testing, type = "pred_int")




# 5 予測: classification ------------------------------------

# モデル定義
logit_mod <-
  logistic_reg() %>%
    set_engine("glm")


# ワークフロー定義
# --- レシピなし
glm_workflow <-
  workflow() %>%
  add_model(logit_mod)


# 学習
simple_glm <-
  glm_workflow %>%
  add_formula(Class ~ .) %>%
  fit(data = bivariate_train)


# 予測
simple_glm %>% predict(new_data = bivariate_test, type = "class")
simple_glm %>% predict(new_data = bivariate_test, type = "prob")




# 6 レシピが適用されているか確認 --------------------------------

# ***** workflowの中身を確認 *******************

# ＜ポイント＞
# - 学習時は訓練データにレシピが確認されていることが確認できる



# 予測に使用しているデータ
# --- レシピが適用されている
# --- dispに対数変換
fit_workflow %>% pull_workflow_mold() %>% .$predictors


# 参考：元データ
training %>% select(cyl, disp) %>% mutate(disp_log = log(disp)) %>% as_tibble()




# ***** workflowの中身を確認 *******************

# ＜ポイント＞
# - workflowモデルでpredict()を実行するときは前処理前のデータを渡す
#   --- predict()を適用する前に前処理が自動適用される


# 予測
# --- 訓練データは適用されている
fit_workflow %>% predict(new_data = testing, type = "numeric")




# 準備：parsnipモデルの構築
logit_mod <-
  linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression") %>%
    fit(mpg ~ cyl + disp, data = recipe %>% prep() %>% bake(training))


# 検証1：前処理前のテストデータで予測
# --- 前処理を適用したモデルに、前処理をしていないデータを適用している
# --- 誤った結果
logit_mod %>% predict(new_data = testing)


# 検証2：前処理後のテストデータで予測
# --- 一致
logit_mod %>% predict(new_data = recipe %>% prep() %>% bake(testing))

