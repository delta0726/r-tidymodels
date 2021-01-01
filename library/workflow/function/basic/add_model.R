# Title     : add_model
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/10
# URL       : https://workflows.tidymodels.org/reference/add_model.html


# ＜ポイント＞
# - ワークフローにモデルを追加する
# - モデルはparsnipで定義する（式はフォーミュラかレシピで定義）


# ＜構文＞
# add_model(x, spec, formula = NULL)
# remove_model(x)
# update_model(x, spec, formula = NULL)



library(tidyverse)
library(tidymodels)



# モデル定義
lm_model <-
  linear_reg() %>%
    set_engine("lm")

regularized_model <-
  linear_reg() %>%
    set_engine("glmnet")


# ワークフロー設定
workflow <-
  workflow() %>%
    add_formula(mpg ~ .) %>%
    add_model(lm_model)


# 確認
# # --- Computational engine: lm
workflow %>% print()


# モデル削除
workflow %>%
  remove_model()


# モデル更新
# --- Computational engine: glmnet
workflow %>%
  update_model(regularized_model)