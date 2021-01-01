# Title     : translate
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/translate.html



# ＜ポイント＞
# - モデルエンジンの固有オブジェクトに変換する
# - 汎用パラメータを対応するパラメータに変換する
# - 基本的に内部関数として使用するが、ユーザーが構文の理解を深めるために使ってもよい




lm_spec <- linear_reg(penalty = 0.01)
lm_spec$args$penalty



LM <- lm_spec %>% translate(engine = "lm")



lm_spec %>% translate(engine = "spark")

translate(linear_reg(mixture = varying()), engine = "glmnet")