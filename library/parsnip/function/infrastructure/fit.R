# Title     : fit / fit_xy
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/12
# URL       : https://parsnip.tidymodels.org/reference/fit.html



# ＜ポイント＞
# - モデルの学習を行うための関数
# - {parsnip}のみを使って学習する場合はfit()又はfit_xy()でフォーミュラを定義する
# - fit()はフォーミュラ方式でモデルを定義する
# - fit_xy()は変数方式でモデルを定義する


# ＜構文＞
# S3 method for model_spec
# fit(object, formula, data, control = control_parsnip(), ...)
#
# S3 method for model_spec
# fit_xy(object, x, y, control = control_parsnip(), ...)
#


library(tidyverse)
library(parsnip)
library(modeldata)


# データロード
data("lending_club")


# データ確認
lending_club %>% print()
lending_club %>% names()
lending_club %>% glimpse()


# フォーミュラ方式
using_formula <-
  logistic_reg() %>%
    set_engine("glm") %>%
    fit(Class ~ funded_amnt + int_rate, data = lending_club)


# 変数方式
using_xy <-
  logistic_reg() %>%
    set_engine("glm") %>%
    fit_xy(x = lending_club[, c("funded_amnt", "int_rate")],
           y = lending_club$Class)


# 確認
using_formula %>% print()
using_xy %>% print()
