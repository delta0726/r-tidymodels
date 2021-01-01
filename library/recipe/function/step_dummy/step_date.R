# Title     : step_date
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/06
# URL       : https://recipes.tidymodels.org/reference/step_date.html


# ＜ポイント＞
# - 日付データは本来は一意のデータ
# - 日付を要素に分解することでダミー変数のように扱えるようになる



library(tidyverse)
library(lubridate)
library(recipes)


# 日付データの作成
examples <-
  data.frame(Dan = ymd("2002-03-04") + days(1:10),
             Stefan = ymd("2006-01-13") + days(1:10))


# レシピ作成
date_rec <-
  recipe(~ Dan + Stefan, data = examples) %>%
     step_date(all_predictors())


# レシピ確認
date_rec %>% summary()


# 日付要素への分解
# --- yeaa
# --- month
# --- dow(曜日を1-7の数値で表現)
date_rec %>% tidy(number = 1)


# レシピ完成
date_rec <- date_rec %>% prep(training = examples)


# レシピ適用
date_values <- date_rec %>% bake(new_data = examples)


# データ確認
# --- 日付が要素に分解されてダミー変数のように扱えるようになっている
date_values %>% print()
