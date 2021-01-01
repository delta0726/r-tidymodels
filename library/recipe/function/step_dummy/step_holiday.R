# Title     : step_holiday
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/06
# URL       : https://recipes.tidymodels.org/reference/step_holiday.html


# ＜ポイント＞
# - 日付データから休日を判定してダミーフラグを立てる
# - 休日をダミー変数として扱えるようになる
# - timeDate::listHolidays()でカレンダー情報を入手することができる



library(tidyverse)
library(lubridate)
library(recipes)


# 日付データの作成
examples <- data.frame(someday = ymd("2000-12-20") + days(0:40))
examples %>% print()


# レシピ作成
holiday_rec <-
  recipe(~ someday, data = examples) %>%
   step_holiday(someday)


# レシピ確認
holiday_rec %>% summary()


# レシピ完成
holiday_rec <- holiday_rec %>% prep(training = examples)


# レシピ適用
holiday_values <- holiday_rec %>% bake(new_data = examples)


# データ確認
# --- 日付が要素に分解されてダミー変数のように扱えるようになっている
holiday_values %>% as.data.frame()
