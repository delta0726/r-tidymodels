# Title     : step_dummy
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/06
# URL       : https://recipes.tidymodels.org/reference/step_dummy.html
#           : https://recipes.tidymodels.org/articles/Dummies.html



# ＜ポイント＞
# - カテゴリカルデータをダミー変数に変換する


library(tidyverse)
library(lubridate)
library(recipes)


# データロード
data(okc)
okc <- okc[complete.cases(okc),]


# データ確認
okc %>% as_tibble()
okc %>% glimpse()


# レシピ作成
rec <-
  recipe(~ diet + age + height, data = okc) %>%
    step_dummy(diet)


# レシピ確認
# --- prep()する前はダミー変数の列は表示されない
rec %>% summary()


# レシピ完成
dummies <- rec %>% prep(training = okc)
dummies %>% print()


# レシピ確認
# --- ダミー変数列が追加されている
dummies %>% summary()


# レシピ適用
dummy_data <- dummies %>% bake(new_data = okc)
dummy_data %>% print()
dummy_data %>% glimpse()

