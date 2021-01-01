# Title     : step_slice()
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/25
# URL       : https://recipes.tidymodels.org/reference/step_slice.html



# ＜ポイント＞
# - dplyr::slice()を使った行番号によるレコード抽出を行う
# - 機械学習の前処理としては操作頻度は少ないと思われる


# ＜構文＞
# step_slice(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   inputs = NULL,
#   skip = TRUE,
#   id = rand_id("slice")
# )




# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(recipes)
library(modeldata)



# 2 並び替え --------------------------------------------

# ＜ポイント＞
# - {dplyr}と概ね同様の操作を行うことができる
# - 並び替えの対象となるデータを明示的に指定する必要がある
#   --- prep()やbake()で対象データを指定


# ***** recipes ***********************

# レシピ作成
# --- 行番号で行抽出
prepped <-
  recipe( ~ ., data = iris) %>%
    step_slice(1:3) %>%
    prep(training = iris %>% slice(1:75))


# レシピ確認
prepped %>% tidy()
prepped %>% tidy(number = 1)


# データ確認
rec_train <- prepped %>% juice()
rec_train %>% print()


# テストデータ
rec_test <- prepped %>% bake(iris %>% slice(76:150))
rec_test %>% print()



# ***** dplyr ***********************

# 並び替え
# --- 訓練データ
dplyr_train <-
  iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    slice(1:3)


# 並び替え
# --- テストデータ
dplyr_test <-
  iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    slice(1:3)


# データ確認
dplyr_train %>% print()
dplyr_test %>% print()


# 比較
# --- 完全一致
all.equal(dplyr_train, rec_train)
all.equal(dplyr_test, rec_test)




# 3 並び替えの列名をベクトルで与える場合 --------------------------------

# ＜ポイント＞
# - 列をベクトルで与える際には｢!!｣を付ける


# 抽出する列番号
keep_rows <- 1:6


# レシピ作成
qq_rec <-
  recipe( ~ ., data = iris) %>%
    step_slice(!!keep_rows) %>%
    prep(training = iris)


# レシピ確認
qq_rec %>% tidy()
qq_rec %>% tidy(number = 1)


# データ確認
qq_rec %>% juice()

