# Title     : step_arrange
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/25
# URL       : https://recipes.tidymodels.org/reference/step_arrange.html


# ＜ポイント＞
# - dplyr::arrange()を使った並び替えを行う
# - 機械学習アルゴリズムで並び替えが必要なケースは多くない
#   --- 時系列分析の場合に使うことが想定される


# ＜構文＞
# step_arrange(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   inputs = NULL,
#   skip = FALSE,
#   id = rand_id("arrange")
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
# --- 並び替え
prepped <-
  recipe( ~ ., data = iris) %>%
    step_arrange(desc(Sepal.Length), 1/Petal.Length) %>%
    prep(training = iris %>% slice(1:75))


# レシピ確認
prepped %>% tidy(number = 1)


# データ確認
rec_train <- prepped %>% juice()
rec_train %>% print()


# テストデータ
rec_test <- prepped %>% bake(iris %>% slice(76:150))


# ***** dplyr ***********************

# 並び替え
# --- 訓練データ
dplyr_train <-
  iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    arrange(desc(Sepal.Length), 1/Petal.Length)


# 並び替え
# --- テストデータ
dplyr_test <-
  iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    arrange(desc(Sepal.Length), 1/Petal.Length)


# データ確認
dplyr_train %>% print()
dplyr_test %>% print()


# 比較
# --- 完全一致
all.equal(dplyr_train, rec_train)
all.equal(dplyr_test, rec_test)




# 3 並び替えの列名をベクトルで与える場合 --------------------------------

# ＜ポイント＞
# - 列をベクトルで与える際には｢!!!｣を付ける


# 並び替え対象となる列
sort_vars <- c("Sepal.Length", "Petal.Length")


# レシピ作成
qq_rec <-
  recipe( ~ ., data = iris) %>%
    step_arrange(!!!syms(sort_vars)) %>%
    prep(training = iris)


# レシピ確認
qq_rec %>% tidy()
qq_rec %>% tidy(number = 1)


# データ確認
qq_rec %>% juice()