# Title     : step_filter
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/25
# URL       : https://recipes.tidymodels.org/reference/step_filter.html


# ＜ポイント＞
# - dplyr::filter()を使った列抽出を行う



# ＜構文＞
# step_filter(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  inputs = NULL,
#  skip = TRUE,
#  id = rand_id("filter")
#)





# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(recipes)
library(modeldata)



# 2 並び替え --------------------------------------------

# ***** recipes ***********************

# レシピ作成
# --- 並び替え
rec <-
  recipe( ~ ., data = iris) %>%
    step_filter(Sepal.Length > 4.5, Species == "setosa") %>%
    prep(training = iris %>% slice(1:75))


# レシピ確認
prepped %>% tidy(number = 1)


# データ確認
rec_train <- rec %>% juice()
rec_train %>% print()




# ***** dplyr ***********************

# 並び替え
# --- 訓練データ
dplyr_train <-
  iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")



# データ確認
dplyr_train %>% print()


# 比較
# --- 完全一致
all.equal(dplyr_train, rec_train)





# 3 並び替えの列名をベクトルで与える場合 --------------------------------

# ＜ポイント＞
# - 列をベクトルで与える際には｢!!!｣を付ける


# 並び替え対象となる列
values <- c("versicolor", "virginica")


# レシピ作成
qq_rec <-
  recipe( ~ ., data = iris) %>%
    step_filter(Sepal.Length > 4.5, Species  %in% !!values) %>%
    prep()


# レシピ確認
qq_rec %>% tidy()
qq_rec %>% tidy(number = 1)


# データ確認
qq_rec %>% juice()