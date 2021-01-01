# Title     : step_rename
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/26
# URL       : https://recipes.tidymodels.org/reference/step_rename.html



# ＜ポイント＞
# - dplyr::rename()を使って列名変更を行う



# ＜構文＞
# step_rename(
#  recipe,
#  ...,
#  role = "predictor",
#  trained = FALSE,
#  inputs = NULL,
#  skip = FALSE,
#  id = rand_id("rename")
#)




# 1. 特定列の名前を変更 -------------------------------------------

# レシピ作成
rec <-
  recipe( ~ ., data = iris) %>%
    step_rename(Sepal_Width = Sepal.Width)


# 列名確認
iris %>% names()
rec %>% prep() %>% juice() %>% names()




# 2. 列名をベクトルで渡す -------------------------------------------

# 列名リスト
vars <- c(var1 = "cyl", var2 = "am")


# レシピ作成
car_rec <-
  recipe(~ ., data = mtcars) %>%
  step_rename(!!vars)


# 列名確認
mtcars %>% names()
car_rec %>% prep() %>% juice() %>% names()

