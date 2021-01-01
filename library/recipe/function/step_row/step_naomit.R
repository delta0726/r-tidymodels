# Title     : step_naomit
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/25
# URL       : https://recipes.tidymodels.org/reference/step_naomit.html


# ＜ポイント＞
# - NAまたはNaNの値が含まれている場合に行を削除する



# ＜構文＞
# step_naomit(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  columns = NULL,
#  skip = FALSE,
#  id = rand_id("naomit")
#)



# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(recipes)
library(modeldata)


# データ確認
# --- 153   6
airquality %>% as_tibble()
airquality %>% dim()


# NA数の確認
airquality %>% is.na() %>% apply(2, sum)




# 2 NAを含む行を削除 --------------------------------------------

# レシピ作成
rec <-
  recipe(Ozone ~ ., data = airquality) %>%
    step_naomit(Solar.R) %>%
    prep(airquality, verbose = FALSE)



# データ確認
# --- 45   6
rec %>% juice()
rec %>% juice() %>% dim()


# NA数の確認
rec %>% juice() %>% is.na() %>% apply(2, sum)

