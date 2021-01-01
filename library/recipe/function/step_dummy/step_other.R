# Title     : step_other
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/12
# URL       : https://recipes.tidymodels.org/reference/step_other.html


# ＜ポイント＞
# - 特定の値を"Other"



#
library(tidyverse)
library(parsnip)
library(modeldata)


# データロード
data(okc)


# データ確認
okc %>% print()
okc %>% names()
okc %>% glimpse()


# データ分割
set.seed(19)
in_train <- 1:nrow(okc) %>% sample(size = 30000)
okc_tr <- okc[ in_train,]
okc_te <- okc[-in_train,]


# レシピ作成
rec <-
  recipe(~ diet + location, data = okc_tr) %>%
    step_other(diet, location, threshold = .1, other = "other values") %>%
    prep(training = okc_tr)


# レシピ適用
# --- テストデータ
collapsed <- rec %>% bake(okc_te)
collapsed %>% print()


# 集計
table(okc_te$diet, collapsed$diet, useNA = "always")


# レシピ確認
rec %>% tidy(number = 1)


tahiti <- okc[1,]
tahiti$location <- "a magical place"
bake(rec, tahiti)#> # A tibble: 1 x 2
#>   diet              location
#>   <fct>             <fct>
#> 1 strictly anything other values
# threshold as a frequency
rec <- recipe(~ diet + location, data = okc_tr)

rec <- rec %>%
  step_other(diet, location, threshold = 2000, other = "other values")
rec <- prep(rec, training = okc_tr)

tidy(rec, number = 1)#> # A tibble: 6 x 3
#>   terms    retained          id
#>   <chr>    <chr>             <chr>
#> 1 diet     anything          other_p2QWY
#> 2 diet     mostly anything   other_p2QWY
#> 3 diet     strictly anything other_p2QWY
#> 4 location berkeley          other_p2QWY
#> 5 location oakland           other_p2QWY
#> 6 location san francisco     other_p2QWY# compare it to
# okc_tr %>% count(diet, sort = TRUE) %>% top_n(4)
# okc_tr %>% count(location, sort = TRUE) %>% top_n(3)
