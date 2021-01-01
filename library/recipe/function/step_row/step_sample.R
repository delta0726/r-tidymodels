# Title     : step_sample
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/25
# URL       : https://recipes.tidymodels.org/reference/step_sample.html


# ＜ポイント＞
# - size引数の値が0-1の場合はdplyr::sample_frac()が適用される
# - size引数の値が1以上の場合はdplyr::sample_n()が適用される


# ＜構文＞
# step_sample(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  size = NULL,
#  replace = FALSE,
#  skip = TRUE,
#  id = rand_id("sample")
#)



# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(recipes)



# 2 基本的なサンプリング --------------------------------------------

# sample_n
recipe( ~ ., data = iris) %>%
  step_sample(size = 1) %>%
  prep(training = iris) %>%
  juice() %>%
  nrow()#> [1] 1



# sample_frac
recipe( ~ ., data = iris) %>%
  step_sample(size = 0.9999) %>%
  prep(training = iris) %>%
  juice() %>%
  nrow()#> [1] 150




# 2 基本的なサンプリング --------------------------------------------

# Uses `sample_n` and returns _at maximum_ 120 samples.
smaller_iris <-
  recipe( ~ ., data = iris) %>%
  step_sample() %>%
  prep(training = iris %>% slice(1:120))


juice(smaller_iris) %>% nrow()

