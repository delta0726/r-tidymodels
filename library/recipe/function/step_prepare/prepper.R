# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/03

library(magrittr)
library(tidyverse)
library(tidymodels)
library(caret)


# データロード
imbal_data <- twoClassSim(1000, intercept = 10)


# データ概要
imbal_data %>% glimpse()


# レシピ作成
# --- この時点でprep()はしない
imbal_rec <-
  recipe(Class ~ ., data = imbal_data) %>%
  step_downsample(Class)


# データのリサンプリング
# ---10パターンのデータを生成
set.seed(5732)
cv_folds <- imbal_data %>% vfold_cv(strata = "Class", v = 10, repeats = 5)
cv_folds %>% print()


# レシピ適用
# --- prepperでSplitデータのそれぞれにレシピを適用
cv_folds <-
  cv_folds %>%
  mutate(recipes = map(splits, prepper, recipe = imbal_rec))


# 確認
cv_folds %>% print()
cv_folds$recipes[[1]] %>% print()

