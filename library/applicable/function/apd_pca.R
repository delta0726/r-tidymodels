# Title     : apd_pca
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/11
# URL       : https://applicable.tidymodels.org/reference/apd_pca.html




# ＜ポイント＞
# - 指定した閾値に占める主成分を計算します
# - 主成分の絶対値のパーセンタイルや各主成分の平均を計算する


# ＜構文＞
# apd_pca(x, threshold = 0.95, ...)


predictors <- mtcars[, -1]

# Data frame interface
mod <- predictors %>% apd_pca()

# Formula interface
mod2 <- apd_pca(mpg ~ ., mtcars)

# Recipes interface
library(recipes)
rec <- recipe(mpg ~ ., mtcars)
rec <- step_log(rec, disp)
mod3 <- apd_pca(rec, mtcars)