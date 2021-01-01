# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/gather.rset.html

library(rsample)



# Check Data
mtcars %>% as_tibble()


# クロスバリデーション
# --- Listed Dataframe
cv_obj <- mtcars %>% vfold_cv(v = 10)
cv_obj %>% print()

# 仮想の統計量を追加
cv_obj$lm_rmse <- rnorm(10, mean = 2)
cv_obj$nnet_rmse <- rnorm(10, mean = 1)
cv_obj %>% print()


# id以降の列をLong型で集計
# --- モデル統計量の集計に便利
cv_obj %>% gather()
