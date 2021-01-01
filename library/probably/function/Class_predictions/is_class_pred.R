# Title     : is_class_pred
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://probably.tidymodels.org/reference/as_class_pred.html



# ＜ポイント＞
# - pred_classオブジェクトの判定



# ＜構文＞
# is_class_pred(x)



# 1.使用例 ---------------------------------------------------------------------

library(probably)


# オブジェクト作成
x <- factor(1:5) %>% class_pred()
x %>% print()
x %>% class()
x %>% glimpse()


# オブジェクト判定
x %>% is_class_pred()

