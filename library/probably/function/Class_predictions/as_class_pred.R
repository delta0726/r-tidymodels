# Title     : as_class_pred
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://probably.tidymodels.org/reference/as_class_pred.html



# ＜ポイント＞
# - ファクター値をclass_predオブジェクトに強制変換する



# ＜構文＞
# as_class_pred(x, which = integer(), equivocal = "[EQ]")


# ＜引数＞
# - x : ファクター値



# 1.使用例 ---------------------------------------------------------------------

library(tidyverse)
library(probably)


# ファクター作成
x <- factor(c("Yes", "No", "Yes", "Yes"))
x %>% print()


# class_predオブジェクトに変換
obj <- x %>% as_class_pred()
obj %>% print()
obj %>% class()
obj %>% glimpse()

