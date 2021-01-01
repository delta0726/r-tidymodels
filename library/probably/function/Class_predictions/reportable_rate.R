# Title     : reportable_rate
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://probably.tidymodels.org/reference/reportable_rate.html



# ＜ポイント＞
# - ｢Reportable｣の値を取得する



# 1.使用例 ---------------------------------------------------------------------

# pred_classオブジェクトの作成
x <- factor(1:5) %>% class_pred(which = c(1, 2))
x %>% print()


# ｢Reportable｣の値を取得
x %>% reportable_rate()
