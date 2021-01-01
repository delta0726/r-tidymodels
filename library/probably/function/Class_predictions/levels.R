# Title     : levels
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://probably.tidymodels.org/reference/levels.class_pred.html



# ＜ポイント＞
# - pred_classオブジェクトで定義されたレベルを出力




# 1.使用例 ---------------------------------------------------------------------

# pred_classオブジェクトの作成
x <- factor(1:5) %>% class_pred(which = 1)
x %>% print()


# レベルの確認
x %>% levels()
