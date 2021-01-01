# Title     : class_pred
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://probably.tidymodels.org/reference/class_pred.html



# ＜ポイント＞
# - class_predオブジェクトを定義する
# - 分類問題の出力値をclass_predオブジェクトに変換して｢曖昧ゾーン｣を含めたパフォーマンス評価を行う
# - オブジェクトの出力で表示される｢Reportable｣は、全体のファクター値のうち｢equivocal｣以外の要素の割合を示す



# ＜構文＞
# class_pred(x = factor(), which = integer(), equivocal = "[EQ]")


# ＜引数＞
# - x         : ファクター値
# - which     : equivocal(曖昧)と定義するファクター値の場所
# - equivocal : equivocalを表現する文字列表記



# 1.準備 ---------------------------------------------------------------------

library(tidyverse)
library(probably)


# ファクター準備
x <- factor(c("Yes", "No", "Yes", "Yes"))



# 2.class_predオブジェクトに変換 ------------------------------------------------

# そのまま変換
x1 <- x %>% class_pred()
x1 %>% print()


# witch引数
# --- equivocal(曖昧)と定義するファクター値の場所
# --- ｢Reportable｣は、全体のファクター値のうち｢equivocal｣以外の要素の割合を示す
x2 <- x %>% class_pred(which = 3)
x2 %>% print()


# equivocal引数
# --- equivocalを表現する文字列表記
x3 <- x %>% class_pred(which = 3, equivocal = "eq_value")
x3 %>% print()

