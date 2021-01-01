# Title     : unknown / is_unknown / has_unknowns
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://dials.tidymodels.org/reference/unknown.html



# ＜ポイント＞
# - unknown()は、値が後で指定されることを示すために使用されるプレースフォルダー
# - {dial}のチューニング関数の作成時に内部関数として使われる
# - unknownの箇所はデータフレームの大きさなどから判定して最終的に設定される




library(tidyverse)
library(tidymodels)



#%% 動作確認 -------------------------------------------------

# ただunknown()を返すだけ
unknown()


# 判定関数
unknown() %>% is_unknown()


# パラメータにunknown()を持つかを確認
# --- ｢?｣はunknownを示す
mtry() %>% print()
mtry() %>% has_unknowns()




#%% unknownの使用 -------------------------------------------------

# unknownをベクトルで設定
# --- リストで格納される
# --- パラメータの範囲を設定するイメージ
range <- c(1, unknown())
range %>% class()
range %>% print()


# 判定関数
range %>% is_unknown()




#%% チューニングオブジェクトとの関係 --------------------------------

# チューニングオブジェクト
X <- mtry()
X %>% names()


# オブジェクトの中身
# --- range/defaultにunknown()が含まれている
X$type
X$range
X$inclusive
X$trans
X$default
X$label
X$finalize
