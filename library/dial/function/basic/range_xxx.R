# Title     : range_validate / range_get / range_set
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/23
# URL       : https://dials.tidymodels.org/reference/range_validate.html


# ＜ポイント＞
# - パラメータレンジのセッター関数とゲッター関数
#



# ＜構文＞
# range_validate(object, range, ukn_ok = TRUE)
# --- ?



# ＜構文＞
# range_get(object, original = TRUE)
# --- オブジェクトの範囲を取得する


# ＜構文＞
# range_set(object, range)
# --- オブジェクトの範囲を設定する



library(tidyverse)
library(tidymodels)


# 確認
# --- quant_paramオブジェクトには初期状態では｢value｣はない
penalty()
penalty() %>% class()
penalty() %>% names()


# valueの設定
my_lambda <- penalty() %>% value_set(-4:-1)
my_lambda %>% print()
my_lambda %>% names()
my_lambda$values





#%% range_validate() --------------------------------------------------


try(
  range_validate(my_lambda, c(-10, NA)), silent = TRUE
) %>%
  print()




#%% range_get() ---------------------------------------------------------

# レンジの取得
# --- ゲッター関数
my_lambda %>% range_get(original = FALSE)
my_lambda$range



#%% range_set() ---------------------------------------------------------


# レンジの設定
# --- セッター関数
my_lambda %>%
  range_set(c(-10, 2)) %>%
  range_get()

my_lambda %>%
  range_set(c(-10, 2)) %>%
  range_get(original = FALSE)