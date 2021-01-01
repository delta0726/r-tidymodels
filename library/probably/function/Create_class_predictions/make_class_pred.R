# Title     : make_class_pred
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/2
# URL       : https://probably.tidymodels.org/reference/make_class_pred.html



# ＜ポイント＞
# - presdict()の出力値からpred_classオブジェクトを作成する
#   ---データフレームの中で列として使用することが想定されている
# - バイナリ問題とマルチクラス問題で関数が異なる
#   --- バイナリ   ：make_two_class_pred
#   --- マルチクラス：make_class_pred



# ＜構文１＞
# make_two_class_pred(
#   estimate,
#   levels,
#   threshold = 0.5,
#   ordered = FALSE,
#   buffer = NULL
# )


# ＜引数＞
# - estimate  ： クラス確率
# - levels    ： レベルの表記
# - threshold ： 分類の閾値
# - ordered   ：
# - buffer    ： バッファー



# ＜構文２＞
# make_class_pred(
#   ...,
#   levels,
#   ordered = FALSE,
#   min_prob = 1/length(levels)
#  )

# ＜引数＞
# - min_prob  ：下限確率





# 1.準備：バイナリ分類 ----------------------------------------------------------------

library(tidyverse)
library(probably)


# データ準備
data("segment_logistic")


# データ確認
# --- 分類問題のpredict()の出力を想定したもの
segment_logistic %>% print()
segment_logistic %>% glimpse()


# クラス確率
# --- .pred_good
good <- segment_logistic$.pred_good


# クラスのレベルを取得
lvls <- segment_logistic$Class %>% levels()



# 2 バッファーゾーンの指定(両側) -----------------------------------------------------

# バッファゾーンの設定
# --- 中央値の0.5を中心に0.15の範囲
# --- 0.35 - 0.65
good %>% make_two_class_pred(lvls, buffer = 0.15)


# 確認
segment_logistic %>%
  mutate(.class_pred = make_two_class_pred(estimate = .pred_good,
                                           levels = levels(Class),
                                           buffer = 0.15)) %>%
  mutate(.class_pred_str = replace_na(as.character(.class_pred), "buffer")) %>%
  split(.$.class_pred_str) %>%
  map(select, .pred_good) %>%
  map(summary)



# 3 バッファーゾーンの指定(片側ずつ指定) -----------------------------------------------------

# バッファゾーンの設定
# --- 中央値の0.5を中心に下側0.05、上側0.65の範囲
# --- 0.45 - 0.65
good %>% make_two_class_pred(lvls, buffer = c(0.05, 0.15))


# 確認
segment_logistic %>%
  mutate(.class_pred = make_two_class_pred(estimate = .pred_good,
                                           levels = levels(Class),
                                           buffer = c(0.05, 0.15))) %>%
  mutate(.class_pred_str = replace_na(as.character(.class_pred), "buffer")) %>%
  split(.$.class_pred_str) %>%
  map(select, .pred_good) %>%
  map(summary)




# 4.マルチクラス分類 ----------------------------------------------------------------

# データ確認
species_probs %>% print()
species_probs %>% glimpse()


# バッファゾーンの設定
# --- 中央値の0.5を中心に下側0.05、上側0.65の範囲
# --- 0.45 - 0.65
species_buffer <-
  species_probs %>%
    mutate(.class_pred = make_class_pred(.pred_bobcat, .pred_coyote, .pred_gray_fox,
                                         levels = levels(Species),
                                         min_prob = .5))


# 確認1
species_probs %>% print()


# 確認2
species_probs %>%
  mutate(.class_pred = make_class_pred(.pred_bobcat, .pred_coyote, .pred_gray_fox,
                                       levels = levels(Species),
                                       min_prob = .5)) %>%
  mutate(.class_pred_str = replace_na(as.character(.class_pred), "buffer")) %>%
  split(.$.class_pred_str) %>%
  map(select, .pred_bobcat, .pred_coyote, .pred_gray_fox, .class_pred_str)


