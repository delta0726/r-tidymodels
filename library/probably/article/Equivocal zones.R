# Title     : Equivocal zones
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://probably.tidymodels.org/articles/equivocal-zones.html



# ＜ポイント＞
# - 分類問題の場合はクラス確率の水準で分類を行うが、ノイズの可能性のある曖昧ゾーンが存在する
# - 信頼区間のようなもので、曖昧ゾーンを考慮したモデル評価が必要な場合もある



# 0.はじめに -----------------------------------------------------------------

# ファクター定義
x <- factor(c("Yes", "No", "Yes", "Yes"))


# クラス定義
# --- Create a class prediction object
x %>% class_pred()


# クラス定義
# --- which引数でクラス数を定義
# --- Yes / No / [EQ]
x %>% class_pred(which = 3)



# 1.準備 ---------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(probably)
library(yardstick)
library(modeldata)


# データ準備
# --- ロジスティック回帰の結果データ
data("segment_logistic")


# データ確認
segment_logistic %>% print()
segment_logistic %>% glimpse()



# 2.クラス分類(バッファなし) -----------------------------------------------------

# 閾値を指定してクラス確率からクラス分類を作成
# > 0.5 = good
# < 0.5 = poor
segment_logistic_thresh <-
  segment_logistic %>%
    mutate(.pred = make_two_class_pred(estimate = .pred_good,
                                       levels = levels(Class),
                                       threshold = 0.5))

# 確認
segment_logistic_thresh %>% group_by(.pred) %>% count()



# 3.クラス分類(バッファあり) -----------------------------------------------------

# 閾値を指定してクラス確率からクラス分類を作成
# > 0.5 = good
# < 0.5 = poor
segment_pred <-
  segment_logistic %>%
    mutate(.pred = make_two_class_pred(estimate = .pred_good,
                                       levels = levels(Class),
                                       threshold = 0.5,
                                       buffer = 0.05))

# 確認
segment_pred %>% group_by(.pred) %>% count()



segment_pred %>%
  summarise(reportable = reportable_rate(.pred))







# 4.yardstickとの連携 -----------------------------------------------------------

# 注意
# --- ファクターに変換すると[EQ]はNAに変換される
# --- yardstickはラベルをファクターに変換して入力するので該当レコードは計算から除外される
segment_pred %>%
  mutate(.pred_fct = as.factor(.pred)) %>%
  count(.pred, .pred_fct)


# No equivocal zone
segment_logistic_thresh %>%
  mutate(.pred_fct = as.factor(.pred)) %>%
  precision(Class, .pred_fct)



# Equivocal zone
segment_pred %>%
  mutate(.pred_fct = as.factor(.pred)) %>%
  precision(Class, .pred_fct)