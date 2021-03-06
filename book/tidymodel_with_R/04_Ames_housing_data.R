#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 4 The Ames Housing Data
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/13
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜ポイント＞
# - モデリングをする際には対数変換により正規分布に近い方がよい
# - 対数変換のデメリットは、元の単位が失われてパフォーマンス解釈がしにくい点にある
#   --- 回帰問題でRMSEで評価する際に、RMSE0.15ログ単位というのは直観的ではない


# ＜参考＞
# 前処理の推奨事項
# - 左右対称に変換(transform)する必要があるモデルを確認
# https://www.tmwr.org/pre-proc-table.html


# ＜目次＞
# 0 準備
# 1 プロットでデータを確認
# 2 次章以降のためのデータ加工


# 0 準備 --------------------------------------------------------------------------------

library(tidyverse)
library(modeldata)

# データロード
data(ames)

# 確認
ames %>% dim()
ames %>% glimpse()


# 1 プロットでデータを確認 -------------------------------------------------------------

# ヒストグラム作成
# --- Sale_Price（住宅価格）
# --- 右に歪んでおり、高価な家よりも安価な家の方が多いことを示している
ames %>%
  ggplot(aes(x = Sale_Price)) +
    geom_histogram(bins = 50)

# ヒストグラム作成
# --- Sale_Price（住宅価格）
# --- 対数変換（プロットの軸を対数変換）
ames %>%
  ggplot(aes(x = Sale_Price)) +
    geom_histogram(bins = 50) +
    scale_x_log10()


# 2 次章以降のためのデータ加工 ----------------------------------------------------------

# データ再定義
# --- Sale_Priceを対数変換
# --- 次章以降ではamesデータは以下のように定義したものを使う
ames <-
  ames %>%
    mutate(Sale_Price = log10(Sale_Price))
