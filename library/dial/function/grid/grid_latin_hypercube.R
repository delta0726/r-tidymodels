# Title     : grid_latin_hypercube
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/14
# URL       : https://dials.tidymodels.org/reference/grid_max_entropy.html



# ＜ポイント＞
# - ラテン・ハイパーキューブ法によるグリッドサーチを行う
#   --- コンピュータの実験計画におけるパラメータグリッドを構築するために使用される
# -.空間の任意の部分に、それほど遠くない観測された組み合わせが設定される。



# ＜構文＞
# grid_latin_hypercube(x, ..., size = 3, original = TRUE)


# ＜参考＞
# Python ラテン超方格法などで水準表を作成する
# https://hk29.hatenablog.jp/entry/2019/11/30/192329



library(tidyverse)
library(tidymodels)



# ハイパーパラメータの範囲を確認
# --- penalty: 0 - 1
# --- mixture: 0 - 1
penalty() %>% range_get(original = TRUE)
mixture() %>% range_get(original = TRUE)



# グリッドの算出
grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 3,
  original = TRUE)


# グリッドの算出
grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 30,
  original = TRUE)

