# Title     : tune
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/11
# URL       : https://tune.tidymodels.org/reference/tune.html



# ＜ポイント＞
# - チューニングするパラメータにプレースホルダー関数として適用する
# - tune_bays()やtune_grid()と併せて使用する



library(tidymodels)
library(tidyverse)


# クラス確認
X <- tune()
X %>% class()


# 関数の中身
tune %>% print()


# 関数のプレースホルダーとして導入
func <-
  nearest_neighbor(neighbors = tune("K"),
                   weight_func = tune(),
                   dist_power = tune())

# 確認
func %>% print()

