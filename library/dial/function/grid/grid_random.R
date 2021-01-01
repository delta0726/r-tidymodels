# Title     : grid_random
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/14
# URL       : https://dials.tidymodels.org/reference/grid_regular.html



# ＜ポイント＞
# - ランダムサーチのグリッドを作成する



# ＜構文＞
# grid_random(x, ..., size = 5, original = TRUE, filter = NULL)



library(tidyverse)
library(tidymodels)
library(DataExplorer)




#%% グリッドの作成 ------------------------------------------

# パラメータの範囲を確認
penalty() %>% range_get(original = TRUE)
mixture() %>% range_get(original = TRUE)


# パラメータグリッドの作成
grid_random(penalty(), mixture(), size = 5)



#%% ランダム性の確認 ------------------------------------------

# パラメータグリッドの作成
X <- grid_random(penalty(), mixture(), size = 10000)


# 範囲の確認
X$penalty %>% range() %>% round(2)
X$mixture %>% range() %>% round(2)


# ヒストグラムの作成
X %>% plot_histogram()
