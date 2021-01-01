# Title     : grid_regular
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/14
# URL       : https://dials.tidymodels.org/reference/grid_regular.html



# ＜ポイント＞
# - ランダムサーチのグリッドを作成する



# ＜構文＞
# grid_regular(x, ..., levels = 3, original = TRUE, filter = NULL)



library(tidyverse)
library(tidymodels)
library(DataExplorer)
library(tidyquant)



#%% グリッドの作成 ------------------------------------------

# パラメータの範囲を確認
penalty() %>% range_get(original = TRUE)
mixture() %>% range_get(original = TRUE)


# パラメータグリッドの作成
# --- levels引数は1つのパラメータで検索する数を指定する
# --- パラメータ数が2なので、5^2=25
grid_regular(penalty(), mixture(), levels = 5)



#%% ランダム性の確認 ------------------------------------------

# パラメータグリッドの作成
X <- grid_regular(penalty(), mixture(), levels = 100)


# 範囲の確認
X$penalty %>% range() %>% round(2)
X$mixture %>% range() %>% round(2)


# ヒストグラムの作成
g <- X %>% plot_histogram(ggtheme = theme_tq())

