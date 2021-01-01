# Title     : loo_cv
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/loo_cv.html



# ＜ポイント＞
# - Leave-one-out交差検証(LOO)では1レコードのみ評価データとして扱う
# - レコード数と同数のリサンプリングが行われる
# - 全てのデータが使われるのでオーバーフィッティングしやすい
# - リサンプリングが多くなるので学習コストも高い


# ＜構文＞
# loo_cv(data)



library(tidyverse)
library(tidymodels)



# リサンプリング
# --- Leave-one-out法
resample1 <- mtcars %>% loo_cv()
resample1 %>% print()


# 要素の確認
X$splits[[1]] %>% training()
X$splits[[1]] %>% testing()