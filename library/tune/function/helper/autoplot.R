# Title     : autoplot
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/17
# URL       : https://tune.tidymodels.org/reference/autoplot.tune_results.html



# ＜ポイント＞
# - チューニング結果を簡単にプロット化する
# - "marginals"は各ハイパーパラメータとmetricの関係を散布図で表示する
#   --- 数値パラメータはFacetとして表示される
#   --- 名義パラメータはcolor系列として表示される


# ＜構文＞
# autoplot(
#   object,
#   type = c("marginals", "parameters", "performance"),
#   metric = NULL,
#   width = NULL,
#   ...
# )



library(tidyverse)
library(tidymodels)


# データセット
# --- ames_grid_search（tune_grid）
# --- ames_iter_search（tune_bayes）
# --- ames_wflow
data("example_ames_knn")


# 確認
ames_grid_search %>% print()
ames_grid_search %>% collect_metrics()




# tune_grid -----------------------------------------

# パラメータごとのメトリックを表示
ames_grid_search %>% autoplot(metric = "rmse", type = "marginals")



# tune_bayes -----------------------------------------

# パラメータごとのメトリックを表示
ames_iter_search %>% autoplot(metric = "rmse", type = "marginals")


# イテレーションごとのパラメータ探索値を表示
ames_iter_search %>% autoplot(metric = "rmse", type = "parameters")


# イテレーションごとのメトリックの範囲を表示
ames_iter_search %>% autoplot(metric = "rmse", type = "performance")# }

