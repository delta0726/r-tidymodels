# Title     : show_best / select_best
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/16
# URL       : https://tune.tidymodels.org/reference/show_best.html


# ＜ポイント＞
# - チューニング結果から指定したメトリックに従って最良パラメータを抽出する
# - パラメータの水準だけでなくモデルの単純さに着目した抽出も可能



# ＜構文＞
# show_best(x, metric, n = 5, ...)
# select_best(x, metric, ...)
# select_by_pct_loss(x, ..., metric, limit = 2)
# select_by_one_std_err(x, ..., metric)



library(tidyverse)
library(tidymodels)


# データセット
# --- ames_grid_search（tune_grid）
# --- ames_iter_search（tune_bayes）
# --- ames_wflow
data("example_ames_knn")



# 確認
ames_iter_search %>% print()
ames_iter_search$.metrics[[1]]


# メトリックの抽出
ames_iter_search %>% collect_metrics()


# show_best() -------------------------------------

# メトリック上位のパラメータ組み合わせを表示
ames_iter_search %>% show_best(metric = "rmse", n = 10)



# select_best() -----------------------------------

# 最良モデルの取得
ames_grid_search %>% select_best(metric = "rmse")


# select_best()
# --- 計算証明
ames_grid_search %>%
  select(id, .metrics) %>%
    unnest() %>%
    dplyr::filter(.metric == "rmse") %>%
    group_by(K, weight_func, dist_power) %>%
    summarize(mean = mean(.estimate)) %>%
    ungroup() %>%
    arrange(mean) %>%
    slice(1)



# select_by_one_std_err() -----------------------------------------

# 数値的に最適な結果の1つの標準誤差内にある最も単純なモデルを選択する
ames_grid_search %>% select_by_one_std_err(metric = "rmse", desc(K))



# select_by_pct_loss() -----------------------------------------

# パフォーマンスの低下が許容範囲内である最も単純なモデルを選択します。
ames_grid_search %>% select_by_pct_loss(metric = "rmse", limit = 5, desc(K))


