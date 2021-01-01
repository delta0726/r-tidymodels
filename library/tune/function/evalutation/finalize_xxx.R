# Title     : finalize_model / recipe / workflow
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/17
# URL       : https://tune.tidymodels.org/reference/finalize_model.html



# ＜ポイント＞
# - チューニング結果から対象パラメータに対してFoldごとの平均を算出して最良モデルを決める
# - チューニング可能な｢レシピ｣｢モデル｣の方法に対して｢finalize_*｣が用意されている


# ＜構文＞
# finalize_model(x, parameters)
# finalize_recipe(x, parameters)
# finalize_workflow(x, parameters)



library(tidyverse)
library(tidymodels)


# データセット
# --- ames_grid_search（tune_grid）
# --- ames_iter_search（tune_bayes）
# --- ames_wflow
data("example_ames_knn")



#%% モデル構築＆チューニング -------------------------------------------------

# モデル定義
# --- k近傍法
# --- 3つのハイパーパラメータをチューニング
knn_model <-
  nearest_neighbor(
    mode = "regression",
    neighbors = tune("K"),
    weight_func = tune(),
    dist_power = tune()
  ) %>%
  set_engine("kknn")


# チューニング結果の確認
# --- ここではチューニングしていない（ames_grid_searchにチューニング結果が作成されている）
ames_grid_search %>% print()
ames_grid_search$.metrics[[1]]



#%% 最良パラメータの決定 -------------------------------------------------

# ベストパラメータの取得
# --- ames_grid_searchにチューニング結果が作成されている
lowest_rmse <-
  ames_grid_search %>%
    select_best(metric = "rmse")


# 確認
lowest_rmse %>% print()


# 計算証明
# --- select_best()
ames_grid_search %>%
  select(id, .metrics) %>%
    unnest() %>%
    dplyr::filter(.metric == "rmse") %>%
    group_by(K, weight_func, dist_power) %>%
    summarize(mean = mean(.estimate)) %>%
    ungroup() %>%
    arrange(mean) %>%
    slice(1)



#%% 最終モデルの作成 -------------------------------------------------

# 現行モデル
knn_model %>% print()


# 最終モデルの定義
knn_model %>%
  finalize_model(lowest_rmse) %>%
  print()

