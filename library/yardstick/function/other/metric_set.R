# Title     : metric_set
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       : https://yardstick.tidymodels.org/reference/metric_set.html



# ＜ポイント＞
# - 複数のメトリック関数を組み合わせて計算するラッパー関数を定義する


# ＜構文＞
# metric_set(...)



library(tidyverse)
library(tidymodels)



#%% 回帰問題 ---------------------------------------------------------

# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()
solubility_test %>% glimpse()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()


# 関数定義
# --- 指定したメトリックを計算する新しい関数
multi_metric <- metric_set(rmse, rsq, ccc)
multi_metric %>% print()
multi_metric %>% class()


# メトリックの出力
solubility_test %>%
  multi_metric(truth = solubility, estimate = prediction)




#%% 分類問題 ---------------------------------------------------------

# データ確認
# --- 分類問題を想定したデータセット
hpc_cv %>% as_tibble()
hpc_cv %>% glimpse()


# プロット
hpc_cv %>%
  conf_mat(truth = obs, estimate= pred) %>%
  autoplot(type = "heatmap")



# 関数定義
# --- 指定したメトリックを計算する新しい関数
class_metrics <- metric_set(accuracy, kap)


# メトリックの出力
# --- Foldごとに集計
hpc_cv %>%
  group_by(Resample) %>%
  class_metrics(truth = obs, estimate = pred)



#%% 新たなメトリックの作成 ---------------------------------------------------------

# 関数定義
# --- 元の関数をラップする形で定義する
ccc_with_bias <- function(data, truth, estimate, na_rm = TRUE, ...) {
  ccc(
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    # set bias = TRUE
    bias = TRUE,
    na_rm = na_rm,
    ...
  )
}


# クラスと属性の定義
class(ccc_with_bias) <- class(ccc)
attr(ccc_with_bias, "direction") <- attr(ccc, "direction")




#%% 新たなメトリックを含む計算 ---------------------------------------------------------

# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()
solubility_test %>% glimpse()


# 関数定義
# --- 指定したメトリックを計算する新しい関数
# --- 独自定義の関数を含む
multi_metric2 <- metric_set(rmse, rsq, ccc_with_bias)
multi_metric2 %>% print()


# メトリックの出力
solubility_test %>%
  multi_metric2(truth = solubility, estimate = prediction)




#%% 分類とクラス確率の両方出力 ---------------------------------------------------------


# データ確認
# --- 分類問題を想定したデータセット
hpc_cv %>% as_tibble()
hpc_cv %>% glimpse()


# 関数定義
# --- 指定したメトリックを計算する新しい関数
# --- クラス確率：roc_auc, pr_auc
# --- 分類     ：accuracy
class_and_probs_metrics <- metric_set(roc_auc, pr_auc, accuracy)


# メトリックの出力
hpc_cv %>%
  as_tibble() %>%
  filter(Resample == "Fold01") %>%
  class_and_probs_metrics(obs, VF:L, estimate = pred)
