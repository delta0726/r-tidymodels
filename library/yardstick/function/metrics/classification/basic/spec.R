# Title     : spec（Specificity：特異度）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/1
# URL       : https://yardstick.tidymodels.org/reference/spec.html



# ＜ポイント＞
# - 特異度



# ＜構文＞
# spec(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...)




library(tidyverse)
library(tidymodels)



#%% 2クラスの分類問題 -------------------------------------------

# データ準備
data("two_class_example")


# データ確認
two_class_example %>% as_tibble()


# 混合行列
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted)


# 再現率の計算
two_class_example %>%
  spec(truth = truth, predicted)



#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  spec(obs, pred)


#%% グループごとに算出 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  spec(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  spec(obs, pred, estimator = "macro_weighted")


