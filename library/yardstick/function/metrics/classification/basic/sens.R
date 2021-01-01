# Title     : sens（Sensitivity）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/20
# URL       : https://yardstick.tidymodels.org/reference/sens.html



# ＜ポイント＞
# - 正解(T)のうち、正しく〇(P)として分類できたデータの割合
# - TP / (TP + FN)


# ＜分類定義＞
# TP: 〇(P)と予想して正解(T)
# FN: ✕(N)と予想して不正解(F)
# FP: 〇(P)と予想して不正解(F)
# TN: ✕(N)と予想して正解(T)


# ＜構文＞
# sens(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...)




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
  sens(truth = truth, predicted)



#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  sens(obs, pred)



#%% グループごとに算出 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  sens(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  sens(obs, pred, estimator = "macro_weighted")



