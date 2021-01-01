# Title     : j_index（J-index）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/2
# URL       : https://yardstick.tidymodels.org/reference/j_index.html


# ＜ポイント＞
# -



# ＜構文＞
# sens(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...)





library(tidyverse)
library(tidymodels)



#%% 2クラスの分類問題 -------------------------------------------

# データ準備
data("two_class_example")


# データ確認
two_class_example %>% as_tibble()



# j_indexの計算
two_class_example %>%
  j_index(truth = truth, predicted)



#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  j_index(obs, pred)


# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  j_index(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  j_index(obs, pred, estimator = "macro_weighted")