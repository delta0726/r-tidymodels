# Title     : f_meas（F Measure）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/21
# URL       : https://yardstick.tidymodels.org/reference/f_meas.html



# ＜ポイント＞
# - F値 = (2 * recall * precision) / (recall + precision)
# - ｢再現率｣｢適合率｣の調和平均を見ている（調和平均は比率の平均を見るための指標）



# ＜構文＞
# f_meas(data, truth, estimate, beta = 1, estimator = NULL, na_rm = TRUE, ...)



library(tidyverse)
library(tidymodels)



#%% 2クラスの分類問題 -------------------------------------------

# データ準備
data("two_class_example")


# データ確認
two_class_example %>% as_tibble()



# F値の計算
two_class_example %>%
  f_meas(truth = truth, predicted)



#%% マルチクラスの分類問題 -------------------------------------------

# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  f_meas(obs, pred)



#%% グループごとに算出 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  f_meas(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  f_meas(obs, pred, estimator = "macro_weighted")