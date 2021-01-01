# Title     : Detection prevalence
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/2
# URL       : https://yardstick.tidymodels.org/reference/detection_prevalence.html




# ＜ポイント＞
# -



# ＜構文＞
# detection_prevalence(
#   data,
#   truth,
#   estimate,
#   estimator = NULL,
#   na_rm = TRUE,
#   ...
# )




library(tidyverse)
library(tidymodels)



#%% 2クラスの分類問題 -------------------------------------------

# データ準備
data("two_class_example")


# データ確認
two_class_example %>% as_tibble()



# detection_prevalenceの計算
two_class_example %>%
  detection_prevalence(truth = truth, predicted)



#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  detection_prevalence(obs, pred)


# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  detection_prevalence(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  detection_prevalence(obs, pred, estimator = "macro_weighted")
