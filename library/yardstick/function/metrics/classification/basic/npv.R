# Title     : npv（Negative predictive value：陰性的中率）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/1
# URL       : https://yardstick.tidymodels.org/reference/npv.html



# ＜ポイント＞
# -



# ＜構文＞
# npv(
#   data,
#   truth,
#   estimate,
#   prevalence = NULL,
#   estimator = NULL,
#   na_rm = TRUE,
#   ...
# )




library(tidyverse)
library(tidymodels)


data("two_class_example")


# データ確認
# --- 2クラスの分類問題
two_class_example %>% as_tibble()


# 混合行列
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted)


#
two_class_example %>%
  npv(truth = truth, predicted)



#%% 1グループで算出 -------------------------------------------

# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  npv(obs, pred)


# 計算証明
#solubility_test %>%
#  mutate(Nmerator    = (solubility - prediction)^2,
#         Denominator = (solubility - mean(prediction))^2) %>%
#  summarise(Nmerator    = sum(Nmerator),
#            Denominator = sum(Denominator)) %>%
#  mutate(rsq = 1 - Nmerator/ Denominator))




#%% グループごとに算出 -------------------------------------------

# グループごとに算出
recall(two_class_example, truth, predicted)hpc_cv %>%
  group_by(Resample) %>%
  npv(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  npv(obs, pred, estimator = "macro_weighted")


