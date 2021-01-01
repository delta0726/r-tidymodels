# Title     : Getting Started
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/11
# URL       : https://tidyposterior.tidymodels.org/index.html




# ＜参考＞
#  - https://qiita.com/ysekky/items/16cd279c9be0dfb75217

library(tidyverse)
library(tidyposterior)



# データ準備
data(precise_example)


# データ確認
# --- Foldごとに各種モデルのメトリック値が格納されている
precise_example %>% print()
precise_example %>% glimpse()


# Accuracyのみ抽出
accuracy <-
  precise_example %>%
   select(id, contains("Accuracy")) %>%
   setNames(tolower(gsub("_Accuracy$", "", names(.))))


# 確認
accuracy %>% print()


# Accuracyの結果に対して
# --- Bayesian Analysis of Resampling Statistics
acc_model <- accuracy %>% perf_mod(seed = 13311, verbose = TRUE)
acc_model %>% print()


# 事後分布の州出
accuracy_dists <- acc_model %>% tidy()
accuracy_dists %>% print()
accuracy_dists %>% group_by(model) %>% tally()

# 信頼区間
# --- 事後分布に基づく

accuracy_dists %>% summary()