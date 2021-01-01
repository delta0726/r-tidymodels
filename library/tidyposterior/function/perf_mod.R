# ****************************************************************************
# Title     : pref_mod
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/11
# URL       : https://tidyposterior.tidymodels.org/reference/perf_mod.html
# ****************************************************************************


# ＜概要＞
# -




library(tidyverse)
library(tidyposterior)


# データ準備
data(precise_example)


# データ確認
# --- Foldごとに各種モデルのメトリック値が格納されている
precise_example %>% print()
precise_example %>% glimpse()


# ROCのみを抽出
rocs <-
  precise_example %>%
   select(id, contains("ROC")) %>%
   setNames(tolower(gsub("_ROC$", "", names(.))))


# 確認
rocs %>% print()


# リサンプリング統計のベイズ分析
roc_model <- rocs %>% perf_mod(seed = 2824)
roc_model %>% print()
roc_model %>% glimpse()



# 事後分布の取得
# --- 事後分析のメトリック値
roc_post <- roc_model %>% tidy()

