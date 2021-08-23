# **********************************************************************************
# Library   : workflowsets
# Function  : # collect_metrics
# Created by: Owner
# Created on: 2021/08/23
# URL       : https://workflowsets.tidymodels.org/reference/collect_metrics.workflow_set.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセットからメトリックを抽出する


# ＜構文＞
# collect_metrics(x, summarize = TRUE, ...)


# ＜使用例＞
# 0 準備
# 1 メトリック一覧の抽出


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(workflowsets)


# オブジェクト確認
two_class_res %>% print()


# オブジェクト構造の確認
# --- メトリックはresultの中にある
two_class_res %>% names()
two_class_res$result[[1]]



# 1 メトリック一覧の抽出 ------------------------------------------------------------

# Foldを集計
two_class_res %>% collect_metrics(summarize = TRUE)

# Foldを個別に出力
two_class_res %>% collect_metrics(summarize = FALSE)

# 個別処理で抽出 
# --- resultの中からメトリックを抽出
two_class_res %>%
  dplyr::filter(grepl("cart", wflow_id)) %>%
  mutate(metrics = map(result, collect_metrics)) %>%
  dplyr::select(wflow_id, metrics) %>%
  tidyr::unnest(cols = metrics)
