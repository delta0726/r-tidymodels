# **********************************************************************************
# Library   : workflowsets
# Function  : collect_predictions
# Created by: Owner
# Created on: 2021/08/23
# URL       : https://workflowsets.tidymodels.org/reference/collect_metrics.workflow_set.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセットから予測値を抽出する


# ＜構文＞
# collect_predictions(
#   x,
#   summarize = TRUE,
#   parameters = NULL,
#   select_best = FALSE,
#   metric = NULL,
#   ...
# )


# ＜使用例＞
# 0 準備
# 1 予測値の抽出


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(workflowsets)


# オブジェクト確認
two_class_res %>% print()


# オブジェクト構造の確認
# --- predictionは保存されていない
two_class_res %>% names()
two_class_res$option[[1]] %>% str()



# 1 予測値の抽出 ---------------------------------------------------------------------
