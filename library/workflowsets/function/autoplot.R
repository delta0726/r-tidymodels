# **********************************************************************************
# Library   : workflowsets
# Function  : autoplot
# Created by: Owner
# Created on: 2021/08/23
# URL       : https://workflowsets.tidymodels.org/reference/autoplot.workflow_set.html
# **********************************************************************************


# ＜概要＞
# - {workflowsets}の情報を視覚化するためのプロットを作成する


# ＜構文＞
# autoplot(
#   object,
#   rank_metric = NULL,
#   metric = NULL,
#   id = "workflow_set",
#   select_best = FALSE,
#   std_errs = qnorm(0.95),
#   ...
# )


# ＜使用例＞
# 0 準備
# 1 プロット作成


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(workflowsets)


# オブジェクト確認
# --- 学習済ワークフローセット
two_class_res %>% print()


# メトリック一覧の抽出
# --- accucary/roc_aucが21個ずつ
two_class_res %>% collect_metrics() %>% print(n = nrow(.))


# 1 プロット作成 -------------------------------------------------------------------

# 基本プロット
# --- メトリックを信頼区間表示
two_class_res %>% autoplot()

# 上位プロット
# --- wflow_idごとの上位プロット
two_class_res %>% autoplot(select_best = TRUE)

# 特定シミュレーション
# --- チューニングパターンごとの結果
two_class_res %>% 
  autoplot(id = "yj_trans_cart", metric = "roc_auc")
