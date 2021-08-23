# **********************************************************************************
# Library   : workflowsets
# Function  : rank_results
# Created by: Owner
# Created on: 2021/08/24
# URL       : https://workflowsets.tidymodels.org/reference/rank_results.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセットの学習結果を特定のメトリックでランキング化してデータフレームで表示する


# ＜構文＞
# rank_results(x, rank_metric = NULL, select_best = FALSE)


# ＜使用例＞
# 0 準備
# 1 予測精度のランキング抽出


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(workflowsets)


# オブジェクトの確認
# --- 学習済ワークフローセット
chi_features_res

# メトリックの格納場所
chi_features_res$result[[1]]
chi_features_res$result[[1]]$.metrics


# 1 予測精度のランキング抽出 ------------------------------------------------------

# ランキング抽出
# --- 全て
chi_features_res %>% rank_results()

# ランキング抽出
# --- 各モデルのメトリックごとの1位のみ
chi_features_res %>% rank_results(select_best = TRUE)

# ランキング抽出
# --- メトリックを指定して抽出
chi_features_res %>% rank_results(rank_metric = "rsq")
