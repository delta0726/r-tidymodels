# **********************************************************************************
# Library   : workflowsets
# Function  : option_*
# Created by: Owner
# Created on: 2021/08/24
# URL       : https://workflowsets.tidymodels.org/reference/option_add.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセットのoption列の内容を操作する


# ＜構文＞
# option_add(x, ..., id = NULL, strict = FALSE)
# option_remove(x, ...)
# option_add_parameters(x, id = NULL, strict = FALSE)


# ＜使用例＞
# 0 準備
# 1 オプションの追加
# 2 オプションにパラメータを追加


# 0 準備 --------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(workflowsets)


# オブジェクトの確認
# --- 未学習ワークフローセット
two_class_set

# オプション
two_class_set$option[[1]]



# 1 オプションの追加 -------------------------------------------------------------

# オプションの追加
# --- 全てのモデルに同じ情報を追加
wfset_1 <- two_class_set %>% option_add(a = 1)
wfset_1$option


# オプションの追加
# --- 全てのモデルに同じ情報を追加
# --- 指定したモデルに情報を追加
wfset_2 <- 
　two_class_set %>%
    option_add(a = 1) %>%
    option_add(b = 2, id = "none_cart")

# 確認
wfset_2$option


# 2 オプションにパラメータを追加 -------------------------------------------------

# オプションの追加
# --- チューニングパラメータの転記
wfset_3 <- 
  two_class_set %>%
    option_add_parameters()

# 確認
wfset_3$option
wfset_3$option[[1]]
wfset_3$option[[1]]$param_info
