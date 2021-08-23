# **********************************************************************************
# Library   : workflowsets
# Function  : as_workflow_set
# Created by: Owner
# Created on: 2021/03/18
# URL       : https://workflowsets.tidymodels.org/reference/as_workflow_set.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセットの各モデルのコメントを操作する


# ＜構文＞
# comment_add(x, id, ..., append = TRUE, collapse = "\n")
# comment_get(x, id)
# comment_reset(x, id)
# comment_print(x, id = NULL, ...)


# ＜使用例＞
# 0 準備
# 1 コメントの操作


# 0 準備 --------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(workflowsets)


# オブジェクトの確認
# --- 未学習のワークフローセット
two_class_set %>% print()


# コメントの格納場所
two_class_set$info[[1]]



# 1 コメントの操作 ---------------------------------------------------------------

# コメントの抽出
two_class_set %>% comment_get("none_cart")


# コメントの追加
new_set <-
  two_class_set %>%
  comment_add("none_cart", "What does 'cart' stand for\u2753") %>%
  comment_add("none_cart", "Classification And Regression Trees.")

# 確認
new_set %>% comment_print()
new_set$info[[1]]
