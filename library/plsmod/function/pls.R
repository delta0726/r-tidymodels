# Title     : pls
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://plsmod.tidymodels.org/reference/pls.html



# ＜ポイント＞
# -



# ＜構文＞
# pls(mode = "unknown", num_terms = NULL, num_comp = NULL)
#
## S3 method for pls
#update(
#  object,
#  parameters = NULL,
#  num_terms = NULL,
#  num_comp = NULL,
#  fresh = FALSE,
#  ...
#)


# ＜引数＞
# - num_terms：各PLS負荷に影響を与えることが許可されている予測子の数。
# - num_comp ：保持するPLSコンポーネントの数。




# 1.準備 ---------------------------------------------------------------------

library(plsmod)




# 2.モデル構築 ---------------------------------------------------------------------


# 回帰問題
pls(num_comp = 2, num_terms = 10) %>%
  set_engine("mixOmics") %>%
  set_mode("regression") %>%
  translate()


# 分類問題
pls(num_comp = 2, num_terms = 10) %>%
  set_engine("mixOmics") %>%
  set_mode("classification") %>%
  translate()



