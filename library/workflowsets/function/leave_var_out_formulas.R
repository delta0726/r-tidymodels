# **********************************************************************************
# Library   : workflowsets
# Function  : leave_var_out_formulas
# Created by: Owner
# Created on: 2021/08/24
# URL       : https://workflowsets.tidymodels.org/reference/leave_var_out_formulas.html
# **********************************************************************************


# ＜概要＞
# - 初期モデル式から、1つの特徴量を除外したフォーミュラをリスト形式で作成
#   --- 特徴量の異なるモデルをワークフローセットで分析することを想定


# ＜構文＞
# leave_var_out_formulas(formula, data, full_model = TRUE, ...)



# ＜使用例＞
# 0 準備
# 1 フォーミュラ・パターンの作成


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(workflowsets)


# データロード
data(penguins, package = "modeldata")

# データ確認
penguins %>% print()
penguins %>% glimpse()


# 1 フォーミュラ・パターンの作成 ------------------------------------------------------

# フォーミュラ作成
formulas <- 
  leave_var_out_formulas(
    bill_length_mm ~ .,
    data = penguins
  )

# 確認
formulas %>% print()
