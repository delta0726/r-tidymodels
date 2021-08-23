# **********************************************************************************
# Library   : workflowsets
# Function  : as_workflow_set
# Created by: Owner
# Created on: 2021/08/24
# URL       : https://workflowsets.tidymodels.org/reference/workflow_set.html
# **********************************************************************************


# ＜概要＞
# - 個別のレシピとモデルからワークフローセットを定義する


# ＜構文＞
# workflow_set(preproc, models, cross = TRUE)



# ＜使用例＞
# 0 準備
# 1 レシピ作成
# 2 モデル定義
# 3 ワークフローセットの作成


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(workflowsets)


# データ準備
data(cells)
cells <- cells %>% dplyr::select(-case)

# データ確認
cells %>% print()
cells %>% glimpse()

# データ分割
set.seed(1)
val_set <- cells %>% validation_split()



# 1 レシピ作成 ------------------------------------------------------------------------

# 基本レシピ
basic_recipe <-
  recipe(class ~ ., data = cells) %>%
    step_YeoJohnson(all_predictors()) %>%
    step_normalize(all_predictors())

# レシピ1：PCA
# --- チューニングあり
pca_recipe <-
  basic_recipe %>%
    step_pca(all_predictors(), num_comp = tune())

# レシピ2：空間記号
ss_recipe <-
  basic_recipe %>%
    step_spatialsign(all_predictors())


# 2 モデル定義 ------------------------------------------------------------------------

# k近傍法
# --- チューニングあり
knn_mod <-
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
    set_engine("kknn") %>%
    set_mode("classification")

# ロジスティック回帰
lr_mod <-
  logistic_reg() %>%
  set_engine("glm")


# 3 ワークフローセットの作成 ----------------------------------------------------------

# リスト登録
# --- 前処理（レシピ）
# --- モデル
preproc <- list(none = basic_recipe, pca = pca_recipe, sp_sign = ss_recipe)
models <- list(knn = knn_mod, logistic = lr_mod)

# ワークフローセットの作成
# --- cross=TRUEの場合は全パターンのワークフローが登録される
cell_set <- workflow_set(preproc, models, cross = TRUE)
cell_set %>% print()

# 確認
# --- infoの中に前処理とモデルが格納されている
cell_set_cross$info[[1]]

