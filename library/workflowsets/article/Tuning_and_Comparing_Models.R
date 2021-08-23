# **********************************************************************************
# Library   : workflowsets
# Title     : Tuning and Comparing Models
# Created by: Owner
# Created on: 2021/08/24
# URL       : https://workflowsets.tidymodels.org/articles/articles/tuning-and-comparing-models.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセットで複数の線形判別分析モデルを用いて学習する


# ＜目次＞
# 0 準備
# 1 モデル用データの作成
# 2 モデル構築
# 3 ワークフローセットの構築
# 4 ワークフローセットのリサンプル学習
# 5 予測精度の確認
# 6 オブジェクトの抽出


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(workflowsets)
library(discrim)


# データロード
data(parabolic)

# データ確認
parabolic %>% print()
parabolic %>% glimpse()


# 1 モデル用データの作成 --------------------------------------------------------------

# データ分割
set.seed(1)
split <- parabolic %>% initial_split()

# 確認
train_set <- split %>% training()
test_set <- split %>% testing()

# プロット確認
train_set %>% 
  ggplot(aes(x = X1, y = X2, col = class)) + 
    geom_point(alpha = 0.5) + 
    coord_fixed(ratio = 1) + 
    scale_color_brewer(palette = "Dark2")

# リサンプルデータの作成
set.seed(2)
train_resamples <- train_set %>% bootstraps()

# 確認
train_resamples %>% print()


# 2 モデル構築 ------------------------------------------------------------------------

# 線形判別分析
# --- flexible
mars_disc_spec <- 
  discrim_flexible(prod_degree = tune()) %>% 
    set_engine("earth")

# 線形判別分析
# --- regularized
reg_disc_sepc <- 
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>% 
    set_engine("klaR")

# 線形判別分析
# --- tree
cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
    set_engine("rpart") %>% 
    set_mode("classification")


# 3 ワークフローセットの構築 -------------------------------------------------------------

# ワークフローセットの定義
# --- 前処理はフォーミュラのみ（1パターン）
all_workflows <- 
  workflow_set(preproc = list("formula" = class ~ .), 
               models  = list(regularized = reg_disc_sepc, 
                              mars        = mars_disc_spec, 
                              cart        = cart_spec))

# 確認
all_workflows %>% print()



# 4 ワークフローセットのリサンプル学習 ----------------------------------------------------

# リサンプル学習
all_workflows <- 
  all_workflows %>% 
  workflow_map(resamples = train_resamples, grid = 20, verbose = TRUE)


# 確認
all_workflows %>% print()


# 5 予測精度の確認 -------------------------------------------------------------------------

# 予測精度のランキング
all_workflows %>% rank_results(rank_metric = "roc_auc")

# プロット作成
all_workflows %>% autoplot(metric = "roc_auc")

# プロット作成
# --- 特定モデル
all_workflows %>% autoplot(metric = "roc_auc", id = "formula_mars")



# 6 オブジェクトの抽出 ----------------------------------------------------------------------

# 結果の抽出
mars_results <- 
  all_workflows %>% 
    pull_workflow_set_result("formula_mars")

# 確認
mars_results %>% print()


# ワークフローの抽出
mars_workflow <- 
  all_workflows %>% 
    pull_workflow("formula_mars")

# 確認
mars_workflow


# モデルの抽出
mars_workflow_fit <- 
  mars_workflow %>% 
    finalize_workflow(tibble(prod_degree = 1)) %>% 
    fit(data = train_set)

# 確認
mars_workflow_fit

