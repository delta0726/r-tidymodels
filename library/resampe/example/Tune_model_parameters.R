# Title     : モデルパラメータのチューニング
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/05
# URL       : https://www.tidymodels.org/start/tuning/


# ＜ポイント＞
#・ハイパーパラメーターは、モデルのトレーニング中にデータセットから直接学習することができない
#・よって、リサンプリングされたデータセットで多くのモデルをトレーニングしてモデルパフォーマンスから最良の値を推定する
#・このプロセスを｢チューニング｣といい、最終モデルを構築する手前のプロセスとして位置づけられる



library(tidyverse)
library(tidymodels)
library(modeldata)
library(scales)
library(vip)


# データロード
data(cells, package = "modeldata")


#%% 前処理 ----------------------------------------------------------

# データ分割
set.seed(123)
cell_split <- cells %>% select(-case) %>% initial_split(strata = class)
cell_split %>% print()


# 分割データの取得
cell_train <- cell_split %>% training()
cell_test  <- cell_split %>% testing()
cell_train %>% dim()
cell_test %>% dim()



#%% モデル構築 ----------------------------------------------------------

# モデリング
# --- チューニングパラメータあり
tune_spec <-
  decision_tree(cost_complexity = tune(), tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")


# チューニングパラメターの設定
# --- dial::grid_regular
tree_grid <- grid_regular(cost_complexity(), tree_depth(), levels = 5)
tree_grid %>% count(tree_depth)
tree_grid %>% as.data.frame()


# データ分割
# --- 10 fold
set.seed(234)
cell_folds <- cell_train %>% vfold_cv()
cell_folds %>% print()


# ワークフロー設定
set.seed(345)
tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(class ~ .)



#%% チューニング ----------------------------------------------------------


# チューニング
tree_res <- tree_wf %>% tune_grid(resamples = cell_folds, grid = tree_grid)
tree_res


# パフォーマンス統計量の取得
tree_res %>% collect_metrics()


# プロット作成
# --- パフォーマンス統計量
tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)


# パフォーマンス上位のパラメータ
tree_res %>% show_best("roc_auc")


# ベストパフォーマンスのパラメータ
best_tree <- tree_res %>% select_best("roc_auc")
best_tree %>% print()


# ワークフローにパラメータを設定
final_wf <-
  tree_wf %>%
  finalize_workflow(best_tree)


# ワークフローの完成
final_wf %>% print()



#%% モデル学習 ---------------------------------------------------

# 学習
final_tree <- final_wf %>% fit(data = cell_train)
final_tree %>% print()


# 変数重要度分析
final_tree %>%
  pull_workflow_fit() %>%
  vip()



#%% 最終モデルの分析 ---------------------------------------------------

# 学習
final_fit <- final_wf %>% last_fit(cell_split)
final_fit %>% collect_metrics()


# AUCプロット
final_fit %>%
  collect_predictions() %>%
  roc_curve(class, .pred_PS) %>%
  autoplot()
