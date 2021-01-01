# Title     : モデルパラメータのチューニング
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/18
# URL       : https://www.tidymodels.org/start/tuning/


# ＜ポイント＞
# - ハイパーパラメータはモデル学習で直接学習することができない
# - モデルチューニングプロセスで事前に適切な水準を推定しておく必要がある
# - ツリーベースモデルはいくつかのハイパーパラメータがモデル精度に大きく影響する


# ＜目次＞
# 1 データ準備
# 2 データ分割
# 3 チューニング設定
# 4 チューニング
# 5 パフォーマンス評価
# 6 最終モデルで学習



# 1 データ準備 ---------------------------------------------------


library(tidyverse)
library(tidymodels)
library(modeldata)
library(vip)


# データロード
data(cells, package = "modeldata")


# データ概要
# --- 58変数
# --- Y:class
cells %>% print()
cells %>% glimpse()


# データ確認
# --- A tibble: 2,019 x 58
cells %>% as_tibble()
cells %>% glimpse()


# 比率確認
# --- PS / WS
cells %>%
  count(class) %>%
  mutate(prop = n/sum(n))

## A tibble: 2 x 3
#  class     n  prop
#  <fct> <int> <dbl>
#1 PS     1300 0.644
#2 WS      719 0.356



# 2 データ分割 ---------------------------------------------------

# データ分割
set.seed(123)
cell_split <- cells %>% select(-case) %>% initial_split(strata = class)
cell_train <- cell_split %>% training()
cell_test  <- cell_split %>% testing()


# データ確認
cell_train %>% print()
cell_test %>% print()


# バリデーションデータの作成
# --- 訓練データを使用
# --- 10Fold Cross-Validation
set.seed(234)
cell_folds <- cell_train %>% vfold_cv()
cell_folds %>% print()



# 3 チューニング設定 ---------------------------------------------------

# ＜ポイント＞
# - ｢ランダムフォレスト｣は、既定のハイパーパラメーターで実行しても頑健な結果を返す
# - ｢ブーストツリー｣や｢ディシジョンツリー｣は、ハイパーパラメーターの値に影響されやすい
#   --- 以降では｢ディシジョンツリー｣のパラメータを調整する


# ＜ハイパーパラメータ＞
# - complexity: 木の剪定して複雑度を調整
# - tree_depth: 木の深さ



# モデル構築
# --- チューニング設定あり
tune_spec <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>%
  set_engine("rpart") %>%
  set_mode("classification")


# 確認
tune_spec %>% print()



# パラメータセットの作成
# --- dial::grid_regular()はグリッドサーチ用のパラメータセットを作成する
# --- dial::cost_complexity()などは一般的なパラメータ検索範囲をデフォルト値として提供する
tree_grid <-
  grid_regular(cost_complexity(),
               tree_depth(),
               levels = 5)


# 確認
tree_grid %>% as.data.frame() %>% print()
tree_grid %>% count(tree_depth)



# 4 チューニング ---------------------------------------------------

# ワークフロー設定
# --- 単純化のためレシピは適用しない
set.seed(345)
tree_wf <-
  workflow() %>%
    add_model(tune_spec) %>%
    add_formula(class ~ .)


# チューニング
# --- tune::fit_resample()と同じ出力パターン
tree_res <-
  tree_wf %>%
    tune_grid(resamples = cell_folds, grid = tree_grid)


# 確認
# --- Fold: 10個
# --- チューニング・グリッド: 25
# --- メトリック: 2
tree_res %>% print()



# 5 パフォーマンス評価 ---------------------------------------------------

# ＜ポイント＞
# - tree_depthが1のツリーは、各メトリックのcost_complexityから見て最悪のモデルであることが確認できる
# - tree_depthが4あたりが最良パフォーマンスとなっている


# チューニング結果の取得
# --- collect_metrics()で簡単にデータフレームに変換
tree_res %>%
  collect_metrics() %>%
  as.data.frame()


# プロット
# --- X    : cost_complexity
# --- Color: tree_depth
tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)


# パラメータをランキング順に表示
tree_res %>% show_best("roc_auc")




# 6 最終モデルで学習 ---------------------------------------------------

# 最良パラメータの抽出
# --- メトリックを指定
# --- 機械的に最良のものを取得するので注意
best_tree <- tree_res %>% select_best("roc_auc")
best_tree %>% print()


# ワークフローの完成
# --- tune()だったパラメータに入力される
final_wf <- tree_wf %>% finalize_workflow(best_tree)
tree_wf %>% print()
final_wf %>% print()


# モデル学習
# --- 通常の学習
final_tree <- final_wf %>% fit(data = cell_train)
final_tree %>% print()


# モデルのフィッティング
# --- {tune}の出力パターン
# --- tune::last_fit()はfit()やpredict()を同時に行う
final_fit <- final_wf %>% last_fit(cell_split)
final_fit %>% print()



# 最終モデルのパフォーマンス統計量
final_fit %>% collect_metrics()


# ROCカーブ
final_fit %>%
  collect_predictions() %>%
  roc_curve(class, .pred_PS) %>%
  autoplot()


