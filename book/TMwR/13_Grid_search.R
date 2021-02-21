#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 13 Grid search
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/20
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜概要＞
# - 個別のチューニングパラメータが明らかになったら、グリッドサーチで組み合わせを評価する
# - チューニングは計算コストの高い処理なので、高速化の手段も併せて検討する


# ＜目次＞
# 0 準備
# 1 チューニングモデルの構築
# 2 グリッドの作成
# 3 プロットによるグリッドの確認
# 4 チューニングの準備
# 5 チューニングプロセス-1
# 6 チューニングプロセス-2
# 7 ワークフローでの最終モデルへの更新
# 8 サブモデル最適化
# 9 並列処理
# 10 レーシングメソッド


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(ggforce)
library(finetune)


# データロード
data(cells)

# データ加工
cells <- cells %>% select(-case)

# データ確認
cells %>% print()
cells %>% glimpse()


# 1 チューニングモデルの構築 ----------------------------------------------------------

# ＜ポイント＞
# - 単層ニューラルネットワークをグリッドサーチでチューニングする
#   --- tune::tune()で対象パラメータを指定
#   --- {dial}の調整パラメータの設定が適用される


# モデル構築
# --- 単層ニューラルネットワーク
# --- 3項目をチューニング
mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet", trace = 0) %>%
  set_mode("classification")

# パラメータ取得
mlp_param <- mlp_spec %>% parameters()
mlp_param %>% print()


# パラメータの調整レンジ
# hidden_units
# --- Hidden Units (quantitative)
# --- Range: [1, 10]
mlp_param %>% pull_dials_object("hidden_units")

# パラメータの調整レンジ
# penalty
# --- Amount of Regularization (quantitative)
# --- Transformer:  log-10
# --- Range (transformed scale): [-10, 0]
mlp_param %>% pull_dials_object("penalty")

# パラメータの調整レンジ
# epochs
# --- Epochs (quantitative)
# --- Range: [10, 1000]
mlp_param %>% pull_dials_object("epochs")


# 2 グリッドの作成 ---------------------------------------------------------------------

# ＜ポイント＞
# - tune::grid_*でチューニング用のグリッドを作成する
#   --- チューニング回数をコントロールするための引数が含まれる
#   --- tidyr::crossing()でもパターン作成は可能
# - グリッドサイズが大きくなると計算コストも高まる
#   --- ランダムサーチや実験計画法アルゴリズムで精度と効率性を高めながらサイズを減らすことが可能
#   --- ただし、グリッドサーチはパラメータとモデル精度の関係を理解するのに役立つ


# グリッドの作成
# --- tidyr::crossing()で全パターン作成
crossing(hidden_units = 1:3,
         penalty = c(0.0, 0.1),
         epochs = c(100, 200))

# グリッドの作成
# --- レベル数を指定することが可能
mlp_param %>%
  grid_regular(levels = 2)

# グリッドの作成
# --- パラメータごとにレベル数を指定することが可能
mlp_param %>%
  grid_regular(levels = c(hidden_units = 3, penalty = 2, epochs = 2))

# グリッドの作成
# --- ランダムサーチ
set.seed(10)
mlp_param %>%
  grid_random(size = 1000) %>%
  summary()


# 3 プロットによるグリッドの確認 ------------------------------------------------

# ＜ポイント＞
# - original引数をFALSEに指定すると対数表記のまま扱うことができる
# - ランダムサーチはパターンによって空白地帯が発生、その位置もランダムに異なる
# - ラテン方式はパラメータごとの探索位置が整合的となる


# プロット作成
# --- グリッドサーチ
set.seed(200)
mlp_param %>%
    grid_regular(levels = 5, original = FALSE) %>%
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    geom_point() +
    geom_blank() +
    facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) +
    labs(title = "Grid Search with 125 candidates")

# プロット作成
# --- ランダムサーチ
set.seed(200)
mlp_param %>%
  grid_random(size = 15, original = FALSE) %>%
  ggplot(aes(x = .panel_x, y = .panel_y)) +
    geom_point() +
    geom_blank() +
    facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) +
    labs(title = "Random design with 15 candidates")

# プロット作成
# --- ラテン方式
set.seed(200)
mlp_param %>%
  grid_latin_hypercube(size = 15, original = FALSE) %>%
  ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point() +
  geom_blank() +
  facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) +
  labs(title = "Latin Hypercube design with 15 candidates")

# プロット作成
# --- 最大エントロピー
set.seed(200)
mlp_param %>%
  grid_max_entropy(size = 15, original = FALSE) %>%
  ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point() +
  geom_blank() +
  facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) +
  labs(title = "Max Entropy design with 15 candidates")

# 参考：プロット作成
# --- グリッドサーチ
# --- original引数をTRUEにする
# --- 対数ではなく自然数でグリッドが作成される
set.seed(200)
mlp_param %>%
    grid_regular(levels = 5, original = FALSE) %>%
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    geom_point() +
    geom_blank() +
    facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) +
    labs(title = "Grid Search with 125 candidates")


# 4 チューニングの準備 -----------------------------------------------------

# データ準備（再掲）
data(cell)
cells <- cells %>% select(-case)

# バリデーションセットの作成
set.seed(33)
cell_folds <- cells %>% vfold_cv()
cell_folds %>% print()

# レシピの作成
# --- pcaのnum_compをチューニング
# --- 特徴量には相関が高いものが含まれるのでPCAで次元削減
mlp_rec <-
  recipe(class ~ ., data = cells) %>%
    step_YeoJohnson(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors(), num_comp = tune()) %>%
    step_normalize(all_predictors())

# モデル構築（再掲）
# --- 単層ニューラルネットワーク
# --- 3項目をチューニング
mlp_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet", trace = 0) %>%
  set_mode("classification")

# ワークフロー設定
mlp_wflow <-
  workflow() %>%
    add_model(mlp_spec) %>%
    add_recipe(mlp_rec)

# パラメータ範囲の確認
mlp_wflow %>% parameters() %>% pull_dials_object("epochs")
mlp_wflow %>% parameters() %>% pull_dials_object("num_comp")

# パラメータ更新
mlp_param <-
  mlp_wflow %>%
    parameters() %>%
    update(epochs = epochs(c(50, 200)),
           num_comp = num_comp(c(0, 40)))

# パラメータ範囲の確認
mlp_param %>% pull_dials_object("epochs")
mlp_param %>% pull_dials_object("num_comp")

# 評価メトリック指定
roc_res <- metric_set(roc_auc)


# 5 チューニングプロセス-1 ------------------------------------------------

# チューニング
# --- グリッドサーチ
set.seed(99)
mlp_reg_tune <-
  mlp_wflow %>%
  tune_grid(cell_folds,
            grid = mlp_param %>% grid_regular(levels = 3),
            metrics = roc_res)

# 確認
mlp_reg_tune

# プロット作成
mlp_reg_tune %>%
  autoplot() +
  theme(legend.position = "top")

# 最良パラメータ
mlp_reg_tune %>%
  show_best() %>%
  select(-.estimator)


# 6 チューニングプロセス-2 ------------------------------------------------

# グリッドで再チューニング
set.seed(99)
mlp_sfd_tune <-
  mlp_wflow %>%
  tune_grid(cell_folds,
            grid = 20,
            param_info = mlp_param,
            metrics = roc_res)

# 確認
mlp_sfd_tune %>% print()

# プロット作成
mlp_sfd_tune %>%
  autoplot() +
  theme(legend.position = "top")

# 最良パラメータ
mlp_sfd_tune %>%
  show_best() %>%
  select(-.estimator)

# 最終モデルの作成
mlp_reg_tune %>%
  select_best(metric = "roc_auc")


# 7 ワークフローでの最終モデルへの更新 ------------------------------------------------

# パラメータ格納
logistic_param <-
  tibble(num_comp = 0,
         epochs = 125,
         hidden_units = 1,
         penalty = 1)

# 最終モデルへの更新
final_mlp_wflow <-
  mlp_wflow %>%
    finalize_workflow(logistic_param)

# 確認
final_mlp_wflow

# 学習
final_mlp_fit <-
  final_mlp_wflow %>%
    fit(cells)


# 8 サブモデル最適化 ---------------------------------------------------------------------

c5_spec <-
  boost_tree(trees = tune()) %>%
    set_engine("C5.0") %>%
    set_mode("classification")

set.seed(2)
c5_spec %>%
  tune_grid(
    class ~ .,
    resamples = cell_folds,
    grid = data.frame(trees = 1:100),
    metrics = roc_res
  )



# 9 並列処理 ---------------------------------------------------------------------

for (rs in resamples) {
  # Create analysis and assessment sets
  # Preprocess data (e.g. formula or recipe)
  for (mod in configurations) {
    # Fit model {mod} to the {rs} analysis set
    # Predict the {rs} assessment set
  }
}


all_tasks <- crossing(resamples, configurations)

for (iter in all_tasks) {
  # Create analysis and assessment sets for {iter}
  # Preprocess data (e.g. formula or recipe)
  # Fit model {iter} to the {iter} analysis set
  # Predict the {iter} assessment set
}


# 10 レーシングメソッド -------------------------------------------------------------

# ＜問題提起＞
# - グリッドサーチは全てのパターンリサンプルで学習させる必要があるので時間がかかる
#   --- 中間分析として本当にひどいパラメーター候補を排除できれば時間とリソースの無駄を排除できる


set.seed(99)
mlp_sfd_race <-
  mlp_wflow %>%
    tune_race_anova(cell_folds,
                    grid = 20,
                    param_info = mlp_param,
                    metrics = roc_res,
                    control = control_race(verbose_elim = TRUE))


# 最良モデル
# --- トップ10
mlp_sfd_race %>%
  show_best(n = 10)