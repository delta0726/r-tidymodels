# ***********************************************************************************************
# Title   : Rユーザーのためのtidymodels実践入門
# Chapter : 0 本書の特徴
# Date    : 2023/3/26
# Page    : Ⅳ - Ⅹ
# URL     : https://github.com/ghmagazine/tidymodels_book
# ***********************************************************************************************


# ＜概要＞
# - {tidymodels}を使った機械学習のフローを確認する


# ＜目次＞
# 0 準備
# 1 データセットの確認
# 2 データ準備
# 3 特徴量エンジニアリング
# 4 モデル構築
# 5 ハイパーパラメータのチューニング
# 6 最終モデルで学習
# 7 モデル評価


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidymodels)


# 1 本書で主に使用するデータセット --------------------------------------------------

# Ames Housigデータの呼び出し
data(ames, package = "modeldata")

# データ確認
ames %>% print()
ames %>% colnames()


# 2 データ準備 --------------------------------------------------------------------

# データ分割
# --- 訓練データ/検証データ
set.seed(71)
split_ames_df <- ames %>% initial_split(strata = "Sale_Price")
ames_train <- split_ames_df %>% training()
ames_test  <- split_ames_df %>% testing()

# データ分割
# --- 訓練データをクロスバリデーション用に分割
ames_cv_splits <- ames_train %>% vfold_cv(strata = "Sale_Price", v = 10)


# 3 特徴量エンジニアリング ---------------------------------------------------------

# レシピの作成
ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_log(Sale_Price, base = 10) %>%
    step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>%
    step_other(Neighborhood, threshold = .1)  %>%
    step_zv(recipes::all_predictors()) %>%
    prep()

# 前処理の適用
ames_train_baked <- ames_rec %>% bake(new_data = ames_train)
ames_test_baked  <- ames_rec %>% bake(new_data = ames_test)


# 4 モデル構築 -----------------------------------------------------------------

# ワークフローの設定
# --- モデルのハイパーパラメータはチューニングするように設定
ames_rf_cv <-
  workflow() %>%
    add_model(rand_forest(mtry = tune::tune(), trees = tune::tune()) %>%
      set_engine("ranger", num.threads = parallel::detectCores()) %>%
      set_mode("regression")) %>%
      add_formula(Sale_Price ~ .)


# 5 ハイパーパラメータのチューニング ---------------------------------------------

# グリッドサーチのパラメータ範囲設定
# --- mtryは特徴量の列数をインプットする必要がある
rf_params <-
  list(trees(),
       mtry() %>% finalize(ames_train_baked %>% select(-Sale_Price))) %>%
    parameters()

# パラメータの探索範囲を指定
set.seed(71)
rf_grid_range <- rf_params %>% grid_max_entropy(size = 10)

# グリッドサーチの実行
ames_rf_grid <-
  ames_rf_cv %>%
    tune_grid(ames_cv_splits,
              grid = rf_grid_range,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(yardstick:::rmse))

# プロット作成
ames_rf_grid %>% autoplot()

# 最適パラメータの選択
ames_rf_grid_best <-  ames_rf_grid %>% show_best()

# 最適パラメータの確認
# --- 上位の組み合わせが列挙されている（1行目が最良パターン）
ames_rf_grid_best %>% print()


# 6 最終モデルで学習 -------------------------------------------------

# モデル更新
# --- チューニングパラメータを設定
ames_rf_model_best <-
  rand_forest(trees = ames_rf_grid_best$trees[1],
              mtry = ames_rf_grid_best$mtry[1]) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# ワークフローの更新
# --- 以前作成したワークフローのモデルのみを更新
ames_rf_cv_last <-
  ames_rf_cv %>%
    update_model(ames_rf_model_best)

# モデル学習
# --- 更新したワークフローで訓練データ全体にモデル適用
ames_rf_last_fit <-
  ames_rf_cv_last %>%
    last_fit(split_ames_df)


# 7 モデル評価 -------------------------------------------------------

# 最終的なモデルの精度を算出
ames_rf_last_fit %>% collect_metrics()

