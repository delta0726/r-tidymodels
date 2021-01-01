# Title     : fit_resamples
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/06
# URL       : https://tune.tidymodels.org/reference/fit_resamples.html


# ＜ポイント＞
# - リサンプリングされたデータセットをFoldごとのFittingしてパフォーマンス指標を算出する
#    --- {tune}の関数だが、チューニング自体は行わない
#    --- 代わりに、リサンプリングのfoldごとにフィッティング結果を得る
# - 予測値はテストデータだけで出力される（｢予測｣ではなく｢評価｣を目的としたもの）


# ＜構文＞
# fit_resamples(
#   workflow,
#   resamples,
#   ...,
#   metrics = NULL,
#   control = control_resamples()
# )


# ＜コントロール変数＞
# control_resamples(
#   verbose = FALSE,
#   allow_par = TRUE,
#   extract = NULL,
#   save_pred = FALSE,
#   pkgs = NULL
# )


# ＜パフォーマンス指標＞
# - metric引数にyardstick::metric_set()で計算したいメトリックを一括指定する
# - 指定されない場合は、回帰の場合はRMSEとR2、分類の場合はROC-AUCとAccuracyを算出


# ＜予測値＞
# - save_pred引数をTRUEにすると、Foldごとの予測値を得ることができる
# - 予測値はFoldごとの｢テストデータ｣のみ出力される
#   --- ｢訓練データ｣でモデリング
# - collect_prediction()で結果をデータフレーム形式に変換することができる


# ＜チューニングとの相違点＞
# - tune_*()はFoldごとにグリッドを作成してチューニングを行う
# - fit_resample()はFoldごとに｢訓練データ｣でフィッティング、｢テストデータ｣で予測値を計算
#   --- 基本的にFoldごとのメトリックを取得することが目的
#   --- リサンプリングによるモデルの安定性をメトリックで確認（propを下げるほうが目的にかなう？）



library(tidyverse)
library(tidymodels)


# 乱数シードの設定
set.seed(6735)



#%% 前処理 ---------------------------------------

# リサンプリングデータの作成
# --- クロスバリデーション
folds <- mtcars %>% vfold_cv(v = 5)
folds %>% print()


# レシピ作成
spline_rec_2 <-
  recipe(mpg ~ ., data = mtcars) %>%
    step_ns(disp) %>%
    step_ns(wt)


# モデリング
lin_mod <-
  linear_reg() %>%
    set_engine("lm")


# コントロール設定
# --- Fittingした際に｢.predictions｣が出力される
# --- データセットが大きい場合は一層サイズが大きくなるので注意
control <- control_resamples(save_pred = TRUE)



#%% 学習 ---------------------------------------

# モデル学習
spline_res_2 <-
  lin_mod %>%
    fit_resamples(spline_rec_2, folds, control = control)


# 確認
spline_res_2 %>% print()
spline_res_2 %>% names()


# 要素へのアクセス
# --- 予測値はテストデータの分しか出力されない（しかも、デフォルト設定では出力されない）
#     - 予測値の取得自体は目的ではない
# --- メトリックでモデルの安定性を評価
spline_res_2$.metrics[[1]]
spline_res_2$.notes[[1]]
spline_res_2$.predictions[[1]]


# 最良モデルの抽出
spline_res_2 %>% show_best(metric = "rmse")

