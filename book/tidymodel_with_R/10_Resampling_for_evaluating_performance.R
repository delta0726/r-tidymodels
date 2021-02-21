#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 10 Resampling for evaluating performance
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/13
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜ポイント＞
# - モデル評価メトリックや考え方を確認する
# - メトリックで確認しながらモデルのPDCAプロセスを回すのでメトリックの選択は非常に重要


# ＜モデル評価について＞
# - モデル評価は第10章で説明するリサンプリング手法を用いるのがベストプラクティス
#   --- テストデータは1回のみ評価に使用できる
#   --- 検証データはクロスバリデーションで複数回使用することができる


# ＜目次＞
# 0 準備
# 1 モデル精度の比較における注意点
# 2 リサンプリング方法の種類
# 3 検証セットとは
# 4 ブートストラップ・サンプリング
# 5 時系列サンプリング
# 6 バリデーションによるパフォーマンス評価
# 7 予測が不十分なレコードの調査
# 8 並列処理
# 9 リサンプリングにおけるモデル保存


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(magrittr)
library(doParallel)


# データロード
data(ames)

# データ加工
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# データ分割
set.seed(123)
ames_split <- ames %>% initial_split(prob = 0.80, strata = Sale_Price)
ames_train <- ames_split %>% training()
ames_test  <-  ames_split %>% testing()


# 1 モデル精度の比較における注意点 ---------------------------------------------------

# ＜ポイント＞
# - ランダムフォレストと線形回帰モデルを比較する
#   --- ランダムフォレストは低バイアスモデル、線形回帰は高バイアスモデル
#   --- 低バイアスモデルの場合、モデルがトレーニングセットデータをほぼ記憶することがある
#   --- 訓練セットでモデル評価するのはバイアスを持ち込むことに等しい（参考程度に見るくらいは可）


# モデル1： ランダムフォレスト *************************

# モデル構築
rf_model <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# ワークフロー設定
rf_wflow <-
  workflow() %>%
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
      Latitude + Longitude) %>%
  add_model(rf_model)

# モデル学習
rf_fit <-
  rf_wflow %>%
    fit(data = ames_train)


# モデル2： 線形回帰 *************************

# モデル構築
lm_model <-
  linear_reg() %>%
  set_engine("lm")

# ワークフロー設定
lm_wflow <-
  workflow() %>%
  add_model(lm_model) %>%
  add_formula(Sale_Price ~ Longitude + Latitude)

# 学習
lm_fit <-
  lm_wflow %>%
    fit(data = ames_train)


# 比較 *************************

# 関数定義
# --- メトリックスにモデル情報を追加して出力
estimate_perf <- function(model, dat) {
  # Capture the names of the objects used
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)

  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)

  model %>%
    predict(dat) %>%
    bind_cols(dat %>% select(Sale_Price)) %>%
    reg_metrics(Sale_Price, .pred) %>%
    select(-.estimator) %>%
    mutate(object = obj_name, data = data_name)
}

# メトリック出力
# --- 訓練データ
# --- ランダムフォレストが圧倒的に低い
rf_fit %>% estimate_perf(ames_train)
lm_fit %>% estimate_perf(ames_train)

# メトリック出力
# --- 検証データ
# --- それぞれ水準は高まるが、ランダムフォレストの優位性は変わらない
rf_fit %>% estimate_perf(ames_test)
lm_fit %>% estimate_perf(ames_test)


# 2 リサンプリング方法の種類 -------------------------------------------------------

# ＜ポイント＞
# - 訓練データを分割してFoldを作成し、テストデータとは相互排他的になっている
# - クロスバリデーションの考え方を書籍で確認（ランダムに色分けしているのがポイント）


# V-Fold クロスバリデーション *************************************

# クロスバリデーション
# --- 訓練データを分割している（テストデータは含まない）
set.seed(55)
ames_folds <- ames_train %>% vfold_cv(v = 10)
ames_folds %>% print()

# Foldの中を確認
ames_folds$splits[[1]] %>% analysis() %>% dim()
ames_folds$splits[[1]] %>% assessment() %>% dim()


# V-Fold 繰り返しあり・クロスバリデーション ****************************

# クロスバリデーション
# --- リピートでさらにリサンプリングを増やしている
ames_train %>%
  vfold_cv(v = 5, repeats = 5) %>%
  print(n = nrow(.))


# モンテカルロ・クロスバリデーション ************************************

# ＜ポイント＞
# - V分割交差検定と同様に、一定の割合のデータを評価セットに割り当てる
#   --- 違いは、MCCVの場合、データのこの比率が毎回ランダムに選択されること

# モンテカルロ・クロスバリデーション
ames_train %>%
  mc_cv(prop = 9/10, times = 20)



# 3 検証セットとは -------------------------------------------------------------------

#
set.seed(12)
val_set <- ames_train %>% validation_split(prop = 3/4)
val_set

# Foldの中を確認
# --- ｢訓練データ｣と｢評価データ｣に分かれている
val_set$splits[[1]] %>% analysis() %>% dim()
val_set$splits[[1]] %>% assessment() %>% dim()


# 4 ブートストラップ・サンプリング ----------------------------------------------------

# ＜ポイント＞
# - ブートストラップサンプルとは、復元抽出サンプリングを行う
#   --- 各データポイントには、少なくとも1回はトレーニングセットに含まれる可能性が63.2％となる
#   --- 評価セットには、分析セット用に選択されなかったすべてのトレーニングセットサンプルが含まれる
# - ブートストラップサンプルは、分散が非常に小さい

# ＜問題点＞
# - 悲観的バイアスがあるパフォーマンス推定値を生成する傾向にある
#   --- モデルの真の精度が90％の場合、ブートストラップは値を90％未満と推定する傾向がある
#   --- バイアス量は経験的にこのトロールすることは出来ず、更にメトリックによって異なる


# ブートストラップ・サンプリング
# --- 5回のイテレーション
ames_train %>% bootstraps(times = 5)


# 5 時系列サンプリング ----------------------------------------------------

# ＜ポイント＞
# - データに強い時間成分がある場合はランダムサンプリングより、ローリングサンプリングの方が適切な場合がある
# - cumulative引数をTRUEにするとExpandとなり、訓練データがどんどん多くなる


# データ作成
time_slices <-
  tibble(x = 1:365) %>%
    rolling_origin(initial = 6 * 30, assess = 30, skip = 29, cumulative = FALSE)

# 確認
time_slices %>% print()

# スライスの確認
time_slices$splits[1][[1]] %>% analysis() %>% use_series(x)
time_slices$splits[1][[1]] %>% assessment() %>% use_series(x)
time_slices$splits[2][[1]] %>% analysis() %>% use_series(x)
time_slices$splits[2][[1]] %>% assessment() %>% use_series(x)


# 関数定義
# --- 開始/終了地点を出力
data_range <- function(x) {
  summarize(x, first = min(x), last = max(x))
}

# 訓練データ
time_slices$splits %>%
  map_dfr(~ analysis(.x) %>% data_range())

# 評価データ
time_slices$splits %>%
  map_dfr(~ assessment(.x) %>% data_range())


# 6 バリデーションによるパフォーマンス評価 ----------------------------------------------

# ＜ポイント＞
# - リサンプリング手法とモデリングプロセス(前処理/モデル構築)を用いてモデル精度を評価する
# - サブサンプルごとに学習/予測を行うので基本的に重い操作
#   --- 並列処理が可能

# ＜実行パターン＞
# - レシピ/ワークフローを指定することでリサンプリング予測とプロセスを統合することが可能
# model_spec %>% fit_resamples(formula,  resamples, ...)
# model_spec %>% fit_resamples(recipe,   resamples, ...)
# workflow   %>% fit_resamples(          resamples, ...)


# リサンプリング予測
# --- 10Fold-CrossValidation
# --- サブサンプルごとに10回実行する
set.seed(130)
rf_res <-
  rf_wflow %>%
    fit_resamples(resamples = ames_folds, control = control_resamples(save_pred = TRUE))

# 確認
rf_res %>% print()

# メトリックの抽出
rf_res %>% collect_metrics()

# 予測値の抽出
assess_res <- rf_res %>% collect_predictions()
assess_res %>% print()

# プロット作成
# --- 正解値 vs 予測値
# --- リサンプリングごとに大きな違いが生じていないことを確認
assess_res %>%
  ggplot(aes(x = Sale_Price, y = .pred, fill = id, color = id)) +
  geom_point(alpha = .15) +
  geom_abline(col = "red") +
  coord_obs_pred() +
  ylab("Predicted")


# 7 予測が不十分なレコードの調査 -----------------------------------------------------

# データ抽出
# --- 極端に残差の大きいレコード（プロットで確認）
over_predicted <-
  assess_res %>%
    mutate(residual = Sale_Price - .pred) %>%
    arrange(desc(abs(residual))) %>%
    slice(1)

# 確認
over_predicted %>% print()

# レコード確認
ames_train %>%
  slice(over_predicted$.row) %>%
  select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)

# データ分割
# --- 評価データの分割（訓練データではない）
set.seed(12)
val_set <- ames_train %>% validation_split(prop = 3/4)
val_set %>% print()

# リサンプリング予測
val_res <- rf_wflow %>% fit_resamples(resamples = val_set)
val_res %>% print()

# メトリックの取得
val_res %>% collect_metrics()


# 8 並列処理 ----------------------------------------------------------------------

# ＜ポイント＞
# - リサンプリング中に作成されたモデルは互いに独立している
# -


# コア数の確認
# --- 以下の2つの値の違いは、コンピューターのプロセッサーに関連している
# --- 多くのIntelプロセッサは、物理コアごとに2つの仮想コアを作成するハイパースレッディングを使用
parallel::detectCores(logical = FALSE)
parallel::detectCores(logical = TRUE)


# バックエンドの登録
# --- シーケンシャルパラレルバックエンドをforeachパッケージに明示的に登録
# --- ％dopar％関数が呼び出さる際に警告メッセージが発行されなくなる
registerDoSEQ()

# クラスタの作成/登録
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

# 処理の実行
# Now run fit_resamples()`...

# クラスタの終了
stopCluster(cl)


# 9 リサンプリングにおけるモデル保存 ----------------------------------------------------

# ＜ポイント＞
# - リサンプリング中に作成されたモデルは保持されない
#   --- パフォーマンス評価のみを目的としてトレーニングされているたため
#   --- control_resamples()のextract引数を使うと一部を保存することが可能

# レシピ作成
ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Latitude, Longitude, deg_free = 20)

# ワークフロー設定
lm_wflow <-
  workflow() %>%
  add_recipe(ames_rec) %>%
  add_model(linear_reg() %>% set_engine("lm"))

# 学習
lm_fit <-
  lm_wflow %>%
    fit(data = ames_train)

# レシピの抽出
lm_fit %>% pull_workflow_prepped_recipe()

# 関数定義
# --- 学習結果の抽出
get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

# モデル抽出
lm_res <- lm_wflow %>%  fit_resamples(resamples = ames_folds,
                                      control = control_resamples(extract = get_model))
# 確認
lm_res %>% print()

# 確認
lm_res$.extracts[[1]]
lm_res$.extracts[[1]][[1]]
