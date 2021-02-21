#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 9 Judging model effectiveness
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/15
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
# 1 回帰メトリック
# 2 二値分類メトリック
# 3 クラス確率メトリック
# 4 マルチクラスのメトリック
# 5 マルチクラス指標の計算証明


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)

# データロード
data(ames)

# データ加工
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# データ分割
set.seed(123)
ames_split <- ames %>% initial_split(prob = 0.80, strata = Sale_Price)
ames_train <- ames_split %>% training()
ames_test  <-  ames_split %>% testing()

# レシピ作成
ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Latitude, Longitude, deg_free = 20)

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
lm_fit <- lm_wflow %>% fit(ames_train)


# 1 回帰メトリック ----------------------------------------------------------

# 予測
ames_test_res <-
  lm_fit %>%
    predict(new_data = ames_test %>% select(-Sale_Price))

# 確認
ames_test_res %>% print

# データ結合
# --- 予測データと正解データを並べる
ames_test_res <-
  ames_test_res %>%
    bind_cols(ames_test %>% select(Sale_Price))

# 確認
ames_test_res %>% print()

# プロット作成
# --- 正解 vs 予測
ames_test_res %>%
  ggplot(aes(x = Sale_Price, y = .pred)) +
    geom_abline(lty = 2) +
    geom_point(alpha = 0.5) +
    labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
    coord_obs_pred()

# メトリックの出力
# --- RMSE
ames_test_res %>%
  rmse(truth = Sale_Price, estimate = .pred)

# 複数メトリックの出力
# --- 複数メトリックを出力する関数を新たに定義して実行
ames_metrics <- metric_set(rmse, rsq, mae)
ames_test_res %>% ames_metrics(truth = Sale_Price, estimate = .pred)
ames_metrics %>% class()


# 2 二値分類メトリック --------------------------------------------------------

# ＜ポイント＞
# - 分類結果に対してメトリックで評価しており、クラス確率は反映されない


# データ出力
data(two_class_example)

# データ確認
two_class_example %>% as_tibble() %>% print()
two_class_example %>% glimpse()

# 混合行列
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted)

# メトリック出力
# --- Accuracy
two_class_example %>%
  accuracy(truth = truth, estimate = predicted)

# メトリック出力
# --- Matthews correlation coefficient:
two_class_example %>%
  mcc(truth = truth, estimate = predicted)

# メトリック出力
# --- F1 metric
# --- 書籍ではevent_level引数について議論している（必要に応じて確認）
two_class_example %>%
  f_meas(truth = truth, estimate = predicted)


# 3 クラス確率メトリック --------------------------------------------------------

# ROCカーブ
two_class_example %>%
  roc_curve(truth = truth, Class1)

# AUCカーブ
two_class_example %>%
  roc_auc(truth = truth, Class1)

# プロット
two_class_curve %>% autoplot()


# 4 マルチクラスのメトリック --------------------------------------------------------

# ＜ポイント＞
# - 3つ以上のクラスの場合に｢.estimator｣が追加される
#   --- macro： クラスごとにメトリックを計算して調和平均
#   --- macro-weighted： クラスごとに名トリックを計算してサンプル数で加重
#   --- micro-averaging： 全てを1/0に直して単一メトリックを算出

# データロード
data(hpc_cv)

# データ確認
hpc_cv %>% as_tibble() %>% print()
hpc_cv %>% glimpse()
hpc_cv$obs %>% levels()

# Accuracy
# --- .estimator列が追加されている（3つ以上のクラスの場合に出現）
hpc_cv %>%
  accuracy(truth = obs, estimate = pred)

# Matthews correlation coefficient
hpc_cv %>%
  mcc(truth = obs, estimate = pred)


# 5 マルチクラス指標の計算証明 --------------------------------------------------------

# クラスウエイトの計算
# --- クラスごとのカウント
# --- カウントの割合
class_totals <-
  count(hpc_cv, obs, name = "totals") %>%
  mutate(class_wts = totals / sum(totals))

# 確認
class_totals %>% print()

# カテゴリのカウント
# --- 全パターン
cell_counts <-
  hpc_cv %>%
    group_by(obs, pred) %>%
    count() %>%
    ungroup()

# sensの算出
one_versus_all <-
  cell_counts %>%
    filter(obs == pred) %>%
    full_join(class_totals, by = "obs") %>%
    mutate(sens = n / totals)


one_versus_all %>% print()
#> # A tibble: 4 x 6
#>   obs   pred      n totals class_wts  sens
#>   <fct> <fct> <int>  <int>     <dbl> <dbl>
#> 1 VF    VF     1620   1769    0.510  0.916
#> 2 F     F       647   1078    0.311  0.600
#> 3 M     M        79    412    0.119  0.192
#> 4 L     L       111    208    0.0600 0.534

# Three different estimates:
one_versus_all %>%
  summarize(
    macro = mean(sens),
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )


hpc_cv %>%
  sensitivity(obs, pred, estimator = "macro")

hpc_cv %>%
  sensitivity( obs, pred, estimator = "macro_weighted")


hpc_cv %>%
  sensitivity( obs, pred, estimator = "micro")

hpc_cv %>%
  roc_auc( obs, VF, F, M, L)

hpc_cv %>%
  roc_auc( obs, VF, F, M, L, estimator = "macro_weighted")


hpc_cv %>%
  group_by(Resample) %>%
  accuracy(obs, pred)



# Four 1-vs-all ROC curves for each fold
hpc_cv %>%
  group_by(Resample) %>%
  roc_curve(obs, VF, F, M, L) %>%
  autoplot()

