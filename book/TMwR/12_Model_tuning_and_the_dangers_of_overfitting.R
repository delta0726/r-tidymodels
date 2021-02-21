#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 12 Model tuning and the dangers of overfitting
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/13
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜概要＞
# - tidymodelsにおけるハイパーパラメータのチューニングの仕組みを知る
#   --- {tune}でチューニング箇所を指定、{dials}で初期パラメータを指定
#   --- パラメータ範囲は手動調整/データ調整が可能


# ＜目次＞
# 0 準備
# 1 さまざまなパラメータ調整
# 2 tidymodelsにおけるチューニングパラメータ
# 3 dialsによるパラメータレンジの指定
# 4 エンジン固有のパラメータチューニング


# 0 準備 -------------------------------------------------------------------------

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
ames_test  <- ames_split %>% testing()


# 1 さまざまなパラメータ調整 -----------------------------------------------------------------

# 関数定義
# --- 尤度の取得
llhood <- function(...) {
  logistic_reg() %>%
    set_engine("glm", ...) %>%
    fit(Class ~ ., data = training_set) %>%
    glance() %>%
    select(logLik)
}

bind_rows(
  llhood(),
  llhood(family = binomial(link = "probit")),
  llhood(family = binomial(link = "cloglog"))
) %>%
  mutate(link = c("logit", "probit", "c-log-log"))  %>%
  arrange(desc(logLik))

set.seed(1292)
rs <- vfold_cv(training_set, repeats = 10)

# Return the individual resampled performance estimates:
lloss <- function(...) {
  perf_meas <- metric_set(roc_auc, mn_log_loss)

  logistic_reg() %>%
    set_engine("glm", ...) %>%
    fit_resamples(Class ~ A + B, rs, metrics = perf_meas) %>%
    collect_metrics(summarize = FALSE) %>%
    select(id, id2, .metric, .estimate)
}

resampled_res <-
  bind_rows(
    lloss()                                    %>% mutate(model = "logitistic"),
    lloss(family = binomial(link = "probit"))  %>% mutate(model = "probit"),
    lloss(family = binomial(link = "cloglog")) %>% mutate(model = "c-log-log")
  ) %>%
  # Convert log-loss to log-likelihood:
  mutate(.estimate = ifelse(.metric == "mn_log_loss", -.estimate, .estimate)) %>%
  group_by(model, .metric) %>%
  summarize(
    mean = mean(.estimate, na.rm = TRUE),
    std_err = sd(.estimate, na.rm = TRUE) / sum(!is.na(.estimate)),
    .groups = "drop"
  )

resampled_res %>%
  filter(.metric == "mn_log_loss") %>%
  ggplot(aes(x = mean, y = model)) +
  geom_point() +
  geom_errorbar(aes(xmin = mean - 1.96 * std_err, xmax = mean + 1.96 * std_err),
                width = .1) +
  labs(y = NULL, x = "log-likelihood")




# 2 tidymodelsにおけるチューニングパラメータ ---------------------------------------------

# ランダムフォレスト
# --- 2種類のパラメータがある
rand_forest(trees = 2000, min_n = 10) %>%                    # 共通パラメータ
  set_engine("ranger", regularization.factor = 0.5)   # エンジン固有パラメータ

# 単層ニューラルネットワーク
# --- tune::tune()を用いてチューニング個所を指定
# --- hidden_unitsをチューニング
neural_net_spec <-
  mlp(hidden_units = tune()) %>%
  set_engine("keras")

# 確認
neural_net_spec %>% print()
neural_net_spec %>% parameters()

# レシピ作成
# --- チューニングあり
# --- 自然スプラインをチューニング
ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train)  %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = tune()) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Longitude, deg_free = tune("longitude df")) %>%
  step_ns(Latitude,  deg_free = tune("latitude df"))

# 確認
ames_rec %>% parameters()

# ワークフロー設定
# --- ｢モデル｣と｢レシピ｣の両方にチューニングあり
wflow_param <-
  workflow() %>%
    add_recipe(ames_rec) %>%
    add_model(neural_net_spec)

# 確認
wflow_param %>% print()
wflow_param %>% parameters()


# 3 dialsによるパラメータレンジの指定 ----------------------------------------------

# パラメータごとのデフォルト設定 ************************************

# Hidden Units (quantitative)
# --- Range: [1, 10]
hidden_units()

# Threshold (quantitative)
# --- Range: [0, 1]
threshold()

# Piecewise Polynomial Degree (quantitative)
# --- Range: [1, 10]
spline_degree()


# ワークフローからの抽出/更新 ************************************

# パラメータ抽出
# --- Threshold (quantitative)
# --- Range: [0, 0.1]
wflow_param %>% pull_dials_object("threshold")

# パラメータ更新
ames_rec %>%
  parameters() %>%
  update(threshold = threshold(c(0.8, 1.0))) %>%
  pull_dials_object("threshold")


# 事前では未知のパラメータ ***************************************

# ＜ポイント＞
# - パラメータにはデータのレコード数や特徴量の数で適正値が変化するものがある
#   --- 初期状態では｢?｣として表示される
#   --- 事後的に手動又はデータを適用させて更新する

# モデル構築
# --- mtryをチューニング
rf_spec <-
  rand_forest(mtry = tune()) %>%
  set_engine("ranger", regularization.factor = tune("regularization"))

# ハイパーパラメータの確認
# --- mtryの上限が｢?｣で表示される
# --- パラメータ更新を促すメッセージも出る（Model parameters needing finalization）
rf_param <- rf_spec %>% parameters()
rf_param %>% print()
rf_param %>% pull_dials_object("mtry")


# 手動更新 ***************************************************

# ハイパーパラメータの更新
# --- 手動で更新
rf_param %>%
  update(mtry = mtry(c(1, 70))) %>%
  pull_dials_object("mtry")


# データに基づいて更新 ******************************************

# レシピ作成
# --- 訓練データの情報を持つ
pca_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_normalize(contains("SF")) %>%
  step_pca(contains("SF"), threshold = .95)

# ワークフロー設定
rf_wflw <-
  workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(pca_rec)

# パラメータ更新
rf_wflw %>%
  parameters() %>%
  finalize(ames_train) %>%
  pull_dials_object("mtry")


# 4 エンジン固有のパラメータチューニング ----------------------------------------------

# ＜ポイント＞
# - {dials}ではエンジン固有パラメータもチューニング対象に提供されている
#   --- モデルへの影響が強いもののみ

# {ranger}の正則化ファクター
# --- Gain Penalization (quantitative)
# ---  Range: [0, 1]
regularization_factor()

# {gmlnet}の正則化ペナルティ
# --- Transformer:  log-10 （対数表示となっている）
# ---  Range (transformed scale): [-10, 0]
penalty()


# 対数表記のイメージ
# --- 1000個の数値を指定レンジでランダムに発生
# --- データ範囲を確認
penalty(c(-1, 0)) %>%
  value_sample(1000) %>%
  summary()

# 範囲の確認
penalty(c(0.1, 1.0)) %>%
  value_sample(1000) %>%
  summary()

