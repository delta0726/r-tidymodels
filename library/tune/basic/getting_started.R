# Title     : Getting Started
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/11
# URL       : https://tune.tidymodels.org/articles/getting_started.html



# ＜目的＞
# - ｢グリッドサーチ｣｢ベイズ最適化｣の各チューニングのプロセスを確認する
# - モデルとレシピのチューニング方法を確認する


# ＜ポイント＞
# - tuneでは｢レシピ｣と｢モデル｣の両方に対してチューニングを行うことができる
# - ｢レシピ｣では、自然スプライン変換などチューニング要素を持つものに限定される
# - ｢モデル｣では、{parsnip}の関数で定義されるハイパーパラメータ
# - {dials}では、チューニング範囲をあらかじめ設定したオブジェクトがパラメータごとに用意されている


# ＜コア関数の構文＞

# tune_grid(
#   object,
#   resamples,
#   ...,
#   param_info = NULL,
#   grid = 10,
#   metrics = NULL,
#   control = control_grid()
# )

# tune_bayes(
#   object,
#   resamples,
#   ...,
#   iter = 10,
#   param_info = NULL,
#   metrics = NULL,
#   objective = exp_improve(), (※)
#   initial = 5, (※)
#   control = control_bayes()
# )


library(tidyverse)
library(tidymodels)
library(AmesHousing)


# データロード
ames <- make_ames()


# データ確認
ames %>% print()
ames %>% names()
ames %>% glimpse()


# プロット
# --- Y: Sales Price
# --- X: Longitude, Latitude（両方ともYと非線形な関係）
# --- スムージングすると複雑なスプラインが描かれる
ames %>%
  dplyr::select(Sale_Price, Longitude, Latitude) %>%
  tidyr::pivot_longer(cols = c(Longitude, Latitude),
                      names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, Sale_Price)) +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  facet_wrap(~ predictor, scales = "free_x")



#%% データ作成 -----------------------------------------------------------

# データ分割
set.seed(4595)
data_split <- ames %>% initial_split(strata = "Sale_Price")
ames_train <- data_split %>% training()
ames_test  <- data_split %>% testing()


# 確認
data_split %>% print()
ames_train %>% print()
ames_test %>% print()


# バリデーションデータ作成
set.seed(2453)
cv_splits <-
  ames_train %>%
    vfold_cv(v = 10, strata = "Sale_Price")


# 確認
cv_splits %>% print()



#%% レシピ作成(チューニング設定) -------------------------------------------

# レシピ作成
# --- step_ns()をチューニング
# --- ただし、1つのステップで複数のファクターをチューニングすると区別ができなくなる
ames_rec <-
  recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
    step_log(Sale_Price, base = 10) %>%
    step_ns(Longitude, Latitude, deg_free = tune())


# 確認
ames_rec %>% print()
ames_rec %>% parameters()
ames_rec %>% tidy()


# レシピ修正
# --- 列ごとにチューニングのステップを分ける
# --- 列ごとのチューニングを区別可能となる
ames_rec <-
  recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
    step_log(Sale_Price, base = 10) %>%
    step_ns(Longitude, deg_free = tune("long df")) %>%
    step_ns(Latitude,  deg_free = tune("lat df"))


# 確認
ames_rec %>% print()
ames_rec %>% parameters()
ames_rec %>% tidy()



#%% チューニング範囲の決定 -----------------------------------------------

# チューニング範囲の初期設定
# --- {dials}がデフォルトのチューニング設定を管理する
deg_free()
spline_degree()


# 参考：{dials}のチューニング・オブジェクトの中身
x <- learn_rate()
x %>% names()
x$type
x$range
x$inclusive
x$trans
x$default
x$label
x$finalize


# パラメータ更新
# --- チューニング範囲を設定
ames_param <-
  ames_rec %>%
    parameters() %>%
    update(`long df` = spline_degree(),
           `lat df` = spline_degree())

# 確認
ames_param %>% print()
ames_param %>% as_tibble()


# グリッド作成
# --- 最大エントロピー
# --- dials::grid_max_entropy()
spline_grid <- ames_param %>% grid_max_entropy(size = 10)
spline_grid %>% as_tibble()


# spline_degree()のチューニング範囲
spline_degree()
ames_param$object


# グリッド作成
# --- 順列(81パターン)
df_vals <- seq(2, 18, by = 2)
spline_grid <- expand.grid(`long df` = df_vals, `lat df` = df_vals)
spline_grid %>% as_tibble()



#%% グリッドサーチ・チューニング -----------------------------------------

# モデル定義
lm_mod <- linear_reg() %>% set_engine("lm")
lm_mod


# チューニング
# --- モデルをチューニング
# --- グリッドサーチ
# --- サンプルのデータセットが大きいので実行に時間がかかる
ames_res <-
  lm_mod %>%
    tune_grid(preprocessor = ames_rec, resamples = cv_splits, grid = spline_grid)


# 結果確認
# --- 1つのfoldに162(81パターン*2指標)の結果が表示される
ames_res %>% print()
ames_res$.metrics[[1]]


# パフォーマンス統計量の確認
# --- デフォルトではFoldごとの集計値が出力される
ames_res %>% collect_metrics()
ames_res %>% collect_metrics(summarize = FALSE)


# 結果
# --- RMSEが小さい順に表示
ames_res %>%
  collect_metrics() %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean)


# プロット作成
# --- イテレーションごとのRMSEの範囲
ames_res %>% autoplot(metric = "rmse")


# プロット作成
# --- Longitude, LatitudeともにRMSEの低かった｢16｣を用いる
# --- 元のプロットと同じようなスプラインが描かれる
ames_train %>%
  dplyr::select(Sale_Price, Longitude, Latitude) %>%
  tidyr::pivot_longer(cols = c(Longitude, Latitude),
                      names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, Sale_Price)) +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE, method = lm, formula = y ~ splines::ns(x, df = 3),  col = "red")  +
  geom_smooth(se = FALSE, method = lm, formula = y ~ splines::ns(x, df = 16)) +
  scale_y_log10() +
  facet_wrap(~ predictor, scales = "free_x")




#%% ベイズ最適化によるチューニング --------------------------------------------------


# モデル構築
# --- ハイパーパラメータ最適化
knn_mod <-
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
    set_engine("kknn") %>%
    set_mode("regression")


# レシピ作成
# --- レシピ最適化
ames_rec <-
  recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_ns(Longitude, deg_free = tune("long df")) %>%
  step_ns(Latitude,  deg_free = tune("lat df"))


# ワークフロー定義
knn_wflow <-
  workflow() %>%
    add_model(knn_mod) %>%
    add_recipe(ames_rec)


# ワークフロー更新
# --- チューニング範囲を設定
# --- spline_degreeは前回のチューニング結果から上限を決定
knn_param <-
  knn_wflow %>%
    parameters() %>%
    update(`long df` = spline_degree(c(2, 18)),
           `lat df` = spline_degree(c(2, 18)),
            neighbors = neighbors(c(3, 50)),
            weight_func = weight_func(values = c("rectangular", "inv",
                                                 "gaussian", "triangular")))


# チューニング
# --- ベイズ最適化
ctrl <- control_bayes(verbose = TRUE)
set.seed(8154)
knn_search <-
  knn_wflow %>%
    tune_bayes(resamples = cv_splits, iter = 20, param_info = knn_param,
               initial = 5, control = ctrl)


# 確認
knn_search %>% print()
knn_search$.metrics[[1]]


# パフォーマンス統計量の確認
knn_search %>% collect_metrics()
knn_search %>% collect_metrics(summarize = FALSE)


# プロット作成
# --- イテレーションごとのRMSEの範囲
knn_search %>%
  autoplot(type = "marginals", metric = "rmse")

knn_search %>%
  autoplot(type = "parameters", metric = "rmse")

knn_search %>%
  autoplot(type = "performance", metric = "rmse")


# 結果
# --- RMSEが小さい順に表示
knn_search %>%
  collect_metrics() %>%
  dplyr::filter(.metric == "rmse") %>%
  arrange(mean)

