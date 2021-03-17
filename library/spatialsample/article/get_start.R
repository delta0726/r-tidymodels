# **********************************************************************************
# Title     : Get Started
# Objective : TODO
# Created by: Owner
# Created on: 2021/03/18
# URL       : https://spatialsample.tidymodels.org/articles/spatialsample.html
# **********************************************************************************


# ＜概要＞
# - 空間データは自己相関を持つことが多い（単純なクロスバリデーションでは評価できない）
# - {Spatialsample}はk-meansクラスタリングを使用して、データを互いに素なセットのVグループに分割する


# ＜ポイント＞
# - {Spatialsample}のリサンプリングされたオブジェクトは{rsample}と同様に使用することができる


# ＜参考＞
# - 地理空間データの交差検証、正しくできていますか？
#   https://speakerdeck.com/s_uryu/spatial-cross-validation-with-r


# ＜目次＞
# 0 準備
# 1 データ分割
# 2 予測結果のデータセット作成
# 3 CVセットの予測結果を作成
# 4 予測精度の評価


# 0 準備 ------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(spatialsample)

# データロード
# --- アイオワ州エイムズの住宅データ
data("ames", package = "modeldata")

# データ確認
ames %>% print()
ames %>% glimpse()


# 1 データ分割 ----------------------------------------------------------------

# ＜ポイント＞
# - 空間的自己相関を示す可能性があるデータに対して、空間リサンプリング戦略を使用してモデルを評価する

# データ確認
ames %>%
  select(Sale_Price, Year_Built, Gr_Liv_Area, Bldg_Type)

# 問題提起
# --- 空間的自己相関を示す可能性がある
log10(Sale_Price) ~ Year_Built + Gr_Liv_Area +  Bldg_Type

# データ分割
# --- clustering_cv
# --- vfold_cv
set.seed(123)
folds <- ames %>% spatial_clustering_cv(coords = c("Latitude", "Longitude"), v = 15)
folds2 <- ames %>% vfold_cv(v = 15)

# 確認
folds %>% print()
folds %>% glimpse()


# 2 予測結果のデータセット作成 ------------------------------------------------------

# 関数定義
# --- 予測データの作成
compute_preds <- function(splits) {
  # fit the model to the analysis set
  mod <- lm(log10(Sale_Price) ~ Year_Built + Bldg_Type * log10(Gr_Liv_Area),
            data = analysis(splits))
  # identify the assessment set
  holdout <- assessment(splits)
  # return the assessment set, with true and predicted price
  tibble::tibble(Longitude = holdout$Longitude,
                 Latitude = holdout$Latitude,
                 Sale_Price = log10(holdout$Sale_Price),
                 .pred = predict(mod, holdout))
}

# 実行
folds$splits[[7]] %>% compute_preds()


# 3 CVセットの予測結果を作成 ------------------------------------------------------

# 各セットの予測値
cv_res <- folds %>% mutate(.preds = map(splits, compute_preds))
cv_res2 <- folds2 %>% mutate(.preds = map(splits, compute_preds))

# 確認
cv_res %>% print()
cv_res2 %>% print()


# 4 予測精度の評価 --------------------------------------------------------------

# 予測精度の作成
# --- RMSE
cv_rmse <-
  cv_res %>%
    unnest(.preds) %>%
    group_by(id) %>%
    rmse(Sale_Price, .pred)

cv_rmse2 <-
  cv_res2 %>%
    unnest(.preds) %>%
    group_by(id) %>%
    rmse(Sale_Price, .pred)

# 確認
cv_rmse %>% print()
cv_rmse2 %>% print()


# 5 プロット確認 --------------------------------------------------------------

# プロット用データ
X_Plot <-
  cv_res %>%
    unnest(.preds) %>%
    left_join(cv_rmse)

# 確認
X_Plot %>% print()

# プロット作成
# --- FoldごとにRMSEが決定される
# --- RMSEが寄っている
X_Plot %>%
  ggplot(aes(Longitude, Latitude, color = .estimate)) +
  geom_point(alpha = 0.5) +
  labs(color = "RMSE") +
  scale_color_viridis_c()

# プロット作成
# --- 比較用
cv_res2 %>%
    unnest(.preds) %>%
    left_join(cv_rmse) %>%
    ggplot(aes(Longitude, Latitude, color = .estimate)) +
    geom_point(alpha = 0.5) +
    labs(color = "RMSE") +
    scale_color_viridis_c()