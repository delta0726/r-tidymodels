# Title     : 1. Recipes with rsample
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/10
# URL       : https://rsample.tidymodels.org/articles/Applications/Recipes_and_rsample.html


# ＜ポイント＞
# - リサンプリングデータを{purrr}で操作する
# - purrrでレシピやモデルをtidyに適用する
# - tidymodelsでは{workflow}でリサンプリングデータを一括して扱えるが{purrr}の操作も重要



library(tidyverse)
library(tidymodels)
library(AmesHousing)


# データロード
ames <- make_ames()


# データ確認
ames %>% as_tibble()
ames %>% names()
ames %>% glimpse()


# フォーミュラ定義
# --- 使用するのは以下の５データのみ
log10(Sale_Price) ~ Neighborhood + House_Style + Year_Sold + Lot_Area
ames %>% select(Sale_Price, Neighborhood, House_Style, Year_Sold, Lot_Area)


# プロット作成
# --- ヒストグラム
ames %>%
  ggplot(aes(x = Lot_Area)) +
    geom_histogram(binwidth = 5000, col = "red", fill ="red", alpha = .5) +
    theme_bw()


# プロット作成
# --- バーチャート
ames %>%
  ggplot(aes(x = Neighborhood)) +
    geom_bar() +
    coord_flip() +
    xlab("")



#%% レシピ作成 ----------------------------------------------

# レシピ作成
rec <-
  recipe(Sale_Price ~ Neighborhood + House_Style + Year_Sold + Lot_Area, data = ames) %>%
  step_log(Sale_Price, base = 10) %>%
  step_other(Neighborhood, House_Style, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  step_BoxCox(Lot_Area) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# 確認
rec %>% print()
rec %>% tidy()
rec %>% summary()


# レシピ完成
rec_training_set <- rec %>% prep(training = ames)
rec_training_set

# 確認
rec_training_set %>% print()
rec_training_set %>% tidy()
rec_training_set %>% summary()


# レシピ適用
rec_training_set %>% bake(new_data = head(ames))


# レシピ作成で使用したデータを抽出
# --- レシピ適用済
rec_training_set %>% juice() %>% head



#%% リサンプリング ----------------------------------------------

# ブートストラップ・リサンプリング
set.seed(7712)
bt_samples <- bootstraps(ames)
bt_samples %>% print()
bt_samples$splits[[1]]


# レシピ適用
# --- 各bootstrapにレシピを適用して列追加
bt_samples$recipes <-
  bt_samples$splits %>%
    map(prepper, recipe = rec)


# 確認
bt_samples %>% print()
bt_samples$recipes[[1]]



#%% モデリング ----------------------------------------------

# モデル構築
# --- 実際は使わない
fit_lm <- function(rec_obj, ...)
  lm(..., data = juice(rec_obj, everything()))


# モデル適用
# --- モデルを各ブートストラップに適用
bt_samples$lm_mod <-
  bt_samples$recipes %>%
    map(fit_lm, Sale_Price ~ . )

# 確認
bt_samples



#%% 予測 ----------------------------------------------


# 予測データの作成
pred_lm <- function(split_obj, rec_obj, model_obj, ...) {
  mod_data <-
    rec_obj %>%
      bake(new_data = assessment(split_obj),all_predictors(), all_outcomes())

  out <- mod_data %>% select(Sale_Price)
  out$predicted <- predict(model_obj, newdata = mod_data %>% select(-Sale_Price))
  out
}


bt_samples$pred <-
  pmap(
    lst(
      split_obj = bt_samples$splits,
      rec_obj = bt_samples$recipes,
      model_obj = bt_samples$lm_mod
    ),
    pred_lm
  )

# 確認
bt_samples




#%% モデル評価 ----------------------------------------------

# 個別のRMSEの計算
results <- bt_samples$pred %>% map_dfr(rmse, Sale_Price, predicted)
results

# 平均RMSE
results$.estimate %>% mean()

