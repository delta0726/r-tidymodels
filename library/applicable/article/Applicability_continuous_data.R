# Title     : Applicability domain methods for continuous data
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/11
# URL       : https://applicable.tidymodels.org/articles/continuous-data.html


# ＜ポイント＞
# - Principal component analysis
# - Hat values statistics


# 1.準備 -------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(applicable)
library(AmesHousing)


# データ準備
ames <- make_ames()


# データ確認
ames %>% print()
ames %>% glimpse()



# 2.前処理 -------------------------------------------------------------

# 列名取得
# --- 使用する列を定義
# --- {applicable}のデータセット
ames_cols <- ames_new %>% names()


# 列の追加
# --- 上記の列名で列を取得
# --- Neighborhoodをファクターとして設定
training_data <-
  ames %>%
    dplyr::select(one_of(ames_cols)) %>%
    mutate(Neighborhood = as.character(Neighborhood),
           Neighborhood = factor(Neighborhood, levels = levels(ames$Neighborhood)))


# 確認
training_data %>% print()
training_data %>% glimpse()


# レシピ作成
# --- outcomeは指定せず
# --- ファクターをダミー変換
# --- Zero-Variance Filter
# --- Yeo-Johnson変換
# --- Zスコア化
training_recipe <-
  recipe( ~ ., data = training_data) %>%
    step_dummy(all_nominal()) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(all_numeric()) %>%
    step_normalize(all_numeric())


# 3.主成分分析 -------------------------------------------------------------

# ＜apd_pca関数＞
# - 最大95％または提供された変動しきい値のいずれかを占める主成分を計算
# - 主成分のパーセンタイルと各主成分の平均も計算


# 主成分の計算
# --- 閾値をデフォルトの0.95に指定
# --- 49個の主成分が必要
ames_pca_1 <- training_recipe %>% apd_pca(training_data)
ames_pca_1 %>% print()


# 主成分の計算
# --- 閾値を0.25に指定
# --- 8個の主成分が必要
ames_pca_2 <- apd_pca(training_recipe, training_data, threshold = 0.25)
ames_pca_2 %>% print()


# プロット
ames_pca_1 %>% autoplot()
ames_pca_2 %>% autoplot()



# 4.スコアの計算 -------------------------------------------------------------

# 主成分の計算
ames_pca <- training_recipe %>% apd_pca(training_data)
ames_pca %>% print()


# スコアの計算
pca_score <- ames_pca %>% score(ames)
pca_score %>% select(matches("PC00[1-3]"), contains("distance"))




# `ames_pca$pcs` is the output of `prcomp()`
comp_one <- ames_pca$pcs$rotation[, 1]
comp_one[order(abs(comp_one), decreasing = TRUE)] %>% head(5)
#>       Year_Built      Garage_Cars Foundation_PConc   Year_Remod_Add      Garage_Area
#>       -0.2537606       -0.2176448       -0.2100323       -0.2050218       -0.2019180


non_singular_recipe <-
  training_recipe %>%
  step_lincomb(all_predictors())

# Recipe interface
ames_hat <- apd_hat_values(non_singular_recipe, training_data)