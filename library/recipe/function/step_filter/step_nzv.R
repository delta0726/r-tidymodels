# Title     : step_nzv
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://recipes.tidymodels.org/reference/step_nzv.html



# ＜ポイント＞
# - 非常にスパースで不均衡な変数を潜在的に削除する
# - 最頻値と2番目の頻度値の頻度比率に基づいて判定している


# ＜構文＞
# step_nzv(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   freq_cut = 95/5,
#   unique_cut = 10,
#   options = list(freq_cut = 95/5, unique_cut = 10),
#   removals = NULL,
#   skip = FALSE,
#   id = rand_id("nzv")
# )



# 1.準備 ----------------------------------------------------------

library(tidyverse)
library(modeldata)
library(recipes)


# データロード
data(biomass)


# スパースな列を追加
# --- 最初の1レコードだけ1、それ以外は0
# --- ほぼ分散ゼロ
biomass$sparse <- c(1, rep(0, nrow(biomass) - 1))


# データ確認
biomass %>% as_tibble()
biomass %>% glimpse()


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# 次元数の確認
biomass_tr %>% dim()
biomass_te %>% dim()




# 2.分散ゼロ付近の列を削除 ----------------------------------------------

# レシピ作成
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + sparse, data = biomass_tr) %>%
    step_nzv(all_predictors()) %>%
    prep(training = biomass_tr)


# レシピ適用
# --- sparse列が削除されている
filtered_te <- rec %>% bake(biomass_te)
filtered_te %>% print()


# 列ごとの標準偏差の確認
biomass_te %>%
  select_if(is.numeric) %>%
  apply(2, sd)


# レシピの確認
rec %>% tidy()