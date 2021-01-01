# Title     : step_center
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://recipes.tidymodels.org/reference/step_center.html


# ＜ポイント＞
# - データの標準偏差が1となるようにリスケールする
# - データを標準偏差で割ることによって定義される
# - 平均値は訓練データで決定され、その値を元に他のデータセットを基準化する


# ＜構文＞
# step_center(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   means = NULL,
#   na_rm = TRUE,
#   skip = FALSE,
#   id = rand_id("center")
# )
#
# S3 method for step_center
# tidy(x, ...)


library(tidyverse)
library(tidymodels)
library(modeldata)


# データロード
data(biomass)


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# レシピ作成
# --- 平均と標準偏差が設定される
# --- 前提として訓練データでレシピを作成しなければならない
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
    step_center(carbon, contains("gen"), -hydrogen) %>%
    prep(training = biomass_tr)


# 訓練データ
# --- 平均が0となることの確認
X_tr <- rec %>% juice()
X_tr %>% apply(2, mean) %>% round(5)


# レシピ適用（テストデータ）
# --- 訓練データの分布を基準として計算される
# --- 平均は必ずしも0とならない
X_te <- rec %>% bake(biomass_te)
X_te %>% apply(2, mean) %>% round(5)


# データ確認
biomass_tr %>% as_tibble()
X_tr %>% as_tibble()
X_te %>% as_tibble()



#%% 平均値の記憶 -----------------------------------

# 平均値を記憶することができる
# --- prep()を実行するまではNA
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
  step_center(carbon, contains("gen"), -hydrogen) %>%
  tidy(number = 1)


# prep()を行うと平均値が記憶される
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
  step_center(carbon, contains("gen"), -hydrogen) %>%
  prep(training = biomass_tr) %>%
  tidy(number = 1)
