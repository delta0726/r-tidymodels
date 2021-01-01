# Title     : step_range
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://recipes.tidymodels.org/reference/step_range.html


# ＜ポイント＞
# - データが0-1の範囲となるように変換する
# - (X - min) / (max - min)
# - データ範囲は訓練データで決定され、その値を元に他のデータセットを変換する


# ＜構文＞
# step_range(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   min = 0,
#   max = 1,
#   ranges = NULL,
#   skip = FALSE,
#   id = rand_id("range")
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
    step_range(carbon, hydrogen) %>%
    prep(training = biomass_tr)


# 訓練データ
# --- データが0-1の範囲となっている
X_tr <- rec %>% juice()
X_tr %>% apply(2, max) %>% round(5)
X_tr %>% apply(2, min) %>% round(5)


# レシピ適用（テストデータ）
# --- 訓練データの分布を基準として計算される
# --- 必ずしも0-1の範囲とならない
X_te <- rec %>% bake(biomass_te)
X_te %>% apply(2, max) %>% round(5)
X_te %>% apply(2, min) %>% round(5)


# データ確認
biomass_tr %>% as_tibble()
X_tr %>% as_tibble()
X_te %>% as_tibble()



#%% 平均値の記憶 -----------------------------------

# 平均値を記憶することができる
# --- prep()を実行するまではNA
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
  step_range(carbon, hydrogen) %>%
  tidy(number = 1)


# prep()を行うと平均値が記憶される
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
  step_range(carbon, hydrogen) %>%
  prep(training = biomass_tr) %>%
  tidy(number = 1)
