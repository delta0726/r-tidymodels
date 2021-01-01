# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/02


# ＜ポイント＞
# - juice()はレシピに使用した前処理済データを取得する（レシピ作成に使用したデータを高速取得）
# - bake()は新たなデータセットをレシピに適用して前処理を行う（使いまわしが可能）


# ＜研究1：データを与える３つのタイミング＞
# - recipe(): 必須、モデル定義用のデータ（列名と列属性が分かればOK）
# - prep()  : 任意、指定したデータにレシピを適用（juice()で抽出する際に使用)）
# - bake()  : 必須、指定したデータにレシピを適用


# ＜研究2：prepが行っていること＞
# 1. レシピの完成（パッケージ化）
# 2. recipe()又はprep()で与えたデータで前処理（juiceで取得可能）


library(magrittr)
library(tidyverse)
library(tidymodels)
library(modeldata)

# データ準備
data(biomass)


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# レシピ作成
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
    step_normalize(all_predictors()) %>%
    prep(training = biomass_tr)


# 前処理済データの取得
# --- juice(): 新たなデータを与えない
og_values <- rec %>% juice(all_predictors())
tr_values <- rec %>% bake(new_data = biomass_tr, all_predictors())


# 比較
all.equal(og_values, tr_values)



# prep()の動作 ---------------------------------------

# 元データ
biomass_tr %>%
  as_tibble() %>%
  select(HHV, carbon, hydrogen, oxygen, nitrogen, sulfur) %>%
  print()


# レシピ作成時
# --- 元データのまま
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
  use_series(template)


# step_*()の追加時
# --- 元データのまま
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
  step_normalize(all_predictors()) %>%
  use_series(template)


# prep()の追加時
# --- レシピ適用！
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
  step_normalize(all_predictors()) %>%
  prep(training = biomass_tr) %>%
  use_series(template)



# インプットするデータ ---------------------------------------

# 元データ
# --- 1行のみ
X <- biomass_tr %>% as_tibble() %>% slice(1)


# レシピ作成時
# --- 元データのまま
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = X) %>%
  use_series(template)


# prep()の追加時
# --- データを再定義してレシピ適用！
recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = X) %>%
  step_normalize(all_predictors()) %>%
  prep(training = biomass_tr) %>%
  use_series(template)
