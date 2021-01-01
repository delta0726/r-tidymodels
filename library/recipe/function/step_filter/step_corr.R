# Title     : step_corr
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://recipes.tidymodels.org/reference/step_corr.html



# ＜ポイント＞
# - 相関係数で列をフィルタ(削除)
# - 欠損値を削除しておかないとエラーが発生する
#   --- step_corr()はuse引数で欠損値を含む相関計算を選ぶことができる


# ＜構文＞
# step_corr(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  threshold = 0.9,
#  use = "pairwise.complete.obs",
#  method = "pearson",
#  removals = NULL,
#  skip = FALSE,
#  id = rand_id("corr")
#)



# 1.準備 ----------------------------------------------------------

library(tidyverse)
library(modeldata)
library(recipes)


# データロード
data(biomass)


# 高相関の列を追加
# --- carbonと高相関
set.seed(3535)
biomass$duplicate <- biomass$carbon + rnorm(nrow(biomass))


# データ確認
biomass %>% as_tibble()
biomass %>% glimpse()


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# 次元確認
biomass_tr %>% dim()
biomass_te %>% dim()


# 相関の確認
# --- 相関係数行列
# --- 0.995
biomass_tr %>%
  select_if(is.numeric) %>%
  cor() %>%
  round(3)



# 2.相関係数で列フィルタ ----------------------------------------------------------

# レシピ作成
# --- 相関係数の閾値を指定する
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
    step_corr(all_predictors(), threshold = .5) %>%
    prep(training = biomass_tr)


# レシピ確認
rec %>% print()
rec %>% tidy()


# レシピ適用
# --- duplicate列が削除されている
filtered_te <- rec %>% bake(biomass_te)
filtered_te %>% print()





