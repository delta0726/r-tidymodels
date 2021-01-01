# Title     : step_rm
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/26
# URL       : https://recipes.tidymodels.org/reference/step_rm.html



# ＜ポイント＞
# - 特定の列を削除する
#   --- 列名指定に加えてパターンマッチングが可能


# ＜構文＞
# step_rm(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   removals = NULL,
#   skip = FALSE,
#   id = rand_id("rm")
# )



# 1.準備 ----------------------------------------------------------

library(tidyverse)
library(modeldata)
library(recipes)


# データロード
data(biomass)


# データ確認
biomass %>% as_tibble()
biomass %>% glimpse()


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# 次元数の確認
biomass_tr %>% dim()
biomass_te %>% dim()



# 2.特定の列を削除 ----------------------------------------------

# レシピ作成
# --- {tidyselect}や{recipes}の選択関数を使用することができる
smaller_set <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
    step_rm(contains("gen")) %>%
    prep()


# レシピ確認
smaller_set %>% print()
smaller_set %>% summary()
smaller_set %>% tidy()


# レシピ完成
# --- Variables removed hydrogen, oxygen, nitrogen [trained]
smaller_set <- smaller_set %>%
smaller_set %>% print()
smaller_set %>% summary()


# レシピ適用
# --- step_rm()を適用した列が含まれない
filtered_te <- smaller_set %>% bake(biomass_te)
filtered_te


# レシピ確認
# --- prep()の後に実際に削除した列情報が追加される
smaller_set %>% summary()
smaller_set %>% tidy()
