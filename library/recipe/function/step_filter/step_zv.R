# Title     : step_zv
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/26
# URL       : https://recipes.tidymodels.org/reference/step_zv.html


# ＜ポイント＞
# - 分散ゼロの変数を除外する
#   --- モデルに入っていても意味がないデータ
#   --- 意図しない挙動を招く恐れがあるので削除しておく


# ＜構文＞
# step_arrange(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   inputs = NULL,
#   skip = FALSE,
#   id = rand_id("arrange")
# )


# 1.準備 ----------------------------------------------------------

library(tidyverse)
library(modeldata)
library(recipes)


# データロード
data(biomass)


# データ追加
# --- 同じ値(分散ゼロ)
biomass$one_value <- 1


# データ確認
biomass %>% as_tibble()
biomass %>% glimpse()


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# 次元数の確認
biomass_tr %>% dim()
biomass_te %>% dim()




# 2.分散ゼロの列を削除 ----------------------------------------------

# レシピ作成
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur + one_value, data = biomass_tr) %>%
    step_zv(all_predictors())


# レシピ確認
# --- one_valueはまだある
# --- レシピの段階で削除されるわけではない
rec %>% summary()


# レシピ完成
# --- one_valueが削除されている
# --- prep()を適用した段階で削除される
filter_obj <- rec %>% prep(training = biomass_tr)
filter_obj %>% print()
filter_obj %>% summary()


# レシピ適用
# --- 分散ゼロの列が含まれない
filtered_te <- filter_obj %>% bake(biomass_te)
filtered_te %>% print()


# レシピ確認
filter_obj %>% tidy()
filter_obj %>% tidy(number = 1)
