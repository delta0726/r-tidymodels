# Title     : step_lincomb
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://recipes.tidymodels.org/reference/step_lincomb.html


library(tidyverse)
library(modeldata)
library(recipes)
library(corrplot)


# データロード
data(biomass)


# 線形結合の列を追加
biomass$new_1 <- with(biomass, .1*carbon - .2*hydrogen + .6*sulfur)
biomass$new_2 <- with(biomass, .5*carbon - .2*oxygen + .6*nitrogen)


# データ確認
biomass %>% as_tibble()
biomass %>% glimpse()


# 相関プロット
biomass %>%
  select(carbon, hydrogen, sulfur, oxygen, nitrogen, new_1, new_2) %>%
  cor() %>%
  corrplot(diag=FALSE, type="upper", order="hclust",)



# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# レシピ作成
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
         sulfur + new_1 + new_2, data = biomass_tr) %>%
    step_lincomb(all_predictors()) %>%
    prep(training = biomass_tr)


# 確認
rec %>% print()


# レシピ適用
# --- sparse列が削除されている
filtered_te <- rec %>% bake(biomass_te)
filtered_te %>% print()



