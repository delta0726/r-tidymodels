# Title     : step_lincomb
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://recipes.tidymodels.org/reference/step_lincomb.html



# ＜ポイント＞
# - 線形結合フィルター
# - 2つ以上の変数間の正確な線形結合を見つけて削除する
# - 引数にmax_stepsが定義されているように、複数回の適用がなされることがある
#   --- 複数の線形結合を探す


# ＜構文＞
# step_lincomb(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  max_steps = 5,
#  removals = NULL,
#  skip = FALSE,
#  id = rand_id("lincomp")
#)



# 1.準備 ----------------------------------------------------------

library(tidyverse)
library(modeldata)
library(recipes)
library(corrplot)


# データロード
data(biomass)


# 線形結合の列を追加
# --- 複数列を加重平均することで作成
biomass$new_1 <- with(biomass, 0.1 * carbon - 0.2 * hydrogen + 0.6 * sulfur)
biomass$new_2 <- with(biomass, 0.5 * carbon - 0.2 * oxygen + 0.6 * nitrogen)


# データ確認
biomass %>% as_tibble()
biomass %>% glimpse()


# 相関プロット1
biomass %>%
  select(carbon, hydrogen, sulfur, oxygen, nitrogen, new_1, new_2) %>%
  cor() %>%
  corrplot(diag=FALSE, type="upper", order="hclust",)


# 相関プロット2
biomass %>% psych::pairs.panels()


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# 次元確認
biomass_tr %>% dim()
biomass_te %>% dim()



# 2.線形結合フィルタ ----------------------------------------------------------

# レシピ作成
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen +
         sulfur + new_1 + new_2, data = biomass_tr) %>%
    step_lincomb(all_predictors(), max_steps = 5) %>%
    prep(training = biomass_tr)


# レシピ適用
# --- sparse列が削除されている
filtered_te <- rec %>% bake(biomass_te)
filtered_te %>% print()


# 確認
rec %>% tidy()
rec %>% tidy(number = 1)

