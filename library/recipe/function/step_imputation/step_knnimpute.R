# Title     : step_knnimpute
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/27
# URL       : https://recipes.tidymodels.org/reference/step_knnimpute.html


# ＜ポイント＞
# - impute_with()で説明変数全てを選択して最近傍を作成する
#   --- 欠損値を持つ予測子自体を受け入れることができる
#   --- 数値データとカテゴリカルデータの両方で使うことができる


# ＜構文＞
# step_knnimpute(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  neighbors = 5,
#  impute_with = imp_vars(all_predictors()),
#  options = list(nthread = 1, eps = 1e-08),
#  ref_data = NULL,
#  columns = NULL,
#  skip = FALSE,
#  id = rand_id("knnimpute")
#)





# 1 準備 ----------------------------------------------------

library(tidyverse)
library(tidymodels)
library(modeldata)
library(skimr)


# データロード
data("biomass")


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]
biomass_te_whole <- biomass_te


# 欠損値の作成
set.seed(9039)
carb_missing <- sample(1:nrow(biomass_te), 3)
nitro_missing <- sample(1:nrow(biomass_te), 3)
biomass_te$carbon[carb_missing] <- NA
biomass_te$nitrogen[nitro_missing] <- NA


# データ作成
biomass_te %>% glimpse()
biomass_te %>% skim()
biomass_te %>% slice(carb_missing)
biomass_te %>% slice(nitro_missing)



# 2 knnによるレシピ作成 ----------------------------------------------------

# レシピ作成
ratio_recipe <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,data = biomass_tr) %>%
  step_knnimpute(all_predictors(), neighbors = 3)


ratio_recipe %>% tidy()


# レシピ完成
ratio_recipe2 <- ratio_recipe %>% prep(training = biomass_tr)
ratio_recipe2 %>% tidy()
ratio_recipe2 %>% tidy(number = 1)


# レシピ適用
imputed <- ratio_recipe2 %>% bake(biomass_te)
imputed %>% print()



# 3 データ確認 ----------------------------------------------------

# carbon
cbind(original = biomass_te_whole$carbon[carb_missing],
      imputed = imputed$carbon[carb_missing])


# nitrogen
cbind(original = biomass_te_whole$nitrogen[nitro_missing],
      imputed = imputed$nitrogen[nitro_missing])

