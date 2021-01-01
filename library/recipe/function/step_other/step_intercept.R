# Title     : step_intercept
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/26
# URL       : https://recipes.tidymodels.org/reference/step_intercept.html




# ＜ポイント＞
# - データマトリックスの最初の列に切片または定数項を追加する
# - step_interceptにはデフォルトで予測子の役割が設定されている
#   --- all_predictorsでステップを呼び出すときは、意図しない変換を避けるように注意


# ＜構文＞
# step_intercept(
#   recipe,
#   ...,
#   role = "predictor",
#   trained = FALSE,
#   name = "intercept",
#   value = 1,
#   skip = FALSE,
#   id = rand_id("intercept")
# )




# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(recipes)
library(modeldata)


# データロード
data(biomass)


# データ確認
biomass %>% print()
biomass %>% glimpse()


# データ分割
# --- 元のデータセットに区分が提供されている
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# 次元確認
biomass_tr %>% dim()
biomass_te %>% dim()



# 2 レシピ作成 ---------------------------------------------------------


# レシピ定義
rec_trans <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
         data = biomass_tr) %>%
    step_intercept(value = 2) %>%
    step_scale(carbon)


# 確認
rec_trans %>% print()


# レシピ準備
rec_obj <- rec_trans %>% prep(training = biomass_tr)
rec_obj %>% print()


# データ適用
# --- intercept列が追加されている
with_intercept <- rec_obj %>% bake(biomass_te)
with_intercept %>% print()


