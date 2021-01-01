# Title     : step_interact
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/26
# URL       : https://recipes.tidymodels.org/reference/step_interact.html
#           : https://recipes.tidymodels.org/articles/Dummies.html


# ＜ポイント＞
# - 2つ以上の変数間の交互作用項である新しい列を作成するレシピステップの仕様を作成します。
# - カテゴリ変数はダミー変数に変換しておく
# - terms引数はRモデル式で指定しなければならない
#   --- tidyselectによる指定が可能



# ＜構文＞
# step_interact(
#  recipe,
#  terms,
#  role = "predictor",
#  trained = FALSE,
#  objects = NULL,
#  sep = "_x_",
#  skip = FALSE,
#  id = rand_id("interact")
#)




# 1.準備 ----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(modeldata)


# データロード
data(biomass)


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# 次元数の確認
biomass_tr %>% dim()
biomass_te %>% dim()



# 2.相互効果の追加 ---------------------------------------------------

# ベースレシピ
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)


# レシピ追加１
# --- 相互効果の追加(1パターン)
int_mod_1 <-
  rec %>%
    step_interact(terms = ~ carbon:hydrogen) %>%
    prep(training = biomass_tr)


# レシピ追加２
# --- 相互効果の追加(複数パターン)
int_mod_2 <-
  rec %>%
    step_interact(terms = ~ (matches("gen$") + sulfur)^2) %>%
    prep(training = biomass_tr)


# レシピ追加３
# --- 相互効果の追加(全パターン)
# --- ラベルデータは除外する
int_mod_3 <-
  rec %>%
    step_interact(terms = ~ (everything() - HHV)^2) %>%
    prep(training = biomass_tr)


# レシピ適用
dat_1 <- int_mod_1 %>% bake(biomass_te)
dat_2 <- int_mod_2 %>% bake(biomass_te)
dat_3 <- int_mod_3 %>% bake(biomass_te)


# データ確認
dat_1 %>% glimpse()
dat_2 %>% glimpse()
dat_3 %>% glimpse()


# レシピ確認
int_mod_1 %>% tidy(number = 1)
int_mod_2 %>% tidy(number = 1)
int_mod_3 %>% tidy(number = 1)



# 3.計算証明 ---------------------------------------------------

# ＜ポイント＞
# - 相互効果には対象項の値を掛け算したものが入っている
#

# ベースレシピ
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr) %>%
    step_interact(terms = ~ carbon:hydrogen) %>%
    prep(training = biomass_tr)


# レシピ適用
int_mod_1 %>%
  bake(biomass_te) %>%
  mutate(interact = carbon * hydrogen,
         diff = carbon_x_hydrogen - interact)