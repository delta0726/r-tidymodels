# Title     : step_ns
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/06
# URL       : https://recipes.tidymodels.org/reference/step_ns.html




# ＜ポイント＞
# - 自然スプラインでデータを複数次元に分割する


# ＜構文＞
# step_ns(recipe, ..., role = "predictor", trained = FALSE, objects = NULL,
#         deg_free = 2, options = list(), skip = FALSE, id = rand_id("ns"))



library(magrittr)
library(tidyverse)
library(tidymodels)
library(modeldata)
library(splines)


# データロード
data(biomass)


# データ確認
biomass %>% as_tibble()
biomass %>% glimpse()


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]


# レシピ作成
rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass_tr) %>%
    step_ns(carbon, hydrogen) %>%
    prep(training = biomass_tr)


# 確認
rec %>% print()


# レシピの適用
expanded <- rec %>% bake(biomass_te)
expanded %>% print()




#%% 計算証明 ------------------------------------------

# 結果は一致しない
biomass_tr$carbon %>%
  ns(2) %>%
  as_tibble() %>%
  set_colnames(c("carbon_ns_1", "carbon_ns_2"))




#%% Natural Splineについて ------------------------------------------

# Speedを2次のスプラインに分解
vars <-
  cars$speed %>%
    ns(2) %>%
    as_tibble() %>%
    set_colnames(c("X1", "X2"))


# データ結合
data <- cars %>% bind_cols(vars) %>% as_tibble()
data %>% head()


# 線形回帰モデル
# --- スプライン変換されたデータを使う
model <- lm(dist ~ X1 + X2, data)
model %>% summary()


# プロット
cars %>%
  ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula= y ~ ns(x, 2))
