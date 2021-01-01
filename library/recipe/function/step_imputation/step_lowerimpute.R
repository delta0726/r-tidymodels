# Title     : step_lowerimpute
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/27
# URL       : https://recipes.tidymodels.org/reference/step_lowerimpute.html


# ＜ポイント＞
# - 負でない数値データを既知の値未満で測定できない場合に使用するレシピ
#   --- 切り捨てられた値を、ゼロと切り捨てポイントの間のランダムな均一の数値で置き換える
#   --- NAを代入するのではなく、下限値を拡張して代入する
# トレーニング引数で使用されるデータから変数の最小値を推定する
#   --- ゼロと最小値の間のランダムな均一値で、最小値のデータの値をシミュレートする
#   --- 数値データとカテゴリカルデータの両方で使うことができる


# ＜構文＞
# step_lowerimpute(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  threshold = NULL,
#  skip = FALSE,
#  id = rand_id("lowerimpute")
#)





# 1 準備 ----------------------------------------------------

library(tidyverse)
library(tidymodels)
library(modeldata)
library(skimr)
library(gridExtra)


# データロード
data("biomass")


# データ修正
# --- 40以下を40に置換
biomass$carbon <- ifelse(biomass$carbon > 40, biomass$carbon, 40)
biomass %>% ggplot(aes(x = carbon)) + geom_histogram()


# データ修正
# --- 5以下を5に置換
biomass$hydrogen <- ifelse(biomass$hydrogen > 5, biomass$carbon, 5)
biomass %>% ggplot(aes(x = hydrogen)) + geom_histogram()


# データ分割
biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]


#
biomass_tr %>% skim()
biomass_te %>% skim()


# 2 knnによるレシピ作成 ----------------------------------------------------

# レシピ作成
impute_rec <-
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,data = biomass_tr) %>%
  step_lowerimpute(carbon, hydrogen)


impute_rec %>% tidy()
impute_rec %>% tidy(number = 1)


# レシピ完成
impute_rec <- impute_rec %>% prep(training = biomass_tr)
impute_rec %>% tidy()
impute_rec %>% tidy(number = 1)


# レシピ適用
transformed_te <- impute_rec%>% bake(biomass_te)
transformed_te %>% print()





# 3 データ確認 ----------------------------------------------------

p1 <- biomass %>% ggplot(aes(x = carbon)) + geom_histogram()
p2 <- transformed_te %>% ggplot(aes(x = carbon)) + geom_histogram()
grid.arrange(p1, p2, nrow = 1)

p1 <- biomass %>% ggplot(aes(x = hydrogen)) + geom_histogram()
p2 <- transformed_te %>% ggplot(aes(x = hydrogen)) + geom_histogram()
grid.arrange(p1, p2, nrow = 1)


# carbon
plot(transformed_te$carbon, biomass_te$carbon,
     ylab = "pre-imputation", xlab = "imputed")

