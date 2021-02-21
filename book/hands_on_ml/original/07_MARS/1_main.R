# ********************************************************************************
# Title   : 多変量適応回帰スプライン
# Chapter : 7
# URL     : https://bradleyboehmke.github.io/HOML/mars.html
# Support : https://koalaverse.github.io/homlr/notebooks/07-mars.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(earth)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞




# 1. データ準備 ----------------------------------------------


# データ取得
ames <- read_csv("data/ames.csv")


# データ分割
set.seed(123) 
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# データ確認
ames_train %>% dim()
ames_test %>% dim()


# モデル用データ
X <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
Y <- ames_train$Sale_Price %>% log()


