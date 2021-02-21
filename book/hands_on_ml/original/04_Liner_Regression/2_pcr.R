# ********************************************************************************
# Title   : PCA回帰 (PCR)
# Chapter : 4
# URL     : https://bradleyboehmke.github.io/HOML/linear-regression.html
# Support : https://koalaverse.github.io/homlr/notebooks/06-regularized-regression.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(glmnet)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞
#・PCAを使用して特徴量を減らしたうえで回帰モデルを適用する
#・PCRはPCAで成分を直交化させていることから多重共線性が発生しない
#・PCAを行う際に変数選択を行っているわけではない
#　-- Yとの関係を最大化するように設計されているわけではない
#・予測結果は安定している
#  -- 導入したデータで予測した本当の姿を示していると見てよい

# ＜解釈＞
#・教師なし次元削減に基づく回帰分析



# 1. データ準備 ----------------------------------------------

# ・このデータは｢Sec2｣「Sec4」で使用する
# 


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



# 2. PCA回帰 1 ----------------------------------------------

# モデル精度の検証
# --- PC20まで使用
set.seed(123)
cv_model_pcr <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)

# RMSE
# --- PCが多いほど高まる
# --- PC20に近いところが表示される
cv_model_pcr$bestTune


# プロット
# --- RMSE
cv_model_pcr %>% ggplot()



# 2. PCA回帰 2 ----------------------------------------------

# モデル精度の検証
# --- PC100まで使用
set.seed(123)
cv_model_pcr <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 100
)

# RMSE
# --- PCが多いほど高まる
# --- PC20に近いところが表示される
cv_model_pcr$bestTune


# プロット
# --- RMSE
cv_model_pcr %>% ggplot()

