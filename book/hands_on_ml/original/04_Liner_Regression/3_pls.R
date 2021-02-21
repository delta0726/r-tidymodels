# ********************************************************************************
# Title   : 部分最小二乗回帰 (PLS)
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
library(AppliedPredictiveModeling)
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
#・教師付き次元削減に基づく回帰分析




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


# 2. PLS回帰 1 ----------------------------------------------

# モデル精度の検証
# --- PC20まで使用
set.seed(123)
cv_model_pls <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)


# RMSE
# --- PCが多いほど高まる
# --- PC20に近いところが表示される
cv_model_pls1$bestTune


# プロット
# --- RMSE
cv_model_pls %>% ggplot()



# 3. PLS回帰 2 ----------------------------------------------

# モデル精度の検証
# --- PC20まで使用
set.seed(123)
cv_model_pls_2 <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 100
)


# RMSE
# --- PCが多いほど高まる
# --- PC20に近いところが表示される
cv_model_pls_2$bestTune


# プロット
# --- RMSE
cv_model_pls_2 %>% ggplot()



# 4. 変数重要度 ----------------------------------------------

p1 <- cv_model_pls_1 %>% vip(num_features = 20, method = "model")
p2 <- cv_model_pls_2 %>% vip(num_features = 20, method = "model")
grid.arrange(p1, p2)


# 5. PDP ----------------------------------------------

p1 <- 
  cv_model_pls %>% 
    pdp::partial(pred.var = "Gr_Liv_Area", grid.resolution = 20) %>% 
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p2 <- 
  cv_model_pls %>% 
    pdp::partial(pred.var = "First_Flr_SF", grid.resolution = 20) %>% 
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p3 <- 
  cv_model_pls %>% 
    pdp::partial(pred.var = "Total_Bsmt_SF", grid.resolution = 20) %>% 
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p4 <- 
  cv_model_pls %>% 
    pdp::partial(pred.var = "Garage_Cars", grid.resolution = 4) %>% 
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

grid.arrange(p1, p2, p3, p4, nrow = 2)


