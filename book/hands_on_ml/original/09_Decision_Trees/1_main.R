# ********************************************************************************
# Title   : 決定木
# Chapter : 9
# URL     : https://bradleyboehmke.github.io/HOML/mars.html
# Support : https://koalaverse.github.io/homlr/notebooks/07-mars.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(rpart)
library(rpart.plot)
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



# 2.モデル1 ----------------------------------------------

# モデル構築
ames_dt1 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova"
)


# 確認
ames_dt1 %>% print()


# ツリープロット
ames_dt1 %>% rpart.plot()


# 複雑度(cp)のプロット
ames_dt1 %>% plotcp()



# 3.モデル2 ----------------------------------------------

# モデル構築
ames_dt2 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova", 
  control = list(cp = 0, xval = 10)
)


# ツリープロット
ames_dt2 %>% rpart.plot()


# 複雑度(cp)のプロット
ames_dt2 %>% plotcp()
abline(v = 11, lty = "dashed")


# rpart cross validation results
ames_dt1$cptable




# 4.モデル3 ----------------------------------------------


# クロスバリデーション
ames_dt3 <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)


ames_dt3 %>% ggplot()




# 5.モデル解釈 ----------------------------------------------

# 変数重要度
ames_dt3 %>% vip(num_features = 40, bar = FALSE)


# Construct partial dependence plots
p1 <- ames_dt3 %>% pdp::partial(pred.var = "Gr_Liv_Area") %>% autoplot()
p2 <- ames_dt3 %>% pdp::partial(pred.var = "Year_Built") %>% autoplot()
#p3 <- ames_dt3 %>% pdp::partial(pred.var = c("Gr_Liv_Area", "Year_Built"))

# Display plots side by side
gridExtra::grid.arrange(p1, p2, ncol = 3)
