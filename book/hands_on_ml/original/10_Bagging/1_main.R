# ********************************************************************************
# Title   : バギング
# Chapter : 10
# URL     : https://bradleyboehmke.github.io/HOML/bagging.html
# Support : https://koalaverse.github.io/homlr/notebooks/10-bagging.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(rpart)
library(rpart.plot)
library(ipred)
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



# 2.ipredによるシミュレーション----------------------------------------------

# 乱数シード
set.seed(123)

# バギング
# --- エラー発生
# train bagged model
# ames_bag1 <- 
#   bagging(formula = Sale_Price ~ .,　
#           data = ames_train, 
#           nbagg = 100, 
#           coob = TRUE, 
#           control = rpart.control(minsplit = 2, cp = 0)
#           )



# 3.ブートストラップでバギングを作成 ----------------------------------------------

# パラメータ設定
# --- 木の数
ntree <- seq(1, 200, by = 2)

# RMSE
# --- 格納用のベクトル
rmse <- vector(mode = "numeric", length = length(ntree))


# ブートストラップ
for (i in seq_along(ntree)) {
  
  # 乱数シード
  set.seed(123)
  
  # モデル構築
  model <- 
    ranger::ranger(formula = Sale_Price ~ ., 
                   data    = ames_train, 
                   num.trees = ntree[i], 
                   mtry = ncol(ames_train) - 1, 
                   min.node.size = 1)
  
  # get OOB error
  rmse[i] <- model$prediction.error %>% sqrt()
}


# 結果整理
bagging_errors <- data.frame(ntree, rmse)


# プロット
# --- ツリーが増えると一般的にRMSEは下がる
# --- 一般的に50-100本程度でRMSEは安定してくる
# --- ツリーを増やしてもRMSEは変化しな一方、モデルの複雑さは増していく
bagging_errors %>% 
  ggplot(aes(ntree, rmse)) +
    geom_line() +
    geom_hline(yintercept = 41019, lty = "dashed", color = "grey50") +
    annotate("text", x = 100, y = 41385, label = "Best individual pruned tree", vjust = 0, hjust = 0, color = "grey50") +
    annotate("text", x = 100, y = 26750, label = "Bagged trees", vjust = 0, hjust = 0) +
    ylab("RMSE") +
    xlab("Number of trees")


# 4.caretでバギング ----------------------------------------------

# バギング + クロスバリデーション
# --- 所要時間：26分
# --- Bagged CART: http://topepo.github.io/caret/train-models-by-tag.html#bagging
ames_bag2 <- 
  train(Sale_Price ~ .,
        data = ames_train,
        method = "treebag",
        #trControl = trainControl(method = "cv", number = 10),
        nbagg = 200,  
        control = rpart.control(minsplit = 2, cp = 0)
        )

ames_bag2



# 4.caretでバギング(並列化) ----------------------------------------------

# Create a parallel socket cluster
cl <- makeCluster(8)
registerDoParallel(cl)

# バギングを並列化して実行
# --- エラー発生
predictions <- foreach(
  icount(160), 
  .packages = "rpart", 
  .combine = cbind
) %dopar% {
  # bootstrap copy of training data
  index <- sample(nrow(ames_train), replace = TRUE)
  ames_train_boot <- ames_train[index, ]  
  
  # fit tree to bootstrap copy
  bagged_tree <- rpart(
    Sale_Price ~ ., 
    control = rpart.control(minsplit = 2, cp = 0),
    data = ames_train_boot
  ) 
  
  predict(bagged_tree, newdata = ames_test)
}

predictions[1:5, 1:7]


# Shutdown parallel cluster
stopCluster(cl)
