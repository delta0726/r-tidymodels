# Title     : Chapter 8 K-Nearest Neighbors
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/9
# URL       : https://bradleyboehmke.github.io/HOML/knn.html
#           : https://koalaverse.github.io/homlr/notebooks/08-knn.nb.html



# ＜ポイント＞
# - k近傍法は各観測値の予測を、他の予測値の｢類似性｣に基づいて決定している
# - 前処理でも利用されることが多い



# ＜目次＞
# 8.1 準備
# 8.2 類似性の測定
# 8.3 kを選択
# 8.4 MNISTの例



# 8.1 準備 ----------------------------------------------


library(tidyverse)
library(tidymodels)
library(modeldata)
library(magrittr)
library(caret)
library(ggmap)
library(rsample)


# 8.1.1 attrition -------------------------------

# データロード
data(attrition)


# データ確認
attrition %>% as_tibble()
attrition %>% glimpse()


# データ加工
attrit <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)


# データ分割
set.seed(123)
churn_split <- initial_split(attrit, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)



# 8.1.2 ames -------------------------------

# データ準備
ames <- AmesHousing::make_ames()


# データ概要
ames %>% as_tibble()
ames %>% glimpse()


# データ分割
set.seed(123)
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()



# 8.2 類似性の測定 ----------------------------------------------

# 前処理
df <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_nzv(all_nominal()) %>%
    step_integer(matches("Qual|Cond|QC|Qu")) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes()) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    prep(training = ames_train, retain = TRUE) %>%
    juice() %>%
    select(-Sale_Price)


# データ確認
df %>% as_tibble()
df %>% glimpse()


home <- 30
k <- 10
index <- FNN::knnx.index(df[-home, ], df[home, ], k = k) %>% as.vector()
knn_homes <- ames_train[c(home, index), ]


# プロット作成
knn_homes %>%
  select(Longitude, Latitude) %>%
  mutate(desc = factor(c('House of interest', rep('Closest neighbors', k)),
                       levels = c('House of interest', 'Closest neighbors'))) %>%
  qmplot(Longitude, Latitude, data = .,
         maptype = "toner-background", darken = .7, color = desc, size = I(2.5)) +
  theme(legend.position = "top",
        legend.title = element_blank())



# 8.3 kを選択 --------------------------------------------------
# 8.4 MNISTの例 --------------------------------------------------



# import MNIST training data
mnist <- dslabs::read_mnist()
names(mnist)

