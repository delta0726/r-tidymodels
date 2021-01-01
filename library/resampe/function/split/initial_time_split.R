# Title     : initial_time_split
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/initial_split.html


# ＜ポイント＞
# - 時系列データを訓練データとテストデータに分割する
# - 時系列データは時系列の関係を保存しておく必要がある


# ＜メモ＞
# - lagがうまく動作しない


# ＜構文＞
# initial_time_split(data, prop = 3/4, lag = 0, ...)




library(tidyverse)
library(rsample)
library(psych)
library(modeldata)



# データ準備
data(drinks, package = "modeldata")


# Check Data
drinks %>% as_tibble()



#%% ベーシックなケース -------------------------------------------

# データ分割
set.seed(1353)
drinks_split <- drinks %>% initial_time_split()
drinks_split %>% print()


# 分割データの格納
train_data <- drinks_split %>% training()
test_data  <- drinks_split %>% testing()


# 確認
c(max(train_data$date), min(test_data$date))
train_data %>% headTail()
test_data %>% headTail()



#%% ラグありのケース -------------------------------------------

# データ分割
# --- ラグ設定
set.seed(1353)
drinks_split_l <- drinks %>% initial_time_split(lag = 12)
drinks_split_l %>% print()


# 分割データの格納
train_data_l <- drinks_split_l %>% training()
test_data_l  <- drinks_split_l %>% testing()


# 確認
c(max(train_data_l$date), min(test_data_l$date))
train_data_l  %>% headTail()
test_data_l  %>% headTail()



#%% 研究：時系列のパネルデータ -------------------------------------------


data(okc)
str(okc)


# データ概要
okc %>% glimpse()


# 日付ごとのレコード数
okc %>%
  arrange(date) %>%
  group_by(date) %>%
  tally()



okc_split <- okc %>% arrange(date) %>% initial_time_split()
okc_split %>% training() %>% group_by(date) %>% tally() %>% as.data.frame()
okc_split %>% testing() %>% group_by(date) %>% tally() %>% as.data.frame()


