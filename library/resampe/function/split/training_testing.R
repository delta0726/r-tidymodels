# Objective : training / testing
# Title     : TODO
# Created by: Owner
# Created on: 2020/06/24
# URL       : https://rsample.tidymodels.org/reference/initial_split.html



# ＜ポイント＞
# - rsplitオブジェクトからAnalysisデータとAssesmentデータを取得する
# - training()とtesting()はラッパー関数になっている
#   --- analysis()とassesment()でも同様に動作する


# ＜関数の中身＞
# --- analysis()とassesment()のラッパー関数になっている
training %>% print()
testing %>% print()



library(magrittr)
library(rsample)


# データ分割
# --- Analysis / Assesment という名称で分割されている
set.seed(1353)
car_split <- mtcars %>% initial_split()
car_split %>% print()
car_split %>% class()


# 分割データの取得
train_data <- car_split %>% training()
test_data  <- car_split %>% testing()


# 分割データの取得
train_data <- car_split %>% analysis()
test_data  <- car_split %>% assessment()


