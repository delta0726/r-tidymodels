# Title     : rolling_origin
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/rolling_origin.html



# ＜概要＞
# - 時系列データのリサンプリングを行う



# ＜構文＞
# rolling_origin(data, initial = 5, assess = 1, cumulative = TRUE, skip = 0, lag = 0)



library(tidyverse)
library(rsample)



set.seed(1131)


#%% 基本的な出力 -------------------------------------------

# データ作成
ex_data <- data.frame(row = 1:20, some_var = rnorm(20))
Sliced <- ex_data %>% rolling_origin(initial = 5, , assess = 1, cumulative = TRUE)
Sliced %>% print()


# データ確認
Sliced$splits[[1]] %>% print()
Sliced$splits[[1]] %>% analysis()
Sliced$splits[[1]] %>% assessment()



#%% 年ごとに集計 -------------------------------------------

# データロード
data(drinks, package = "modeldata")
drinks %>% print()


# 年ごとにデータをネスト
drinks_annual <-
  drinks %>%
    mutate(year = as.POSIXlt(date)$year + 1900) %>%
    nest(-year)#> Warning: All elements of `...` must be named.


# 確認
drinks_annual %>% print()


# リサンプリング
multi_year_roll <- drinks_annual %>% rolling_origin(cumulative = FALSE)
multi_year_roll %>% print()


# データ確認
multi_year_roll$splits[[1]] %>% analysis()
multi_year_roll$splits[[1]] %>% assessment()
