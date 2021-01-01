# Title     : validation_split
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/6
# URL       : https://rsample.tidymodels.org/reference/validation_split.html




# ＜ポイント＞
# - 元のデータセットの単一のランダムサンプリングを行う
# - initial_split()とは異なり、Listed DataFrameに格納される



# ＜構文＞
# validation_split(data, prop = 3/4, strata = NULL, breaks = 4, ...)




library(tidyverse)
library(rsample)


# データセットのレコード数
iris %>% nrow()


# データ分割
X <- iris %>% validation_split(prop = .9)
X %>% print()


# データ集計
X %>%
  mutate(training = map(splits, training),
         testing  = map(splits, testing))

