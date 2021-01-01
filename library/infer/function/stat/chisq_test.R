# Title     : chisq_test
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/chisq_test.html


# ＜ポイント＞
# - カイ2乗統計量を出力


# ＜構文＞
# chisq_test(x, formula, response = NULL, explanatory = NULL, ...)



library(tidyverse)
library(infer)
library(ggpubr)



# データ確認
gss %>% print()
gss %>% glimpse()


# 統計量の出力
gss %>% chisq_test(college ~ finrela)


# 平均値の確認
gss %>% chisq_stat(college ~ finrela)


# t値の算出
gss %>%
  chisq_test(response = finrela,
             p = c("far below average" = 1/6,
                   "below average" = 1/6,
                   "average" = 1/6,
                   "above average" = 1/6,
                   "far above average" = 1/6,
                   "DK" = 1/6))
