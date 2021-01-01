# Title     : get_confidence_interval
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/get_confidence_interval.html
#           : https://infer.netlify.app/articles/infer.html



# ＜ポイント＞
# - 要約統計量に関する信頼区間を計算します。


# ＜構文＞
# get_confidence_interval(
#   x,
#   level = 0.95,
#   type = "percentile",
#   point_estimate = NULL
# )




library(tidyverse)
library(infer)



# 点推定(平均値)
point_estimate <-
  gss %>%
    specify(response = hours) %>%
    calculate(stat = "mean") %>%
    dplyr::pull()



# 確認
point_estimate %>% print()



# 信頼区間の計算
# --- hoursについて平均40という点推定を行う
# --- 元データからブートストラップで1000セットの分布を生成
# --- それぞれのセットごとに平均値を算出
# --- 信頼区間の算出
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  get_confidence_interval(point_estimate = point_estimate,
                          level = .95,
                          type = "se")


