# Title     : get_p_value
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/get_p_value.html


# ＜ポイント＞
# - 帰無分布と観測された統計からp値を計算します。
# -.シミュレーションベースのメソッドがサポートされています
# -


# ＜構文＞
# get_p_value(x, obs_stat, direction)



# ＜参考＞
# - https://clover.fcg.world/2017/04/10/8518/




library(tidyverse)
library(infer)



# 点推定(平均値)
point_estimate <-
  gss %>%
    specify(response = hours) %>%
    calculate(stat = "mean") %>%
    dplyr::pull()



# 確認
point_estimate



# p値の計算
# --- hoursについて平均40という点推定を行う
# --- 元データからブートストラップで1000セットの分布を生成
# --- それぞれのセットごとに平均値を算出して点推定の結果のp値を算出
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  get_p_value(obs_stat = point_estimate, direction = "two-sided")