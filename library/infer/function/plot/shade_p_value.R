# Title     : shade_p_value
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/13
# URL       : https://infer.netlify.app/reference/shade_p_value.html


# ＜ポイント＞
# - visualize()で作成したプロットにp値をプロットする
# -


# ＜構文＞
# shade_p_value(obs_stat, direction, color = "red2", fill = "pink", ...)




library(tidyverse)
library(infer)


# データ確認
# --- 整数値のみ
gss$hours %>% sort()
gss$hours %>% table()


# プロット
gss %>%
  ggplot(aes(x = hours)) +
  geom_histogram()


# 点推定
# ---mean number of hours worked per week
point_estimate <-
  gss %>%
    specify(response = hours) %>%
    calculate(stat = "mean") %>%
    pull()


# 帰無分布の作成
# --- 帰無仮説を点推定で平均40と設定
# --- ブートストラップで1000回のシミュレーション
null_dist <-
  gss %>%
    specify(response = hours) %>%
    hypothesize(null = "point", mu = 40) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")


# 確認
null_dist %>% print()
point_estimate %>% print()
null_dist %>% get_p_value(obs_stat = point_estimate, direction = "two_sided")


# プロット作成
# --- 帰無分布とp値
null_dist %>%
  visualize() +
  shade_p_value(obs_stat = point_estimate, direction = "two_sided")


