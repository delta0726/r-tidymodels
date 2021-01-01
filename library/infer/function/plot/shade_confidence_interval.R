# Title     : shade_confidence_interval
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/shade_confidence_interval.html


# ＜ポイント＞
# - visualize()で作成したプロットに信頼区間をプロットする



# ＜構文＞
# shade_confidence_interval(
#   endpoints,
#   color = "mediumaquamarine",
#   fill = "turquoise",
#   ...
# )




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


# 信頼区間の取得
ci <-
  null_dist %>%
    get_confidence_interval(point_estimate = point_estimate,
                            level = .95,
                            type = "se")

# 確認
null_dist %>% print()
ci %>% print()


# プロット作成
# --- 帰無分布と信頼区間
null_dist %>%
  visualize() +
  shade_confidence_interval(ci)


# プロット作成
# --- 帰無分布と信頼区間
# --- 信頼区間のカラーなし
null_dist %>%
  visualize() +
  shade_confidence_interval(ci, fill = NULL)


