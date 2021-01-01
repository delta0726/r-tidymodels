# Title     : t_stat
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/t_stat.html


# ＜ポイント＞
# - コンピュータの実験計画におけるパラメータグリッドを構築するために使用される
# -.空間の任意の部分に、それほど遠くない観測された組み合わせが設定される。
# -


# ＜構文＞
# t_stat(
#   x,
#   formula,
#   response = NULL,
#   explanatory = NULL,
#   order = NULL,
#   alternative = "two-sided",
#   mu = 0,
#   conf_int = FALSE,
#   conf_level = 0.95,
#   ...
# )



library(tidyverse)
library(infer)



# データ確認
gss %>% print()
gss %>% glimpse()


# プロット
gss %>%
  ggplot(aes(x = hours)) +
  geom_histogram()


# 平均値の確認
gss$hours %>% mean(na.rm = TRUE)


# t値の算出
# --- 推定値に対するt値を算出
# --- 実測値を入力するとt値はゼロ
gss %>% t_stat(response = hours, mu = 40)
gss %>% t_stat(response = hours, mu = mean(.$hours, na.rm = TRUE))



#
gss %>%
   drop_na(college) %>%
   t_stat(formula = hours ~ college,
          order = c("degree", "no degree"),
          alternative = "two.sided")

