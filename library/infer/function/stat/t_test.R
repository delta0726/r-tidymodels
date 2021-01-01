# Title     : t_test
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/t_stat.html


# ＜ポイント＞
# -


# ＜構文＞
# t_test(
#  x,
#  formula,
#  response = NULL,
#  explanatory = NULL,
#  order = NULL,
#  alternative = "two-sided",
#  mu = 0,
#  conf_int = TRUE,
#  conf_level = 0.95,
#  ...
# )



library(tidyverse)
library(infer)



# データ確認
gss %>% print()
gss %>% glimpse()


# プロット
gss %>%
  ggplot(aes(x = college, y = hours)) +
  geom_bar(stat = "identity")


# t検定
gss %>%
   tidyr::drop_na(college) %>%
   t_test(formula = hours ~ college,
      order = c("degree", "no degree"),
      alternative = "two-sided")
