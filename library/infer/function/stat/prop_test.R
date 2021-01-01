# Title     : prop_test
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/13
# URL       : https://infer.netlify.app/reference/prop_test.html


# ＜ポイント＞
# - 比率検定に用いる一連のデータを出力する


# ＜構文＞
# prop_test(
#   x,
#   formula,
#   response = NULL,
#   explanatory = NULL,
#   p = NULL,
#   order = NULL,
#   alternative = "two-sided",
#   conf_int = TRUE,
#   conf_level = 0.95,
#   ...
# )



library(tidyverse)
library(infer)



# 区間推定
# --- 信頼区間が表示される
gss %>%
  prop_test(college ~ sex,
            order = c("female", "male"),
            alternative = "two-sided",
            conf_int = TRUE,
            conf_level = 0.95)



# 点推定
# --- フォーミュラの説明変数がNULLだと単一の比率検定となる
# --- 信頼区間は表示されない
prop_test(gss,
          college ~ NULL,
          p = .2)

