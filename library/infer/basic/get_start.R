# Title     : はじめに
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/index.html


# ＜概要＞
# - {infer}はtidyverseのフレームワークで柔軟な統計的推論を行うことを目指す
# - 以下の4つの関数で一連の統計的検定を行う
#   --- specify()     : 関心のある変数または変数間の関係を指定。
#   --- hypothesize() : 帰無仮説を宣言。
#   --- generate()    : 帰無仮説を反映するデータを生成。
#   --- calculate()   : 生成されたデータから統計の分布を計算してNull分布を形成




library(tidyverse)
library(infer)


# データロード
data(gss)



# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()


# F値の算出
F_hat <-
  gss %>%
    specify(age ~ partyid) %>%
    calculate(stat = "F")

# 確認
F_hat %>% print()


# Null分布
null_distn <-
  gss %>%
   specify(age ~ partyid) %>%
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "F")


# 確認
null_distn %>% print()


# プロット
null_distn %>%
  visualize() +
    shade_p_value(obs_stat = F_hat, direction = "greater")


# p値の算出
null_distn %>%
  get_p_value(obs_stat = F_hat, direction = "greater")

