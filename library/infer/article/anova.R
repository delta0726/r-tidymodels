# Title     : Tidy ANOVA (分散分析)
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/14
# URL       : https://infer.netlify.app/articles/anova.html


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



# プロット作成
# --- collegeのカテゴリごとにhoursの分布を表示
gss %>%
  ggplot(aes(x = partyid, y = age)) +
  geom_boxplot()


# F統計量の計算
# --- 分散分析
observed_f_statistic <-
  gss %>%
    specify(age ~ partyid) %>%
    calculate(stat = "F")


# 帰無分布からF値を計算
# --- 順列ベースのシミュレーション
null_distribution <-
  gss %>%
    specify(age ~ partyid) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "F")


# プロット作成
# --- 帰無分布
null_distribution %>%
  visualize() +
  shade_p_value(observed_f_statistic,
                direction = "greater")


# プロット作成
# --- 理論分布
gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") +
  shade_p_value(observed_f_statistic,
                direction = "greater")


# プロット作成
# --- 帰無分布と理論分布
null_distribution %>%
  visualize(method = "both") +
  shade_p_value(observed_f_statistic,
                direction = "greater")


# p値の計算
p_value <-
  null_distribution %>%
    get_p_value(obs_stat = observed_f_statistic,
                direction = "greater")


# 確認
p_value %>% print()

