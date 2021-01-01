# Title     : 4 calculate
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/calculate.html
#           : https://infer.netlify.app/articles/infer.html



# ＜ポイント＞
# - generate()で生成したリサンプリングから検定統計量を計算する
#   --- replicateをグループとして集計することで{dplyr}で再現可能
# - ブートストラップサンプルが1つしかない場合は統計値がNaNとなる


# ＜構文＞
# calculate(
#   x,
#   stat = c("mean", "median", "sum", "sd", "prop", "count", "diff in means",
#     "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z",
#     "ratio of props", "odds ratio"),
#   order = NULL,
#   ...
# )
#
# stat   : シミュレーションデータで計算する統計量
# order  : 説明変数のレベルを減算するために順序付けする順序を指定する文字列ベクトル
#        : order = c（ "first"、 "second"）は（ "first"-"second"）を意味する


# ＜stat＞
# mean            : 平均値
# median          : 中央値
# sum             : 合計値
# sd              : 標準偏差
# prop            : 比率
# count           : 個数
# diff in means   : 平均の差
# diff in medians : 中央値の差
# diff in props   : 比率の差
# Chisq           : カイ二乗
# F               :
# slope           : 傾き
# correlation     : 相関係数
# t               : t値
# z               : z値
# ratio of props  :
# odds ratio      :



library(tidyverse)
library(infer)


# データロード
data(gss)


# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()



#%% 平均値 -----------------------------------------

# 準備
# --- 応答変数の区分を確認
gss$hours %>% unique()


# 統計量の算出
# --- generate()で生成したリサンプリング結果から計算
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 200, type = "bootstrap") %>%
  calculate(stat = "mean")



# 計算証明
# --- stat = mean
sim <-
  gss %>%
    specify(response = hours) %>%
    hypothesize(null = "point", mu = 40) %>%
    generate(reps = 200, type = "bootstrap")


sim %>% calculate(stat = "mean")
sim %>% summarise(hours = mean(hours))





#%% 平均値の差 -----------------------------------------

# 統計量の算出
# --- ｢平均値の差｣の対象を｢order｣で指定
gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate("diff in means", order = c("degree", "no degree"))


# 計算証明
# --- stat = diff in means
sim <-
  gss %>%
    specify(age ~ college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute")


sim %>%
  calculate("diff in means", order = c("degree", "no degree"))

sim %>%
  group_by(replicate, college) %>%
  summarise(age = mean(age)) %>%
  mutate(age = ifelse(college == "degree", age, -age)) %>%
  group_by(replicate) %>%
  summarise(age = sum(age)) %>%
  ungroup()




