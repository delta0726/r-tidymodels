# Title     : 3 generate
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : -
#           : https://infer.netlify.app/articles/infer.html



# ＜ポイント＞
# - 帰無仮説の分布を順列やリサンプリングで生成する
#



# ＜構文＞
# generate(x, reps = 1, type = NULL, ...)
#
# x    : データフレーム（specify()またはhypothesize()で作成したもの）
# reps : リサンプリング回数
# type : bootstrap / permute / simulate


# ＜リサンプリング方法＞
# - bootstrap : 指定したサンプルサイズで復元抽出
# - permute   : 指定したサンプルサイズで非復元抽出(順列でリサンプル)
# - simulate  : 理論分布に基づいてリサンプリング(現在は点推定のみ対応)




library(tidyverse)
library(infer)


# データロード
data(gss)


# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()



#%% 点推定 -----------------------------------------

# 準備
# --- 応答変数の区分を確認
gss$hours %>% unique()


# NULL分布の作成
# --- 帰無仮説を検証するためのシミュレーションを実施
# --- ブートストラップ法で元のデータサイズと同じ500サンプルを抽出（復元抽出）
# --- 500個のデータで200セットのシミュレーションを作成
x1 <-
  gss %>%
     specify(response = hours) %>%
     hypothesize(null = "point", mu = 40) %>%
     generate(reps = 200, type = "bootstrap")


# 確認
x1 %>% print()
x1 %>% group_by(replicate) %>% tally()



# ＜追加属性＞
# $groups
# # A tibble: 200 x 2
#    replicate       .rows
#  *     <int> <list<int>>
#  1         1       [500]
#  2         2       [500]
#  3         3       [500]
#  4         4       [500]
#  5         5       [500]
#  6         6       [500]
#  7         7       [500]
#  8         8       [500]
#  9         9       [500]
# 10        10       [500]
# # ... with 190 more rows



#%% 区間推定 -----------------------------------------


# NULL分布の作成
# --- 帰無仮説を検証するためのシミュレーションを実施
# --- 順列法で元のデータサイズと同じ500サンプルを抽出（復元抽出）
# --- 200セットのシミュレーションを作成
x2 <-
  gss %>%
    specify(partyid ~ age) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 200, type = "permute")


# 確認
x2 %>% print()
x2 %>% group_by(replicate) %>% tally()

