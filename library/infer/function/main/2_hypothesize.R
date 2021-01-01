# Title     : 2 hypothesize
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/hypothesize.html
#           : https://infer.netlify.app/articles/infer.html



# ＜ポイント＞
# - specify()で作成したデータに対して帰無仮説を宣言する
# - 検定問題の場合のみ使用（信頼区間を求めるだけなら必要ない）



# ＜構文＞
# hypothesize(x, null, p = NULL, mu = NULL, med = NULL, sigma = NULL)
# x     : データフレーム（specify()で属性情報を追加したもの）
# null  : independence(区間推定) / point(点推定)
# p     : Successの割合(0-1)
# mu    : 真の平均値（null引数が｢point｣の時に使用）
# med   : 真の中央値（null引数が｢point｣の時に使用）
# sigma : 真の標準偏差（null引数が｢point｣の時に使用）


# ＜independence(独立性)＞
# - 2つの観測グループについて応答変数がグループを割り当てる説明変数から独立しているかどうかをテストしている





library(tidyverse)
library(infer)


# データロード
data(gss)


# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()




#%% 帰無仮説：independence -----------------------------------------

# 準備
# --- 応答変数の区分を確認
gss %>% select(college, partyid)


# 帰無仮説の設定
# --- 応答変数のcollegeはファクター値なので｢success｣を定義
# ---
x1 <-
  gss %>%
    specify(college ~ partyid, success = "degree") %>%
    hypothesize(null = "independence")


# 確認
x1 %>% print()
x1 %>% attributes()


# $response
# college
#
# $explanatory
# partyid
#
# $success
# [1] "degree"
#
# $response_type
# [1] "factor"
#
# $explanatory_type
# [1] "factor"
#
# $type
# [1] "permute"
#
# $theory_type
# [1] "Chi-square test of indep"
#
# $distr_param
# df
#  4
#
# $null (追加された属性)
# [1] "independence"



#%% 帰無仮説：point -----------------------------------------

gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40)


