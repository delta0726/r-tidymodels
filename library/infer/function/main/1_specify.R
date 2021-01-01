# Title     : 1 specify
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/specify.html
#           : https://infer.netlify.app/articles/infer.html


# ＜ポイント＞
# - データフレームのどの列が応答変数と説明変数であるかを指定する
#   --- 分析対象となる列のみを抽出する
# - 指定した列のみのデータフレームが作成される
#   --- クラス情報には｢infer｣が含まれる
#   --- 文字変数は因子に変換されることに注意



# ＜構文＞
# specify(x, formula, response = NULL, explanatory = NULL, success = NULL)
#
# formula     : フォーミュラ指定（引数指定も可能）
# response    : 応答変数
# explanatory : 説明変数
# success     : responseがファクター値の場合の｢興味のある対象｣を指定



library(tidyverse)
library(infer)


# データロード
data(gss)


# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()



#%% 応答変数(数値)のみ指定 -----------------------------------------

# データ確認
# --- 数値データ
gss %>% select(age)


# 応答変数のみ指定
# --- 1変数の点推定
# --- 表面的にはtibbleが返されるが、属性情報が付与されている
x1 <- gss %>% specify(response = age)


# 確認
# --- 単なるデータフレームに見えるが、検定のための属性データを保持している
x1 %>% print()
x1 %>% class()
x1 %>% attributes()

# $response (応答変数)
# age
#
# $response_type (応答変数のデータ型)
# [1] "numeric"
#
# $theory_type
# [1] "One sample t"
#
# $distr_param
#  df
# 499
#
# $type
# [1] "bootstrap"




#%% 応答変数と説明変数を指定 -----------------------------------


# データ確認
# --- 応答変数：数値データ  説明変数：カテゴリカルデータ
gss %>% select(age, partyid)


# 応答変数と説明変数を指定
# --- 区間推定
# --- 表面的にはtibbleが返されるが、属性情報が付与されている
x2 <- gss %>% specify(response = age, explanatory = partyid)


# 確認
x2 %>% print()
x2 %>% class()
x2 %>% attributes()


# $response (応答変数)
# age
#
# $explanatory (説明変数)
# partyid
#
# $response_type (応答変数のデータ型)
# [1] "numeric"
#
# $explanatory_type (説明変数のデータ型)
# [1] "factor"
#
# $type
# [1] "bootstrap"
#
# $theory_type
# [1] "ANOVA"
#
# $distr_param
# [1] 3
#
# $distr_param2
# [1] 496



#%% フォーミュラ指定 -----------------------------------------

# 応答変数と説明変数を指定
# --- 1変数の点推定
x3 <- gss %>% specify(age ~ partyid)

# 確認
x3 %>% print()
x3 %>% class()
x3 %>% attributes()

# $response
# age
#
# $explanatory
# partyid
#
# $response_type
# [1] "numeric"
#
# $explanatory_type
# [1] "factor"
#
# $type
# [1] "bootstrap"
#
# $theory_type
# [1] "ANOVA"
#
# $distr_param
# [1] 3
#
# $distr_param2
# [1] 496



#%% 1つの変数に対する分析の場合 -----------------------------------------

# データ確認
# --- 応答変数：カテゴリカルデータ
gss %>% select(college)


# 応答変数のうち何をSuccessとするかを定義
x4 <- gss %>% specify(response = college, success = "degree")


# 確認
x4 %>% print()
x4 %>% class()
x4 %>% attributes()


# $response (応答変数)
# college
#
# $success (応答変数における正解値)
# [1] "degree"
#
# $response_type (応答変数のデータ型)
# [1] "factor"
#
# $theory_type
# [1] "One sample prop z"
#
# $type
# [1] "bootstrap"


