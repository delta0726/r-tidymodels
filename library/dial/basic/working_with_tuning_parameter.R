# Title     : Working with Tuning Parameters
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/22
# URL       : https://dials.tidymodels.org/articles/Basics.html



# ＜ポイント＞
# - 機械学習モデルには、モデルで直接推定できないパラメーターが含まれている（ハイパーパラメーター）
# - パラメータは｢リサンプリング｣｢遺伝子アルゴリズム｣｢ベイジアン検索｣などの方法で間接的に推定される
# - いずれのグリッド検索をする場合でも、検索範囲を適切に設定する必要がある
# - {dials}はハイパーパラメータの検索が適切に行えるように、範囲や条件を整理して提供する


# ＜設計思想＞
# - パラメータの｢記述｣｢照会｣｢変更｣をしやすいフレームワークを提供する
# - パラメータ名称を標準化している
# - parsnipやrecipeと連携している






library(tidyverse)
library(magrittr)
library(dials)
library(rpart)



#%% parametersオブジェクト -------------------------------------------

# 表示
cost_complexity() %>% print()


# オブジェクトの構成
cost_complexity() %>% names()
cost_complexity() %>% glimpse()


# クラス
# --- quant_param： 数値パラメータ
# --- qual_param ： 文字列/理論値パラメータ
cost_complexity() %>% class()
weight_func() %>% class()


# レンジ取得
# --- range引数でデフォルト値の更新も可能
# --- 対数表示される（transで指定されている）
cost_complexity() %>% range_get()
cost_complexity(range = c(-1, 5)) %>% range_get()


# シーケンス取得
# --- デフォルトではtransに基づいて表示
# --- 通常の数値表示も可能
cost_complexity() %>% value_seq(n = 4)
cost_complexity() %>% value_seq(n = 4, original = FALSE)


# ランダムサンプリング
set.seed(5473)
cost_complexity() %>% value_sample(n = 4, original = FALSE)



#%% parametersオブジェクト -------------------------------------------


# 決定木モデル
cart_mod <- rpart(mpg ~ ., data = mtcars, control = rpart.control(cp = 0.000001))
cart_mod$cptable


# CPを取得
cp_vals <- cart_mod$cptable[, "CP"]
cp_vals %>% print()


# We should only keep values associated with at least one split:
cp_vals <- cp_vals[ cart_mod$cptable[, "nsplit"] > 0 ]
cp_vals %>% print()


# レンジの設定
# --- エラー： 対数にしていないため
mtcars_cp <- cost_complexity() %>% value_set(cp_vals)
mtcars_cp %>% print()


# レンジの設定
# --- 対数に変換して設定
mtcars_cp <- cost_complexity() %>% value_set(log10(cp_vals))
mtcars_cp %>% print()


# 確認
mtcars_cp %>% value_seq(2)
mtcars_cp %>% value_sample(20) %>% table()





#%% 文字列パラメータ -------------------------------------------


# 確認
weight_func() %>% print()
weight_func() %>% class()
weight_func() %>% glimpse()


# redefine values
weight_func() %>% value_set(values = c("rectangular", "triangular"))


# ランダム抽出
weight_func() %>% value_sample(3)


# シーケンス抽出
weight_func() %>% value_seq(3)



#%% UNKNOWN VALUES -----------------------------------------------

# ＜ポイント＞
# - パラメータ値の範囲がデータに依存する場合がある
# - k近傍法の場合、トレーニングセットのデータ数がわからない場合と上限が決定できない
# - 未知の値は、finalize()を使用して、パラメーター値を生成する前に初期化する必要がある


# RangeがUnknown(?)で表示される
mtry()
sample_size()
num_terms()
num_comp()


# 初期化
# --- データセットに合わせてレンジを設定
# --- unknownを置換
mtry() %>% finalize(x = mtcars[, -1])



#%% PARAMETER SETS -----------------------------------------------

# ＜ポイント＞
# - パラメータをオブジェクトにまとめて格納することができる
# - これらのオブジェクトは、チューニンググリッドを作成するときに非常に役立ちます。


# 正則化回帰
# linear_reg(mode = "regression", penalty = NULL, mixture = NULL)


# 正則化回帰のパラメータ
glmnet_set <- parameters(list(lambda = penalty(), alpha = mixture()))
glmnet_set %>% print()
glmnet_set %>% glimpse()


glmnet_set %>% update(alpha = mixture(c(.3, .6)))



#%% PARAMETER GRIDS --------------------------------------------------

# ＜ポイント＞
# - 個々のパラメータを組み合わせてパラメータグリッドを作成する
# - これに基づいてグリッドサーチを行う


#
grid_regular(
  mixture(),
  penalty(),
  levels = 3
)



set.seed(1041)
grid_random(
  mixture(),
  penalty(),
  size = 6
)






