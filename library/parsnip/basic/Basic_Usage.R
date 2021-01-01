# Title     : Basic Usage
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/articles/parsnip_Intro.html



# ＜parsnipの思想＞
# - インターフォースの統一化（エンジンごとの異なる引数を統一化）
# - モデルの｢定義｣と｢設定｣を分離（formulaを分離することでモデルの使いまわしが可能）
# - 遅延処理（fit()を定義することで｢設定｣と｢実行｣を分離）



library(tidyverse)
library(tidymodels)



# インターフェースの統一化
# --- エンジンの違いによる引数名のばらつきを統一化
# --- メイン関数はハイパーパラメータの設定のみに集中
# --- ハイパーパラメータはset_args()でも設定可能
rf_mod <- rand_forest(trees = 2000)
rf_mod %>% print()



# 遅延処理
# --- 後ほど設定したい項目にはvarying()をつける
tune_mtry <- rand_forest(trees = 2000, mtry = varying())
tune_mtry %>% print()



# メイン関数はハイパーパラメータのみ設定
rand_forest %>% args()



# 計算エンジンの設定
# --- ライブラリ名を設定
# --- ライブラリ特有の引数もここで設定
rf_with_seed <-
  rand_forest(trees = 2000, mtry = varying(), mode = "regression") %>%
  set_engine("ranger", seed = 63233)


# 確認
rf_with_seed %>% print()


# ハイパーパラメータの更新
# --- set_*()を使うと後で変更することができる
# --- varying()で指定した項目は、set_args()で自由に変更することができる
rf_with_seed %>%
  set_args(mtry = 4) %>%
  set_engine("ranger") %>%
  fit(mpg ~ ., data = mtcars)



# 計算エンジンの更新
# --- set_*()を使うと後で変更することができる
set.seed(56982)
rf_with_seed %>%
  set_args(mtry = 4) %>%
  set_engine("randomForest") %>%
  fit(mpg ~ ., data = mtcars)

