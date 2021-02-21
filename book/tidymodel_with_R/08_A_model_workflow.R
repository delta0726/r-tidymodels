#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 8 A model workflow
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/14
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜ポイント＞
# - {recipes}の前処理と{parsnip}のモデリングは{workflow}で統合される
# - 前処理とモデリングをパイプライン化することで、チューニングやリサンプリングで簡素な記述が可能となる


# ＜将来計画＞
# - 現在のワークフローは｢モデル｣と｢レシピ｣の2つのみを受け入れることができる
# - 将来計画として以下のようなプロセスを組み込む可能性がある
#   --- 前処理： 特徴量選択など
#   --- 後処理： 閾値のカットオフ、ベイズを用いた確率的評価


# ＜目次＞
# 0 準備
# 1 ワークフローによる機械学習処理
# 2 ワークフローの更新
# 3 ワークフローからの抽出
# 4 特別なフォーミュラの設定


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)

# データロード
data(ames)

# データ加工
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# データ分割
set.seed(123)
ames_split <- ames %>% initial_split(prob = 0.80, strata = Sale_Price)
ames_train <- ames_split %>% training()
ames_test  <-  ames_split %>% testing()

# レシピ作成
ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Latitude, Longitude, deg_free = 20)

# モデル構築
lm_model <-
  linear_reg() %>%
  set_engine("lm")


# 1 ワークフローによる機械学習処理 ----------------------------------------------------

# ワークフロー設定
lm_wflow <-
  workflow() %>%
  add_model(lm_model) %>%
  add_formula(Sale_Price ~ Longitude + Latitude)

# 学習
lm_fit <- lm_wflow %>% fit(ames_train)
lm_fit %>% print()

# 予測
lm_fit %>%
  predict(new_data = ames_test) %>% head()

# クラス確認
lm_fit %>% class()


# 2 ワークフローの更新 --------------------------------------------------------------

# モデル更新
lm_fit %>% update_formula(Sale_Price ~ Longitude)
lm_fit %>% update_formula(Sale_Price ~ Longitude) %>% class()

# レシピの追加
# --- エラー（ワークフローにレシピが既に登録されているため）
# lm_wflow %>%
#   add_recipe(ames_rec)

# レシピの更新
lm_wflow <-
  lm_wflow %>%
  remove_formula() %>%
  add_recipe(ames_rec)

# 確認
lm_wflow %>% print()

# 学習
lm_fit <- lm_wflow %>% fit(ames_train)

# 予測
lm_fit %>% predict(ames_test)


# 3 ワークフローからの抽出 ---------------------------------------------------------------

# レシピの抽出
lm_fit %>%
  pull_workflow_prepped_recipe() %>%
  tidy()

# モデル結果の抽出
lm_fit %>%
  pull_workflow_fit() %>%
  tidy()


# 4 特別なフォーミュラの設定 --------------------------------------------------------------

# ＜ポイント＞
# - 一部のライブラリでは通常のフォーミュラで用いられないパッケージ独自のフォーミュラ表現がある
#   --- 生存分析や階層ベイズ分析など
#   --- parsnipで使用する場合には注意が必要（必要になったら学習）
