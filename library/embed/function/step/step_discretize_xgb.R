# ******************************************************************************
# Title     : step_discretize_xgb
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/30
# URL       : https://embed.tidymodels.org/reference/step_discretize_xgb.html
# ******************************************************************************


# ＜ポイント＞
# - 連続データの離散化を行うレシピの一つ（順序情報を保持）
# - xgboostを使用した教師付きの学習で数値データをビンに離散化する
# - このレシピの前には欠損値を補完しておく必要がある
# - 不均一なビンを作成することにより、データから非線形パターンを学習しやすくなる
#   --- このステップは特に線形モデルで使用することを目的としています。


# ＜構文＞
# step_discretize_xgb(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  outcome = NULL,
#  sample_val = 0.2,
#  learn_rate = 0.3,
#  num_breaks = 10,
#  tree_depth = 1,
#  min_n = 5,
#  rules = NULL,
#  skip = FALSE,
#  id = rand_id("discretize_xgb")
#)


# ＜ハイパーパラメータ＞
# - sample_val : ツリーモデルの複雑さ
# - learn_rate : 学習率
# - num_breaks
# - tree_depth : 木の深さ
# - min_n      : 最小ノードサイズ


# ＜目次＞
# 1 準備
# 2 データ分割
# 3 レシピ適用
# 4 データ確認



# 1 準備 ---------------------------------------------------------


library(tidyverse)
library(tidymodels)
library(modeldata)


# データロード
data(credit_data)


# データ確認
credit_data %>% as_tibble()
credit_data %>% glimpse()


# ラベル確認
credit_data$Status %>% table()


# 欠損値確認
# --- 欠損値あり
credit_data %>% is.na() %>% apply(2, sum)



# 2 データ分割 -------------------------------------------------------

# データ分割
# --- 層別サンプリング
split <- credit_data %>% initial_split(strata = "Status")
credit_data_tr <- split %>% training()
credit_data_te <- split %>% testing()
credit_data_tr %>% as_tibble()
credit_data_te %>% as_tibble()



# 3 レシピ適用 ------------------------------------------------------

# データ型の確認
# --- 数値データが多く含まれる
credit_data %>% glimpse()


# レシピ作成
xgb_rec <-
  recipe(Status ~ ., data = credit_data_tr) %>%
  step_medianimpute(all_numeric()) %>%
  step_discretize_xgb(all_numeric(), outcome = "Status")


# レシピ確認
# --- prep()の適用前
xgb_rec %>% print()
xgb_rec %>% summary()


# レシピ完成
xgb_rec <- xgb_rec %>% prep(training = credit_data_tr)
xgb_rec %>% print()
xgb_rec %>% tidy()
xgb_rec %>% tidy(number = 2)



# 4 データ確認 --------------------------------------------------------

# レシピ適用
credit_data_te_baked <- xgb_rec %>% bake(credit_data_te)


# データ確認
credit_data_te_baked %>% as_tibble()
credit_data_te_baked %>% glimpse()


# 離散値の範囲
credit_data_te_baked %>%
  map(levels)



