# ******************************************************************************
# Title     : step_discretize_cart
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/30
# URL       : https://embed.tidymodels.org/reference/step_discretize_cart.html
# ******************************************************************************


# ＜ポイント＞
# - 連続データの離散化を行うレシピの一つ（順序情報を保持）
# - CARTモデルを使用した教師付きの学習で数値データを因子データに離散化する
#   --- モデルを適用することにより、数値変数から不均一なビンを作成
#   --- 元のデータが置換される点に注意
# - rpart::rpart()と同じハイパーパラメータを持つ
# - このレシピの前には欠損値を補完しておく必要がある



# ＜構文＞
# step_discretize_cart(
#  recipe,
#  ...,
#  role = NA,
#  trained = FALSE,
#  outcome = NULL,
#  cost_complexity = 0.01,
#  tree_depth = 10,
#  min_n = 20,
#  rules = NULL,
#  skip = FALSE,
#  id = rand_id("discretize_cart")
#)


# ＜ハイパーパラメータ＞
# - cost_complexity: ツリーモデルの複雑さ
# - tree_depth     : 木の深さ
# - min_n          : 最小ノードサイズ


# ＜目次＞
# 1 準備
# 2 データ分割
# 3 レシピ適用
# 4 データ確認



# 1 準備 ---------------------------------------------------------


library(tidyverse)
library(tidymodels)
library(embed)
library(modeldata)
library(DataExplorer)


# データロード
data(ad_data)


# データ確認
ad_data %>% as_tibble()
ad_data %>% glimpse()


# ラベル確認
# --- Class列
ad_data$Class %>% table()


# 欠損値確認
# --- 欠損値あり
ad_data %>% is.na() %>% apply(2, sum)



# 2 データ分割 -------------------------------------------------------

# データ分割
# --- 層別サンプリング
split <- ad_data %>% initial_split(strata = "Class")
ad_data_tr <- split %>% training()
ad_data_te <- split %>% testing()
ad_data_tr %>% as_tibble()
ad_data_te %>% as_tibble()




# 3 レシピ適用 ------------------------------------------------------

# 対象列の確認
# --- 数値列
ad_data_tr %>% select(tau, age, p_tau, Ab_42)
ad_data_tr %>% select(tau, age, p_tau, Ab_42) %>% glimpse()


# レシピ作成
# --- CARTの設定は初期値のまま使用
cart_rec <-
  recipe(Class ~ ., data = ad_data_tr) %>%
  step_discretize_cart(tau, p_tau, Ab_42, outcome = "Class", id = "cart splits")


# レシピ確認
# --- prep()の適用前
cart_rec %>% print()


# レシピ完成
cart_rec <- cart_rec %>% prep(training = ad_data_tr)
cart_rec %>% print()
cart_rec %>% summary() %>% as.data.frame()
cart_rec %>% tidy(id = "cart splits")



# 4 データ確認 --------------------------------------------------------

# レシピ適用
ad_data_te_baked <- cart_rec %>% bake(ad_data_te)


# データ確認
ad_data_te_baked %>% select(Class, tau, p_tau, Ab_42)
ad_data_te_baked %>% select(Class, tau, p_tau, Ab_42) %>% glimpse()


# 参考：レシピ詳細情報
cart_rec %>% tidy(id = "cart splits")


# 離散値の範囲
ad_data_te_baked %>%
  select(Class, tau, p_tau, Ab_42) %>%
  map(levels)

