# Title     : 2 レシピでデータを前処理する
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/17
# URL       : https://www.tidymodels.org/start/recipes/


# ＜ポイント＞
# - レシピを導入することで前処理を効率化＆汎化することを目指す
#   --- {recipes}を学ぶことで前処理の手法を理解する
# - ｢モデル｣と｢レシピ｣をバンドルする方法として｢ワークフロー｣を導入する



# ＜目次＞
# 1 データ準備
# 2 データ分割
# 3 前処理(レシピ作成)
# 4 モデリング
# 5 学習
# 6 予測
# 7 モデル評価



# 1 データ準備 ---------------------------------------------------


library(tidyverse)
library(tidymodels)
library(nycflights13)
library(skimr)


# 乱数シードの設定
set.seed(123)


# データ準備
# --- フライトの到着時刻の遅延状況を分析したデータ
flight_data <-
  flights %>%
    mutate(arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
           arr_delay = factor(arr_delay),
           date = as.Date(time_hour)) %>%
    inner_join(weather, by = c("origin", "time_hour")) %>%
    select(dep_time, flight, origin, dest, air_time, distance,
           carrier, date, arr_delay, time_hour) %>%
    na.omit() %>%
    mutate_if(is.character, as.factor)


# 比率確認
# --- late / on_time
# --- lateが16％程度を占める
flight_data %>%
  count(arr_delay) %>%
  mutate(prop = n/sum(n))


# データ確認
flight_data %>% glimpse()


# データサマリー
flight_data %>%
  skimr::skim(dest, carrier)



# 2 データ分割 ---------------------------------------------------

# データ分割
set.seed(555)
data_split <- flight_data %>% initial_split(prop = 3/4)
data_split %>% print()


# 分割データの格納
train_data <- data_split %>% training()
test_data  <- data_split %>% testing()


# データ確認
train_data %>% print()
test_data %>% print()




# 3 前処理(レシピ作成) ----------------------------------------------------

# 3-1 IDの付与 -------------------------------------

# レシピ作成
# --- レコードを特定するキーにロール(ID)を付与
# --- 後のプロセスのデータ管理で使う
flights_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
    update_role(flight, time_hour, new_role = "ID")


# 確認
flights_rec %>% summary()




# 3-2 日付のエンコーディング -------------------------

# 日付の数値変換
# --- 日付はシリアル値で管理されている
flight_data %>%
  distinct(date) %>%
  mutate(numeric_date = as.numeric(date))


# レシピの更新
# --- 日付要素をダミー変数化
# --- 休日をダミー変数化(米国カレンダーに基づく)
# --- 日付は冗長データとなるので削除
flights_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
    update_role(flight, time_hour, new_role = "ID") %>%
    step_date(date, features = c("dow", "month")) %>%
    step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
    step_rm(date)



# 3-3 カテゴリデータのダミー変数化 -------------------------

# レシピの更新
# --- カテゴリカルデータのダミー変数化
flights_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
    update_role(flight, time_hour, new_role = "ID") %>%
    step_date(date, features = c("dow", "month")) %>%
    step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
    step_rm(date) %>%
    step_dummy(all_nominal(), -all_outcomes())




# 3-4 ゼロ分散の変数を削除 -------------------------

# レシピの更新
# --- 分散がゼロの特徴量を除外する
flights_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID") %>%
    step_date(date, features = c("dow", "month")) %>%
    step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
    step_rm(date) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_zv(all_predictors())



# 4 モデリング --------------------------------------------------------

# ＜ポイント＞
# - ｢parsnipモデル｣と｢前処理レシピ｣はワークフローに統合するように設計されている
#


# モデル構築
lr_mod <-
  logistic_reg() %>%
  set_engine("glm")


# ワークフローの設定
# --- ｢モデル｣と｢レシピ｣をワークフローに設定
flights_wflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(flights_rec)


# ワークフローの確認
flights_wflow %>% print()



# 5 学習 --------------------------------------------------------

# モデル学習
flights_fit <-
  flights_wflow %>%
    fit(data = train_data)


# パラメータの確認
flights_fit %>%
  pull_workflow_fit() %>%
  tidy()



# 6 予測 --------------------------------------------------------

# 予測
flights_fit %>% predict(test_data)


# 予測値を元データに結合
flights_pred <-
  flights_fit %>%
    predict(test_data, type = "prob") %>%
    bind_cols(test_data %>% select(arr_delay, time_hour, flight))


# データ確認
flights_pred



# 7 モデル評価 --------------------------------------------------------

# ROCカーブ
flights_pred %>%
  roc_curve(truth = arr_delay, .pred_late) %>%
  autoplot()


# AUC
flights_pred %>%
  roc_auc(truth = arr_delay, .pred_late)
