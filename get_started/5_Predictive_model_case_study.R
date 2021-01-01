# Title     : 予測モデリングのケーススタディ
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/17
# URL       : https://www.tidymodels.org/start/case-study/


# ＜ポイント＞
# - ２つのモデルの予測精度を比較してモデルを選択する
# - グリッドサーチによるチューニングを行ってパラメータを決定する
# - ｢訓練データ｣｢検証データ｣によるチューニング、｢テストデータ｣による最終チェックを行う


# ＜目次＞
# 1 データ準備
# 2 データ分割
# 3 モデル1: ロジスティック回帰
# 4 モデル2: ランダムフォレスト
# 5 最終モデル



# 1 データ準備 ---------------------------------------------------

library(tidyverse)
library(tidymodels)
library(vip)


# データロード
# --- ホテルの予約情報データ
hotels <-
  read_csv("main/get_started/csv/hotels.csv") %>%
  mutate_if(is.character, as.factor)


# データ確認
hotels %>% as_tibble()
hotels %>% glimpse()


# ラベル頻度
hotels %>%
  count(children) %>%
  mutate(prop = n / sum(n))



# 2 データ分割 ------------------------------------------

# データ分割
# ---｢訓練データ｣｢テストデータ｣に分割する
# --- childrenに対して層別サンプリング
set.seed(123)
splits      <- hotels %>% initial_split(strata = children)
hotel_other <- splits %>% training()
hotel_test  <- splits %>% testing()


# 確認
splits %>% print()
hotel_other %>% print()
hotel_test %>% print()


# ラベル頻度
# --- 訓練データ
# --- childrenの比率は維持されている
hotel_other %>%
  count(children) %>%
  mutate(prop = n / sum(n))


# ラベル頻度
# --- テストデータ
# --- childrenの比率は維持されている
hotel_test  %>%
  count(children) %>%
  mutate(prop = n / sum(n))


# 検証セットのセク性
# --- 訓練データをさらに｢訓練データ｣｢検証データ｣に分割
set.seed(234)
val_set <- hotel_other %>% validation_split(strata = children, prop = 0.80)
val_set %>% print()



# 3 モデル1: ロジスティック回帰 ------------------------------------


# 3.1 モデリング ------------------------------------------

# モデル構築
# --- ペナルティ付きロジスティック回帰
# --- チューニングあり
lr_mod <-
  logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")


# 休日定義
# --- step_holiday()で使用
holidays <-
  c("AllSouls", "AshWednesday", "ChristmasEve", "Easter",
    "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")


# レシピ作成
lr_recipe <-
  recipe(children ~ ., data = hotel_other) %>%
    step_date(arrival_date) %>%
    step_holiday(arrival_date, holidays = holidays) %>%
    step_rm(arrival_date) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())


# ワークフロー定義
lr_workflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(lr_recipe)



# 3.2 チューニング ------------------------------------------

# グリッドサーチの設定
# --- ｢ペナルティ｣をチューニング
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
lr_reg_grid %>% as.data.frame()
lr_reg_grid %>% top_n(-5)
lr_reg_grid %>% top_n(5)


# チューニング
# --- グリッドサーチ
lr_res <-
  lr_workflow %>%
    tune_grid(val_set,
              grid = lr_reg_grid,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(roc_auc))


# 確認
lr_res %>% print()



# 3.3 パフォーマンス評価 ------------------------------------------

# パフォーマンス指標のプロット
# --- ペナルティが小さいほどパフォーマンスが優れている
# --- 予測変数の大部分がモデルにとって重要であることを示唆している
# --- 最高のペナルティ値に向かって、ROC曲線の下の領域で急激な低下している
#     ペナルティによってモデルから全ての予測子が削除され、予測精度が急激に低下するために発生している
lr_res %>%
  collect_metrics() %>%
    ggplot(aes(x = penalty, y = mean)) +
    geom_point() +
    geom_line() +
    ylab("Area under the ROC Curve") +
    scale_x_log10(labels = scales::label_number())


# 候補モデル
# --- ROCが高位で安定している範囲のうちペナルティが最も大きいモデル
# --- 12行目のモデルがベスト
top_models <-
  lr_res %>%
  show_best("roc_auc", n = 15) %>%
  arrange(penalty)
top_models %>% print()


# ベストモデル
lr_best <-
  lr_res %>%
  collect_metrics() %>%
  arrange(penalty) %>%
  slice(12)
lr_best %>% print()


# プロット作成
lr_auc <-
  lr_res %>%
  collect_predictions(parameters = lr_best) %>%
  roc_curve(children, .pred_children) %>%
  mutate(model = "Logistic Regression")

lr_auc %>% autoplot()




# 4 モデル2: ランダムフォレスト ------------------------------------

# 4.1 モデリング ------------------------------------------

# 並列処理の設定
# --- コア数の取得
cores <- parallel::detectCores()
cores


# モデル構築
# --- チューニングあり(mtry, min_n)
# --- 並列処理の設定を追加
rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
    set_engine("ranger", num.threads = cores) %>%
    set_mode("classification")


# レシピ設定
rf_recipe <-
  recipe(children ~ ., data = hotel_other) %>%
    step_date(arrival_date) %>%
    step_holiday(arrival_date) %>%
    step_rm(arrival_date)


# 確認
# --- モデル＆レシピ
rf_mod %>% print()
rf_mod %>% parameters()
rf_recipe %>% print()


# ワークフロー設定
rf_workflow <-
  workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(rf_recipe)



# 4.2 チューニング ------------------------------------------

# バリデーションデータの確認
val_set
val_set$splits


# グリッドサーチ
# --- ｢val_set｣はロジスティック回帰モデルの際に定義したもの
set.seed(345)
rf_res <-
  rf_workflow %>%
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))


# 確認
# --- ｢val_set｣に以下の項目が追加されている
# --- ｢.metrics｣｢.notes｣｢.predictions｣
rf_res %>% print()
rf_res %>% collect_metrics() %>% as.data.frame()


# 4.3 パフォーマンス評価 ------------------------------------------

# 候補モデル
rf_res %>%
  show_best(metric = "roc_auc")


# プロット作成
rf_res %>% autoplot()


# ベストモデル
rf_best <-
  rf_res %>%
  select_best(metric = "roc_auc")
rf_best %>% print()


# 予測データの取得
rf_res %>%
  collect_predictions()


# プロット用データの作成
rf_auc <-
  rf_res %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(children, .pred_children) %>%
  mutate(model = "Random Forest")


# プロット作成
# --- チューニング結果の比較
# --- ロジスティック回帰 vs ランダムフォレスト
rf_auc %>%
  bind_rows(lr_auc) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
    geom_path(lwd = 1.5, alpha = 0.8) +
    geom_abline(lty = 3) +
    coord_equal() +
    scale_color_viridis_d(option = "plasma", end = .6)



# 5 最終モデル ------------------------------------

# ＜ポイント＞
# - ランダムフォレストモデルは、ペナルティ付きロジスティック回帰モデルよりも予測に優れている
# - チューニングプロセスで最適なモデルとハイパーパラメーターの値を選択することができた
#   ⇒テストデータでパフォーマンスを再評価


# 最終モデル定義
# --- チューニングで得たパラメータを設定
# --- ｢並列処理｣と｢変数重要度｣の設定を追加
last_rf_mod <-
  rand_forest(mtry = 8, min_n = 7, trees = 1000) %>%
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")


# ワークフローの更新
# --- モデルのみ更新
# --- レシピはそのまま(データはセットされていない)
last_rf_workflow <-
  rf_workflow %>%
  update_model(last_rf_mod)


# 最終モデルの学習
# --- テストデータに対して学習
# --- <37500/12500/50000> ⇒ 12500レコードのみ使用
set.seed(345)
last_rf_fit <-
  last_rf_workflow %>%
  last_fit(splits)


# 確認
last_rf_fit %>% print()


# モデルパフォーマンス
last_rf_fit %>% collect_metrics()


# 予測データ
# --- テストデータが使用されている(12500レコード)
# --- 予測データに正解ラベルも付属されている
last_rf_fit %>% collect_predictions()


# プロット作成
# --- 予測ラベルと正解ラベルからプロット作成
last_rf_fit %>%
  collect_predictions() %>%
  roc_curve(children, .pred_children) %>%
  autoplot()
