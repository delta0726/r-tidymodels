# Title     : リサンプリングでモデルを評価する
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/17
# URL       : https://www.tidymodels.org/start/resampling/


# ＜ポイント＞
# - モデルの予測精度を改善する方法としてクロスバリデーションを導入する
# - 訓練データ全てを用いて学習すると訓練データでは完璧な予測ができるが、検証データだと精度が落ちる
# - クロスバリデーションで訓練データをリサンプリングしたものを用いてモデリングする



# ＜目次＞
# 1 データ準備
# 2 データ分割
# 3 モデリング
# 4 パフォーマンス評価
# 5 クロスバリデーションの導入



# 1 データ準備 ---------------------------------------------------


library(tidyverse)
library(tidymodels)
library(modeldata)


# データロード
data(cells, package = "modeldata")


# データ概要
# --- 58変数
# --- Y:class
cells %>% print()
cells %>% glimpse()


# 比率確認
# --- PS / WS
cells %>%
  count(class) %>%
  mutate(prop = n/sum(n))

## A tibble: 2 x 3
#  class     n  prop
#  <fct> <int> <dbl>
#1 PS     1300 0.644
#2 WS      719 0.356



# 2 データ分割 ---------------------------------------------------

# ＜ポイント＞
# - データ分割で層別サンプリングを実施する
#   --- 訓練データとテストデータで分類ラベルの比率が元データと同じになるようにする
#   --- 不均衡ラベル問題では必須
#   --- initial_split())でstrata引数を指定


# データ分割
# --- classの割合が再現されるように層別サンプリングする
set.seed(123)
cell_split <- cells %>% select(-case) %>% initial_split(strata = class)
cell_split %>% print()


# 分割データの格納
cell_train <- cell_split %>% training()
cell_test  <- cell_split %>% testing()


# レコード数
cell_train %>% nrow()
nrow(cell_train) / nrow(cells)


# classの内訳
# --- 訓練データ
cell_train %>%
  count(class) %>%
  mutate(prop = n/sum(n))


# classの内訳
# --- テストデータ
cell_test %>%
  count(class) %>%
  mutate(prop = n/sum(n))



# 3 モデリング ---------------------------------------------------

# ＜ポイント＞
# - ランダムフォレスト(RF)を用いてモデル構築を行う
# - RFはデータ基準化や不要ラベルの削除などの前処理をしなくてもモデルがワークする
#   --- 今回は{recipes}による前処理は省略する
# - ハイパーパラメータをチューニングしなくても、ある程度妥当な結果を返す
#   --- デフォルトのパラメータで実行する


# モデル構築
rf_mod <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")


# モデル学習
# --- 訓練データをすべて用いて学習
set.seed(234)
rf_fit <-
  rf_mod %>%
  fit(class ~ ., data = cell_train)


# 確認
rf_fit %>% print()



# 4 パフォーマンス評価 ------------------------------------------------

# ＜ポイント＞
# - 訓練データのパフォーマンスが高く、テストデータのパフォーマンスが低くなるのは当たり前
#   --- Holdout法によるデータ分割に基づいて評価
# - ランダムフォレストやニューラルネットワークを用いると常にほぼ完全な予測が得られる
#   --- テストデータのベンチマークとして適切ではない
#   --- クロスバリデーションを用いることで、パフォーマンス評価を行う


# 4-1 訓練データ ------------------------------------

# 予測データの作成
# --- 訓練データを用いて予測（完全予測）
# --- 予測確率と正解値を追加
rf_training_pred <-
  rf_fit %>%
    predict(cell_train) %>%
    bind_cols(predict(rf_fit, cell_train, type = "prob")) %>%
    bind_cols(cell_train %>% select(class))


# 確認
rf_training_pred %>% print()


# ROC-AUC
# --- 完全予測できていることを確認
rf_training_pred %>% roc_auc(truth = class, .pred_PS)


# Accuracy
# --- 完全予測できていることを確認
rf_training_pred %>% accuracy(truth = class, .pred_class)



# 4-2 テストデータ ------------------------------------

# 予測データの作成
# --- 検証データを用いて予測
# --- 予測確率と正解値を追加
rf_testing_pred <-
  rf_fit %>%
    predict(cell_test) %>%
    bind_cols(predict(rf_fit, cell_test, type = "prob")) %>%
    bind_cols(cell_test %>% select(class))


# 確認
rf_testing_pred %>% print()


# ROC-AUC
# --- 推定精度が落ちることを確認
rf_testing_pred %>% roc_auc(truth = class, .pred_PS)


# Accuracy
# --- 推定精度が落ちることを確認
rf_testing_pred %>% accuracy(truth = class, .pred_class)




# 5 クロスバリデーションの導入 ------------------------------------

# ＜ポイント＞
# - Holdout法による訓練データのパフォーマンス指標は完全にフィッティングしている
# - モデルのパフォーマンス評価はクロスバリデーションで行うのが一般的
#   ---- 現実的なパフォーマンスが表示される
#   ---- クロスバリデーションのパフォーマンスとテストデータを比較することで汎化性能を確認



# クロスバリデーション
# --- 訓練データを10foldのセットに分割
set.seed(345)
folds <- cell_train %>% vfold_cv(v = 10)
folds %>% print()


# ワークフローの設定
# --- ｢モデル｣と｢フォーミュラ｣を設定
# --- 今回はレシピは使っていない
rf_wf <-
  workflow() %>%
    add_model(rf_mod) %>%
    add_formula(class ~ .)


# モデル学習
# --- 全てのFoldデータに対してモデルをフィッティングさせてパフォーマンス評価する
# --- tune::fit_resamples()
set.seed(456)
rf_fit_rs <- rf_wf %>% fit_resamples(folds)
rf_fit_rs %>% print()


# パフォーマンス統計量を取得
# --- 訓練データでもクロスバリデーションをすることでStep3-2と同程度の水準に落ち着いている
# --- 現実的なモデルでおーばフィットが回避できている
rf_fit_rs %>% collect_metrics()


# テストデータのパフォーマンス統計量
# --- 比較用
rf_testing_pred %>% accuracy(truth = class, .pred_class)
rf_testing_pred %>% roc_auc(truth = class, .pred_PS)
