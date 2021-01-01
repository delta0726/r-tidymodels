# Title     : Get Started
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/30
# URL       : https://yardstick.tidymodels.org/index.html



# ＜ポイント＞
# - yardstickの関数はモデル予測値のデータフレームを起点とする
#   --- {yardstick}は{parsnip}や{rsample}からの出力値と連携するように作られている
# - 関数の引数は全て同じ構造になっている
# - ｢回帰問題｣｢分類問題(2クラス)｣｢分類問題(マルチクラス)｣の３パターンに別けて整理する




library(yardstick)
library(dplyr)
library(ggplot2)


#%% 分類データ(2クラス) -----------------------------------------------

# データ準備
data("two_class_example")
two_class_example <- two_class_example %>% as_tibble()


# 分類データ
# --- 2クラスのデータ
two_class_example %>% print()


# データ集計
# --- クロス集計
two_class_example %>% select(truth, predicted) %>% table()


# メトリックの出力
# --- metrics()はデータ種別に応じて予め指定された統計値を出力する
# --- truthがファクター値なので、accuracyとkapが出力される
two_class_example %>%
  metrics(truth = truth, estimate = predicted)




#%% 分類データ(マルチクラス) -----------------------------------------------

# データ準備
data("hpc_cv")
hpc_cv <- hpc_cv %>% as_tibble()


# 分類データ
# --- マルチクラス
hpc_cv %>% print()


# データ集計
# --- クロス集計
hpc_cv %>% select(obs, pred) %>% table()


# 適合率(Precision)
hpc_cv %>% precision(truth = obs, estimate = pred)


# 適合率(Precision)
hpc_cv %>% precision(truth = obs, estimate = pred, estimator = "micro")




#%% リサンプリングにおけるメトリックの計算 -------------------------------------

# データ準備
# --- VF:Lには確率が入っている
data("hpc_cv")
hpc_cv <- hpc_cv %>% as_tibble()
hpc_cv %>% as_tibble()


# Foldごとにメトリックを一括計算
hpc_cv %>%
  group_by(Resample) %>%
  roc_auc(truth = obs, VF:L)




#%% Autoplotによる可視化 -------------------------------------

# データ準備
# --- VF:Lには確率が入っている
data("hpc_cv")
hpc_cv <- hpc_cv %>% as_tibble()
hpc_cv %>% as_tibble()


# プロット作成
hpc_cv %>%
  group_by(Resample) %>%
  roc_curve(truth = obs, VF:L) %>%
  autoplot()
