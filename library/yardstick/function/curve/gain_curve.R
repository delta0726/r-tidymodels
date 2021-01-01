# Title     : gain_curve
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       :https://yardstick.tidymodels.org/reference/gain_curve.html




# ＜ポイント＞
# - 分類問題(2クラス、マルチクラス)における評価指標
# - モデルを導入することによる改善効果を示す（モデルを導入しないランダム分類の場合と比較）
# - データセットの各パーセンタイルにおいて、ランダム選択した場合と比較してパフォーマンスがどの程度向上すると予想
# - ベースラインはY=Xの45度線


# ＜構文＞
# gain_curve(data, truth, ..., na_rm = TRUE)
# - truth引数は必須
# -...にはクラス確率を指定（2クラスの場合は1つのクラス確率、マルチクラスの場合は全てのクラス確率）


# ＜出力＞
#.n             ：サンプルのインデックス
#.n_events      ：一意のサンプルのインデックス（繰り返し推定値を持つ場合）
#.percent_tested：テストされた値の累積パーセンテージ
#.percent_found ：真の結果の総数に対する真の結果の累積パーセンテージ。


# ＜参考資料＞
# https://www.datarobot.com/jp/blog/2018-02-07-liftchart/
# https://docs.microsoft.com/ja-jp/azure/machine-learning/how-to-understand-automated-ml




library(tidyverse)
library(tidymodels)


# 2クラス -------------------------------------------------------

# データロード
data(two_class_example)


# データ確認
two_class_example %>% as_tibble()
two_class_example %>% glimpse()


# チャート用データ
# --- 2クラスの場合は1つのクラス確率
two_class_example %>%
  gain_curve(truth = truth, Class1)


# プロット
two_class_example %>%
  gain_curve(truth = truth, Class1) %>%
  autoplot()



# マルチクラス ------------------------------------------------

# データロード
data(hpc_cv)
hpc_cv <- hpc_cv %>% as_tibble()


# データ確認
hpc_cv %>% as_tibble()
hpc_cv %>% glimpse()
hpc_cv %>% group_by(Resample, obs, pred) %>% tally()


# チャート用データ
# --- マルチクラスの場合はクラスごとに表示される
# --- グループがある場合はグループ別に表示される
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  gain_curve(obs, VF:L)


# プロット
hpc_cv %>%
  group_by(Resample) %>%
  gain_curve(obs, VF:L) %>%
  autoplot()
