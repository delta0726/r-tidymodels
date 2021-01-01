# Title     : roc_curve
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       : https://yardstick.tidymodels.org/reference/roc_curve.html





# ＜ポイント＞
# -



# ＜構文＞
# roc_curve(data, truth, ..., options = list(), na_rm = TRUE)
# - truth引数は必須
# -...にはクラス確率を指定（2クラスの場合は1つのクラス確率、マルチクラスの場合は全てのクラス確率）


# ＜出力＞
# .threshold  ：サンプルのインデックス
# specificity ：一意のサンプルのインデックス（繰り返し推定値を持つ場合）
# sensitivity ：テストされた値の累積パーセンテージ


# ＜参考資料＞
# https://qiita.com/g-k/items/b47b9b0ee2015a3b0b94




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
  roc_curve(truth = truth, Class1)


# プロット
two_class_example %>%
  roc_curve(truth = truth, Class1) %>%
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
  roc_curve(obs, VF:L) %>%
  autoplot()


# プロット
hpc_cv %>%
  group_by(Resample) %>%
  roc_curve(obs, VF:L) %>%
  autoplot()


# .thresholdについて --------------------------------------

# .thresholdはベースラインを示す
two_class_example %>%
  roc_curve(truth = truth, Class1) %>%
  pivot_longer(c(".threshold", "sensitivity"), names_to = "y") %>%
  ggplot(aes(x = specificity, y = value, color = y)) +
  geom_point() +
  ylab("specificity")

