# Title     : pr_curve (Precision recall curve)
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       : https://yardstick.tidymodels.org/reference/pr_curve.html



# ＜ポイント＞
# - PR曲線とはrecallrecall(再現率)に対するprecitionprecition(適合率)をプロットしたもの
# - precisionで｢確からしさ｣を担保しながら、recallで｢網羅性｣を確保するというのが理想的


# ＜構文＞
# pr_curve(data, truth, ..., na_rm = TRUE)
# - truth引数は必須
# -...にはクラス確率を指定（2クラスの場合は1つのクラス確率、マルチクラスの場合は全てのクラス確率）


# ＜出力＞
# .threshold ：サンプルのインデックス
# recall     ：再現率 TP/(TP+FN)  ※正解だったうち、Trueを当てた比率
# precision  ：適合率 TP/(TP+FP)  ※〇(Positive)と予想して実際に正解(〇)だった比率



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
  pr_curve(truth = truth, Class1) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_point()


# プロット
two_class_example %>%
  pr_curve(truth = truth, Class1) %>%
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
  pr_curve(obs, VF:L) %>%
  autoplot()


# 複数グループ
# プロット
hpc_cv %>%
  group_by(Resample) %>%
  pr_curve(obs, VF:L) %>%
  autoplot()



# .thresholdについて --------------------------------------

# .thresholdはベースラインを示す
two_class_example %>%
  pr_curve(truth = truth, Class1) %>%
  pivot_longer(c(".threshold", "precision"), names_to = "y") %>%
  ggplot(aes(x = recall, y = value, color = y)) +
  geom_point() +
  ylab("precision")



