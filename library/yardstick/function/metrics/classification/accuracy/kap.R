# Title     : kap（kappa）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/2
# URL       : https://yardstick.tidymodels.org/reference/kap.html



# ＜ポイント＞
# - 1つの事象を2人が観察し分類した際に、結果がどの程度一致しているか
#   --- ｢予想｣｢正解｣を2人に置き換える
# - ObservedからExpectedを調整薄る



# ＜課題＞
# - 3クラス以上の場合に、近い値でも遠い値でも同様にハズレとして評価されてしまい望ましくない。
#   --- 医師がGrade1だGrade2だと争うよりも、Grade1だGrade3だと争うほうが一致率は低いはず
#


# ＜構文＞
# kap(data, truth, estimate, na_rm = TRUE, ...)


# ＜参考＞
# https://blog.attelu.jp/entry/predict-index
# https://qiita.com/tand826/items/4d1fb2045f2b48d21b7d



library(tidyverse)
library(tidymodels)



#%% 2クラスの分類問題 -------------------------------------------

# データ準備
data("two_class_example")


# データ確認
two_class_example %>% as_tibble()


# 混合行列
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted)


# bal_Accuracyの計算
two_class_example %>%
  kap(truth = truth, estimate = predicted)



#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  kap(obs, pred)



#%% グループ別 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  kap(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  kap(obs, pred, estimator = "macro_weighted")