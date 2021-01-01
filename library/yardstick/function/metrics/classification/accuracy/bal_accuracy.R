# Title     : bal_accuracy（Balanced accuracy）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       : https://yardstick.tidymodels.org/reference/bal_accuracy.html



# ＜ポイント＞
# - ｢Balanced accuracy｣は不均衡データに対するAccuracyの性能評価があまり適切ではないという点を考慮した評価指標
# - 不均衡データはAccuracyが基本的に高くなってしまう
# - ｢Balanced accuracy｣は、正例を正しく予測できる割合と負例を正しく予測できる割合の平均をとっている
#   --- sens()とspec()の平均値


# ＜分類定義＞
# TP: 〇(P)と予想して正解(T)
# FN: ✕(N)と予想して不正解(F)
# FP: 〇(P)と予想して不正解(F)
# TN: ✕(N)と予想して正解(T)
# 前半の文字が真実(Truth)、後半の文字が予想(Estimate)を示す

# TP | FP
# --------
# FN | TN



# ＜構文＞
# bal_accuracy(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...)


# ＜参考＞
# - https://buildersbox.corp-sansan.com/entry/2019/10/17/110000
# - https://statisticaloddsandends.wordpress.com/2020/01/23/what-is-balanced-accuracy/



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
  bal_accuracy(truth = truth, estimate = predicted)


# 計算証明
x1 <- two_class_example %>% sens(truth = truth, estimate = predicted)
x2 <- two_class_example %>% spec(truth = truth, estimate = predicted)
x1 %>% bind_rows(x2) %>% pull(.estimate) %>% mean()




#%% マルチクラスの分類問題 -------------------------------------------

# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  bal_accuracy(obs, pred)



#%% グループ別 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  bal_accuracy(obs, pred)


# グループ別に出力
hpc_cv %>%
  group_by(Resample) %>%
  bal_accuracy(obs, pred, estimator = "macro_weighted")


