# Title     : accuracy
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       : https://yardstick.tidymodels.org/reference/accuracy.html



# ＜ポイント＞
# - 正解率(Accuracy)は全体の判断のうち、TPとTNの占める割合を意味する
# - (TP + TN) / (TP + FN + FP + TN)


# ＜分類定義＞
# TP: 〇(P)と予想して正解(T)
# FN: ✕(N)と予想して不正解(F)
# FP: 〇(P)と予想して不正解(F)
# TN: ✕(N)と予想して正解(T)
# 前半の文字が真実(Truth)、後半の文字が予想(Estimate)を示す

# TP | FP
# --------
# FN | TN



# ＜参考＞
# https://www.datarobot.com/jp/blog/
# データサイエンス ⇒ モデル最適騎亜指標・評価指標の選び方


# ＜構文＞
# accuracy(data, truth, estimate, na_rm = TRUE, ...)




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


# Accuracyの計算
two_class_example %>%
  accuracy(truth = truth, estimate = predicted)




#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  accuracy(obs, pred)



#%% グループ別 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  accuracy(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  accuracy(obs, pred, estimator = "macro_weighted")
