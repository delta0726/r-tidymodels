# Title     : precision（適合率）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/1
# URL       : https://yardstick.tidymodels.org/reference/precision.html



# ＜ポイント＞
# - TP / (TP + FP)
# - 〇(P)と予想して、正解(T)だった割合
# - 誤検知を少なくしたい場合に重視
# - Recallとトレードオフ


# ＜解釈＞
# - 予想においてFPが少ないほど価値が高まる
#   --- 常に〇(P)と予想すると、TPは確実に当てることができる
#   --- 適合率を高めるためにはFPを減らす必要がある
#   --- 例：｢退職｣と予測した人のうち、どれだけ実際に｢退職｣したか


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
# precision(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...)





library(tidyverse)
library(tidymodels)



#%% 2クラスの分類問題 -------------------------------------------

# データ準備
data("two_class_example")


# データ確認
two_class_example %>% as_tibble()


# 適合率の計算
two_class_example %>%
  precision(truth, predicted)



#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 混合行列
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted)


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  precision(obs, pred)



#%% グループごとに算出 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  precision(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  precision(obs, pred, estimator = "macro_weighted")

