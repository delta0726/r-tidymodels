# Title     : recall（再現率）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/1
# URL       : https://yardstick.tidymodels.org/reference/recall.html



# ＜ポイント＞
# - TP / (TP + FN)
# - ｢予想(Positive)｣から見た｢正解(T)｣の割合
# - 正例を見逃したくない場合に重視
# - Precisionとトレードオフ


# ＜解釈＞
# - 正解(T)のうち、正しく〇(P)と予測できた割合
#   --- 常に〇(P)と予想すると、TPは確実に当てることができる
#   --- 再現率を高めるためにはFNを減らす必要がある
#   --- 例：実際に｢退職｣した人のうち、どれだけ｢退職｣と予測できたか


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
# recall(data, truth, estimate, estimator = NULL, na_rm = TRUE, ...)




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


# 再現率の計算
two_class_example %>%
  recall(truth = truth, predicted)



#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  recall(obs, pred)



#%% グループごとに算出 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  recall(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  recall(obs, pred, estimator = "macro_weighted")


