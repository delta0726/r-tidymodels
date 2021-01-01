# Title     : Metric types
# Objective : TODO
# Created by: Owner
# Created on: 2020/6/30
# URL       : https://yardstick.tidymodels.org/articles/metric-types.html



# ＜ポイント＞
# - メトリックは｢数値｣｢クラス｣｢クラス確率｣の３種類に分類される
# - yardstickでは全て同じ形式でインプットとアウトプットが行われる
#   --- インプットは｢truth｣と｢estimate｣で共通化されている
# - class_metrics()を使用することで、複数メトリックの算出も可能
#   --- ｢truth｣と｢estimate｣のデータ型で問題の種類を判別する




library(yardstick)
library(dplyr)
library(ggplot2)



# データ準備
# --- VF:Lには確率が入っている
# --- リサンプリングが行われている
data("hpc_cv")
hpc_cv <- hpc_cv %>% as_tibble()
hpc_cv %>% as_tibble()


# 確認
hpc_cv %>%
  group_by(Resample) %>%
  tally()



# パターン1：単一のFoldを1メトリックで集計 ---------------------------

# 対象Foldをフィルタして関数をそのまま適用
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  accuracy(truth = obs, estimate = pred)



# パターン2：複数グループ(Fold)を1メトリックで集計 ---------------------------

# グループ化して関数をそのまま適用
hpc_cv %>%
  group_by(Resample) %>%
  accuracy(truth = obs, estimate = pred)



# パターン3：複数グループ(Fold)を複数メトリックで集計 ---------------------------

# 複数メトリックをセット化
class_metrics <- metric_set(accuracy, kap)
class_metrics %>% as_tibble()
class_metrics %>% class()


# グループ化して関数をそのまま適用
hpc_cv %>%
  group_by(Resample) %>%
  class_metrics(truth = obs, estimate = pred)