# Title     : metrics
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       : https://yardstick.tidymodels.org/reference/metrics.html



# ＜ポイント＞
# - データ種別応じて基本的なメトリックを出力する
# - メトリックは以下のようにデフォルト設定されている



# ＜構文＞
# metrics(data, truth, estimate, ..., options = list(), na_rm = TRUE)



# ＜出力パターン＞
# truth = ファクター
# accuracy, kap

# truth = ファクター(2クラス)、 1つのクラス確率
# mn_log_loss, roc_auc

# truth = ファクター(マルチクラス)、 1つのクラス確率
# mn_log_loss, roc_auc

# truth = 数値
# rmse, rsq, mae



library(dplyr)
library(yardstick)
library(DataExplorer)



#%% 分類問題(2クラス) ----------------------------------------------


# データ準備
two_class_example %>% as_tibble()


# データ確認
two_class_example %>%
  conf_mat(truth = truth, estimate = predicted)


# 検査値がファクターの場合
# --- accuracy()とkap()が実行される
two_class_example %>%
  metrics(truth = truth, estimate = predicted)


# 検査値にクラス確率を追加
# --- truth引数とestimate引数以外に、クラス確率を選択
# --- クラス確率を評価する｢mn_log_loss｣と｢roc_auc｣が実行される
two_class_example %>%
  metrics(truth = truth, estimate = predicted, Class1)





#%% 回帰問題 --------------------------------------------------------

# データ準備
solubility_test %>% as_tibble()


# データ確認
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()


# 検査値が数値の場合
# --- rmse()とrsq()とmae()が実行される
solubility_test %>%
  metrics(truth = solubility, estimate = prediction)



#%% グループ化 ---------------------------------------------------

# グループごとにメトリックを集計
hpc_cv %>%
  group_by(Resample) %>%
  metrics(truth = obs, estimate = pred, VF:L) %>%
  print(n = 40)



