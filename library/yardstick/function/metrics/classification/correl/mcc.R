# Title     : mcc（Matthews correlation coefficient）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/21
# URL       : https://yardstick.tidymodels.org/reference/mcc.html



# ＜ポイント＞
# - 不均衡データに対して使われる
# - 数が少ないクラスを正にする習慣ができている場合は｢F1スコア｣｢リコール｣｢プレシジョン｣などの指標で評価してよい
# - 混同行列なしに指標だけでモデルの精度を評価する時は、F1スコアではなく、マシューズ相関係数をみることをおすす
# - 混同行列の相関みたいなもので、完璧に当てることができた理想のテーブルと予測結果のテーブルがどれくらい一致しているかを見ています


# ＜参考＞
# https://www.datarobot.com/jp/blog/matthews-correlation-coefficient/



# ＜構文＞
# mcc(data, truth, estimate, na_rm = TRUE, ...)







library(tidyverse)
library(tidymodels)



#%% 2クラスの分類問題 -------------------------------------------

# データ準備
data("two_class_example")


# データ確認
two_class_example %>% as_tibble()



# 再現率の計算
two_class_example %>%
  mcc(truth = truth, predicted)




#%% マルチクラスの分類問題 -------------------------------------------


# データ準備
data("hpc_cv")


# データ確認
hpc_cv %>% as_tibble()


# 出力
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  mcc(obs, pred)



#%% グループごとに算出 -------------------------------------------

# グループごとに算出
hpc_cv %>%
  group_by(Resample) %>%
  mcc(obs, pred)


# 集計ウエイトを変更
hpc_cv %>%
  group_by(Resample) %>%
  mcc(obs, pred, estimator = "macro_weighted")