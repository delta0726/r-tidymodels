# Title     : RMSE (Root mean squared error)
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/30
# URL       : https://yardstick.tidymodels.org/reference/rmse.html



# ＜ポイント＞
# - RMSEは予測誤差を二乗して平均して1サンプルあたりの平均誤差量を求めたもの
#   --- 小さいほど良好（誤差指標なので小さいほうがよい）
# - 誤差は正規分布に従うので、正確な評価ができる
# - 元のデータの単位が保存される（RSQのように正規化されない）
# - 大きな間違いをより重要視（大きな価格の誤差を許容できないケースに使用）
# - 損失関数と利用すると平均値に寄りやすくなる
# - 回帰問題においては概ね万能



# ＜構文＞
# rmse(data, truth, estimate, na_rm = TRUE, ...)



library(tidyverse)
library(tidymodels)



# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()



#%% RMSEの算出 -------------------------------------------

# RMSEの算出
solubility_test %>%
  rmse(truth = solubility, estimate = prediction)


# 計算証明
solubility_test %>%
  mutate(diff = (solubility - prediction)^2) %>%
  summarise(diff_sum = sum(diff)) %>%
  mutate(n = length(solubility_test$prediction),
         rmse = sqrt(diff_sum / n))




#%% グループ集計 -------------------------------------------


# パラメータ設定
set.seed(1234)
size <- 100
times <- 10


# リサンプルデータの作成
# --- solubility_testから100個抽出、10回試行
solubility_resampled <- bind_rows(
  replicate(
    n = times,
    expr = sample_n(solubility_test, size, replace = TRUE),
    simplify = FALSE
  ),
  .id = "resample"
)


# 確認
solubility_resampled %>% as_tibble()
solubility_resampled %>% group_by(resample) %>% tally()


# グループごとに算出
metric_results <-
  solubility_resampled %>%
    group_by(resample) %>%
    rmse(truth = solubility, estimate = prediction)


# 確認
metric_results %>% print()


# 全Foldの平均
metric_results %>%
  summarise(avg_estimate = mean(.estimate))