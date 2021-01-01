# Title     : SMAPE (Mean absolute percent error)
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/1
# URL       : https://yardstick.tidymodels.org/reference/mape.html





# ＜ポイント＞
# - 実測値がゼロの場合にInf(無限大)となる問題を解消した指標
#   --- 分子を絶対値誤差にすることで無限大になるのを防いでいる


# ＜MAPEの問題点＞
# https://qiita.com/japanesebonobo/items/ad51cbbf36236b023df0


# ＜構文＞
# smape(data, truth, estimate, na_rm = TRUE, ...)





library(tidyverse)
library(parsnip)
library(rsample)


# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()



#%% SMAPEの算出 -------------------------------------------

# SMAPEの算出
solubility_test %>%
  smape(truth = solubility, estimate = prediction)


# 計算証明
solubility_test %>%
  mutate(Nmerator    = abs(solubility - prediction),
         Denominator = (abs(solubility) + abs(prediction)) / 2,
         Temp        = Nmerator / Denominator) %>%
  summarise(Temp = sum(Temp),
            n    = n()) %>%
  mutate(smape = 100 / n * Temp)



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
    smape(truth = solubility, estimate = prediction)


# 確認
metric_results %>% print()


# 全Foldの平均
metric_results %>%
  summarise(avg_estimate = mean(.estimate))


