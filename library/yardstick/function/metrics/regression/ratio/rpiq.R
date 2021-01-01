# Title     : RPIQ（Ratio of performance to inter-quartile）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/2
# URL       : https://yardstick.tidymodels.org/reference/rpiq.html



# ＜ポイント＞
# -



# ＜構文＞
# rpiq(data, truth, estimate, na_rm = TRUE, ...)





library(tidyverse)
library(tidymodels)



# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()



#%% RPIQの算出 -------------------------------------------

# RPIQの算出
solubility_test %>%
  rpiq(truth = solubility, estimate = prediction)


# 計算証明
solubility_test %>%
  mutate(Nmerator    = (solubility - prediction)^2,
         Denominator = (solubility - mean(prediction))^2) %>%
  summarise(Nmerator    = sum(Nmerator),
            Denominator = sum(Denominator)) %>%
  mutate(rsq = 1 - Nmerator/ Denominator))




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
    rpiq(truth = solubility, estimate = prediction)


# 確認
metric_results %>% print()


# 全Foldの平均
metric_results %>%
  summarise(avg_estimate = mean(.estimate))



