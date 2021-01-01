# Title     : RSQ_TRAD (R squared traditional)
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/4
# URL       : https://yardstick.tidymodels.org/reference/rsq_trad.html



# ＜ポイント＞
# - 性質がよくわからないが、使うことはなさそう


# ＜構文＞
# rsq_trad(data, truth, estimate, na_rm = TRUE, ...)



library(tidyverse)
library(tidymodels)



# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()



#%% RSQの算出 -------------------------------------------

# RSQ-TRADの算出
solubility_test %>%
  rsq_trad(truth = solubility, estimate = prediction)

# 参考：RSQの算出
solubility_test %>%
  rsq(truth = solubility, estimate = prediction)


# 計算証明
solubility_test %>%
  mutate(Nmerator    = (solubility - prediction)^2,
         Denominator = (solubility - mean(prediction))^2) %>%
  summarise(Nmerator    = sum(Nmerator),
            Denominator = sum(Denominator)) %>%
  mutate(rsq = 1 - Nmerator/ Denominator)




#%% RSQとの比較-------------------------------------------

# ＜ポイント＞
# - データのばらつき具合によって結果がことなる？


set.seed(2291)
solubility_test$randomized <- solubility_test$prediction %>% sample()
solubility_test %>% print()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()

# RSQ
# --- 元データ
# --- 結果が一致
solubility_test %>% rsq(solubility, prediction)
solubility_test %>% rsq_trad(solubility, prediction)

# プロット
# --- リサンプル後
solubility_test %>%
  ggplot(aes(x = solubility, y = randomized)) +
  geom_point()

# RSQ
# --- リサンプル後
# --- 結果が異なる
solubility_test %>% rsq(solubility, randomized)
solubility_test %>% rsq_trad(solubility, randomized)



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
    rsq_trad(truth = solubility, estimate = prediction)


# 確認
metric_results %>% print()


# 全Foldの平均
metric_results %>%
  summarise(avg_estimate = mean(.estimate))


