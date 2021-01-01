# Title     : MAE (Mean absolute error)
# Objective : TODO
# Created by: Owner
# Created on: 2020/07/01
# URL       : https://yardstick.tidymodels.org/reference/mae.html



# ＜ポイント＞
# - MAEは予測誤差の絶対値平均を求めたもの
#   --- 小さいほど良好（誤差指標なので小さいほうがよい）
# - 誤差は正規分布に従うので、正確な評価ができる
# - 元のデータの単位が保存される（RSQのように正規化されない）



# ＜参考＞
# 誤差関数
# https://qiita.com/Hatomugi/items/d00c1a7df07e0e3925a8


# ＜構文＞
# mae(data, truth, estimate, na_rm = TRUE, ...)





library(tidyverse)
library(tidymodels)


# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()



#%% MAEの算出 -------------------------------------------

# MAEの算出
solubility_test %>%
  mae(truth = solubility, estimate = prediction)


# 計算証明
solubility_test %>%
  mutate(Nmerator    = abs(solubility - prediction)) %>%
  summarise(Nmerator    = sum(Nmerator),
            Denominator = n()) %>%
  mutate(mae = Nmerator/ Denominator)




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
    mae(truth = solubility, estimate = prediction)


# 確認
metric_results %>% print()


# 全Foldの平均
metric_results %>%
  summarise(avg_estimate = mean(.estimate))


