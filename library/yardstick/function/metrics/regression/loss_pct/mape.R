# Title     : MAPE (Mean absolute percent error)
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/1
# URL       : https://yardstick.tidymodels.org/reference/mape.html




# ＜ポイント＞
# - MAPEは観測誤差をパーセント化することを試みた指標
#   --- パーセント化の際に以下の問題が生じる
#   --- 結果として使えない指標


# ＜MAPEの問題点＞
# - 実測値がゼロの場合にでInf(無限大)となる
# - マイナス誤差に対してより重いペナルティを貸してしまう


# ＜参考＞
# https://qiita.com/japanesebonobo/items/ad51cbbf36236b023df0



# ＜構文＞
# mape(data, truth, estimate, na_rm = TRUE, ...)





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



#%% MAPEの算出 -------------------------------------------

# MAPEの算出
solubility_test %>%
  mape(truth = solubility, estimate = prediction)


# 計算証明
solubility_test %>%
  mutate(Error     = abs((solubility - prediction) / solubility),
         Error_mod = ifelse(is.finite(Error), Error, 0)) %>%
  summarise(Error     = sum(Error),
            Error_mod = sum(Error_mod),
            n = n()) %>%
  mutate(mape     = 100 / n * Error,
         mape_mod = 100 / n * Error_mod)




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
    mape(truth = solubility, estimate = prediction)


# 確認
metric_results %>% print()


# 全Foldの平均
metric_results %>%
  summarise(avg_estimate = mean(.estimate))


