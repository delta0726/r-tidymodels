# Title     : RPD（Ratio of performance to deviation）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/2
# URL       : https://yardstick.tidymodels.org/reference/rpd.html



# ＜ポイント＞
# - 変数の標準偏差と、特定のモデルによるその変数の予測の標準誤差との比率
# - レシオ化しているためモデル間での比較がしやすい



# ＜構文＞
# rpd(data, truth, estimate, na_rm = TRUE, ...)





library(tidyverse)
library(tidymodels)



# データ確認
# --- 回帰問題を想定したデータセット
solubility_test %>% as_tibble()


# プロット
solubility_test %>%
  ggplot(aes(x = solubility, y = prediction)) +
  geom_point()



#%% RPDの算出 -------------------------------------------

# RPDの算出
solubility_test %>%
  rpd(truth = solubility, estimate = prediction)


se <- function(x) sd(x, na.rm = TRUE)/sqrt(length(x))


# 計算証明
# --- 厳密な計算定義が分からない
solubility_test %>%
  mutate(Deviation   = (solubility - prediction)^2,
         Deviation2 = (solubility - mean(prediction))^2)  %>%
  summarise(Nmerator    = sd(Deviation, na.rm = TRUE),
            Denominator = se(Deviation2)) %>%
  mutate(rpd = Nmerator/ Denominator)




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
    rpd(truth = solubility, estimate = prediction)


# 確認
metric_results %>% print()


# 全Foldの平均
metric_results %>%
  summarise(avg_estimate = mean(.estimate))


