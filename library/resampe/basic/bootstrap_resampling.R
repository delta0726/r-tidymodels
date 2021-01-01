# Title     : Bootstrap Resampling
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/09
# URL       : https://rsample.tidymodels.org/articles/Working_with_rsets.html



# ＜ポイント＞
# - ブートストラップはリサンプリングを行うことで統計的推論(信頼区間など)を行う
#
#


library(tidyverse)
library(rsample)


# データロード
data("attrition", package = "modeldata")


# データ概要
attrition %>% as_tibble()
attrition %>% glimpse()
attrition %>% names()



#%% 問題提起 -----------------------------------------------

# プロット作成
# --- Male/Femaleで収入に差はあるのだろうか？
attrition %>%
  ggplot(aes(x = Gender, y = MonthlyIncome)) +
    geom_boxplot() +
    scale_y_log10()



#%% ブートストラップ -----------------------------------------------

# ブートストラップ・リサンプリング
set.seed(353)
bt_resamples <- attrition %>% bootstraps(times = 500)
bt_resamples %>% print()


# 1つのサンプリングの確認
bt_resamples$splits[[1]]
bt_resamples$splits[[1]] %>% training() %>% dim()
bt_resamples$splits[[1]] %>% testing() %>% dim()





#%% 統計的分析 -----------------------------------------------


# 関数定義
# --- Male/Femaleの収入の中央値の差
median_diff <- function(splits) {
  x <- analysis(splits)
  median(x$MonthlyIncome[x$Gender == "Female"]) -
      median(x$MonthlyIncome[x$Gender == "Male"])
}


# サンプリングごとに関数適用
bt_resamples$wage_diff <-
  bt_resamples$splits %>%
    map_dbl(median_diff)


# 確認
bt_resamples %>% print()


# プロット
bt_resamples %>%
  ggplot(aes(x = wage_diff)) +
    geom_line(stat = "density", adjust = 1.25) +
    xlab("Difference in Median Monthly Income (Female - Male)")


# 分位点
bt_resamples$wage_diff %>% quantile(probs = c(0.025, 0.975))
#>  2.5% 97.5%
#>  -189   615


# モデル構築
# --- 一般化線形モデルの回帰係数を取得
glm_coefs <- function(splits, ...) {
  ## use `analysis` or `as.data.frame` to get the analysis data
  mod <- glm(..., data = analysis(splits), family = binomial)
  as.data.frame(t(coef(mod)))
}


# モデル適用
bt_resamples$betas <- map(.x = bt_resamples$splits,
                          .f = glm_coefs,
                          mod_form)

# 確認
bt_resamples %>% print()
bt_resamples$betas[[1]]

