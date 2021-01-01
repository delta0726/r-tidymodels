# Title     : threshold_perf
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/2
# URL       : https://probably.tidymodels.org/reference/threshold_perf.html



# ＜ポイント＞
# -



# ＜構文＞
# threshold_perf(.data, ...)
#
## S3 method for data.frame
# threshold_perf(.data, truth, estimate, thresholds = NULL, na_rm = TRUE, ...)


# 1.準備 ----------------------------------------------------------------

library(tidyverse)
library(probably)


# データ準備
data("segment_logistic")


# データ確認
# --- 分類問題のpredict()の出力を想定したもの
segment_logistic %>% print()
segment_logistic %>% glimpse()



# 2.閾値を指定 ------------------------------------------------------

segment_logistic %>%
  threshold_perf(truth = Class,
                 estimate = .pred_good,
                 thresholds = 0.6)




# 3.複数の閾値を指定 -------------------------------------------------

thresholds <- seq(0.5, 0.9, by = 0.1)

segment_logistic %>%
  threshold_perf(Class, .pred_good, thresholds)





# 4.リサンプリングと複数閾値を指定 -------------------------------------------------

resamples <- 5

mock_resamples <-
  resamples %>%
    replicate(expr = sample_n(segment_logistic, 100, replace = TRUE),
              simplify = FALSE) %>%
    bind_rows(.id = "resample")



resampled_threshold_perf <-
  mock_resamples %>%
    group_by(resample) %>%
    threshold_perf(Class, .pred_good, thresholds)


# 確認
resampled_threshold_perf %>% print()


# 集計
# --- 平均値
resampled_threshold_perf %>%
  group_by(.metric, .threshold) %>%
  summarise(.estimate = mean(.estimate))