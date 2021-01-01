# Title     : expo_decay
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/17
# URL       : https://tune.tidymodels.org/reference/expo_decay.html



# ＜ポイント＞
# - 指数関数的に減衰や逓増する数列を作成することができる
# - ベイズ最適化の反復処理の中でパラメータを動的に設定するために使用する


# ＜構文＞
# expo_decay(iter, start_val, limit_val, slope = 1/5)




library(tibble)
library(purrr)
library(ggplot2)
library(dplyr)



# データ
# --- イテレーションごとの減衰値
X <-
  tibble(iter = 1:40,
         value =  map_dbl(iter,
                  expo_decay,
                  start_val = .1,
                  limit_val = 0,
                  slope = 1 / 5)
         )


# 確認
X %>% print()


# プロット
X %>%
  ggplot(aes(x = iter, y =value)) +
  geom_path()