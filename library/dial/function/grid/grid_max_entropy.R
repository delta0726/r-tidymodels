# Title     : grid_max_entropy
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/14
# URL       : https://dials.tidymodels.org/reference/grid_max_entropy.html


# ＜ポイント＞
# - コンピュータの実験計画におけるパラメータグリッドを構築するために使用される
# -.空間の任意の部分に、それほど遠くない観測された組み合わせが設定される。
# -


# ＜構文＞
# grid_max_entropy(
#   x,
#   ...,
#   size = 3,
#   original = TRUE,
#   variogram_range = 0.5,
#   iter = 1000
# )



library(tidyverse)
library(tidymodels)



# ハイパーパラメータの範囲を確認
# --- hidden_units: 1 - 10
# --- penalty     : 0 - 1
# --- epochs      : 1 - 1000
# --- activation  : NULL
# --- learn_rate  : 1 - 2.718282（log）
hidden_units() %>% range_get(original = TRUE)
penalty() %>% range_get(original = TRUE)
epochs() %>% range_get(original = TRUE)
activation() %>% range_get(original = TRUE)
learn_rate(c(0, 1), trans = scales::log_trans()) %>% range_get(original = TRUE)


# グリッドの算出
grid_max_entropy(hidden_units(),
              penalty(),
                 epochs(),
                 activation(),
                 learn_rate(c(0, 1), trans = scales::log_trans()),
                 size = 10,
                 original = FALSE,
                 iter = 1000)





