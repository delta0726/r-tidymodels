# ********************************************************************************
# Title   : k平均法クラスタリング
# Theme   : 距離測度
# Chapter : 20
# URL     : https://bradleyboehmke.github.io/HOML/kmeans.html
# Support : https://koalaverse.github.io/homlr/notebooks/20-kmeans.nb.html
# H2O　   : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/k-means.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(magrittr)
library(stringr)
library(Hmisc)
library(kableExtra)
library(cluster)
library(factoextra)



# データ作成
# --- 2次元のデータ
df <- tibble(
  x1 = c(rnorm(100), rnorm(100) + 3),
  x2 = c(rnorm(100), rnorm(100) - 2)
)


# 確認
df %>% print()


# プロット作成
# --- 6パターンのk平均法作成
map(1:6, ~ kmeans(df, 3)) %>%
  map2_dfr(1:6, ~ df %>% mutate(
    cluster = .x$cluster,
    name = paste0("Iteration: ", .y, ";  W(Ck): ", round(.x$tot.withinss, 2))
  )) %>%
  ggplot(aes(x1, x2, colour = cluster)) +
  geom_point(show.legend = FALSE, size = 1) +
  facet_wrap(~ name, nrow = 2)


