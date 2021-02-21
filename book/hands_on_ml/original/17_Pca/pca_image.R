# ********************************************************************************
# Title   : 主成分分析
# Theme   : 主成分の探索
# Chapter : 17
# URL     : https://bradleyboehmke.github.io/HOML/pca.html
# Support : https://koalaverse.github.io/homlr/notebooks/17-pca.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(magrittr)
library(pca3d)



# 2次元のPCA  ----------------------------------------------

# 準備
AmesHousing::make_ames() %>% dim()



# 2次元のPCA  ----------------------------------------------

# データ準備
df <- 
  AmesHousing::make_ames() %>%
    select(var1 = First_Flr_SF, var2 = Gr_Liv_Area) %>%
    filter(var1 != var2) %>%
    mutate_all(log) %>%
    scale() %>% 
    data.frame() %>%
    filter(var1 < 4)

df %>% correlate()

# プロット
df %>% 
    ggplot(aes(x = var1, y = var2)) +
    geom_jitter(alpha = .2, size = 1, color = "dodgerblue") +
    geom_segment(
      aes(x = 0, xend = 1.5 , y = 0, yend = 1.5),
      arrow = arrow(length = unit(0.25,"cm")), size = 0.75, color = "black"
    ) +
    annotate("text", x = 1, y = .2, label = "First principal component", size = 2.5, hjust = 0) +
    annotate("text", x = -3, y = .8, label = "Second principal component", size = 2.5, hjust = 0) +
    geom_segment(
      aes(x = 0, xend = -0.27 , y = 0, yend = .65),
      arrow = arrow(length = unit(0.25,"cm")), size = 0.75, color = "black"
    ) +
    xlab("Feature 2") +
    ylab("Feature 1") +
    theme_bw()



# 3次元のPCA  ----------------------------------------------

# データ作成
# --- 対数化
df <- 
  AmesHousing::make_ames() %>%
    select(var1 = First_Flr_SF, 
           var2 = Gr_Liv_Area, 
           var3 = TotRms_AbvGrd) %>%
    filter(var1 != var2) %>%
    mutate_at(vars(var1, var2), log)


# 主成分分析
pca <- df %>% prcomp(scale = FALSE)

# プロット
# --- 3次元
pca %>% pca3d()


