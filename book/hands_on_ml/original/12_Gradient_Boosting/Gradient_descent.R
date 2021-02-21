# ********************************************************************************
# Title   : Gradient Boosting
# Chapter : 12
# URL     : https://bradleyboehmke.github.io/HOML/gbm.html
# Support : https://koalaverse.github.io/homlr/notebooks/10-bagging.nb.html
# ********************************************************************************



library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(rpart)
library(gbm)
library(xgboost)
library(vip)
library(h2o)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# ＜ポイント＞



# 0.準備 ----------------------------------------------------------

# データ作成
x <- seq(-5, 5, by = .05)
y <- x^2 + 3
df <- data.frame(x, y)

# 確認
df %>% as_tibble()



# 0.準備 ----------------------------------------------------------

# ステップ設定
step <- 5
step_size <- 0.2

for(i in seq_len(18)) {
  next_step <- max(step) + round(diff(range(max(step), which.min(df$y))) * step_size, 0)
  step <- c(step, next_step)
  next
}

steps <- df[step, ] %>%
  mutate(x2 = lag(x), y2 = lag(y)) %>%
  dplyr::slice(1:18)



# 2.プロット ----------------------------------------------------------

# plot
df %>% 
  ggplot(aes(x, y)) +
    geom_line(size = 1.5, alpha = .5) +
    theme_classic() +
    scale_y_continuous("Loss function", limits = c(0, 30)) +
    xlab(expression(theta)) +
    geom_segment(data = df[c(5, which.min(df$y)), ], aes(x = x, y = y, xend = x, yend = -Inf), lty = "dashed") +
    geom_point(data = filter(df, y == min(y)), aes(x, y), size = 4, shape = 21, fill = "yellow") +
    geom_point(data = steps, aes(x, y), size = 3, shape = 21, fill = "blue", alpha = .5) +
    geom_curve(data = steps, aes(x = x, y = y, xend = x2, yend = y2), curvature = 1, lty = "dotted") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = df[5, "x"], y = 1, label = "Initial value", hjust = -0.1, vjust = .8) +
    annotate("text", x = df[which.min(df$y), "x"], y = 1, label = "Minimium", hjust = -0.1, vjust = .8) +
    annotate("text", x = df[5, "x"], y = df[5, "y"], label = "Learning step", hjust = -.8, vjust = 0)




# 3.学習率 ----------------------------------------------------------

# create too small of a learning rate
step <- 5
step_size <- .05
for(i in seq_len(10)) {
  next_step <- max(step) + round(diff(range(max(step), which.min(df$y))) * step_size, 0)
  step <- c(step, next_step)
  next
}

too_small <- df[step, ] %>%
  mutate(x2 = lag(x), y2 = lag(y))



# create too large of a learning rate
too_large <- df[round(which.min(df$y) * (1 + c(-.9, -.6, -.2, .3)), 0), ] %>%
  mutate(x2 = lag(x), y2 = lag(y))
# plot


# 学習率：高
p1 <- 
  df %>% 
    ggplot(aes(x, y)) +
    geom_line(size = 1.5, alpha = .5) +
    theme_classic() +
    scale_y_continuous("Loss function", limits = c(0, 30)) +
    xlab(expression(theta)) +
    geom_segment(data = too_small[1, ], aes(x = x, y = y, xend = x, yend = -Inf), lty = "dashed") +
    geom_point(data = too_small, aes(x, y), size = 3, shape = 21, fill = "blue", alpha = .5) +
    geom_curve(data = too_small, aes(x = x, y = y, xend = x2, yend = y2), curvature = 1, lty = "dotted") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = df[5, "x"], y = 1, label = "Start", hjust = -0.1, vjust = .8) +
    ggtitle("b) too small")


# 学習率：低
p2 <- 
  df %>% 
    ggplot(aes(x, y)) +
    geom_line(size = 1.5, alpha = .5) +
    theme_classic() +
    scale_y_continuous("Loss function", limits = c(0, 30)) +
    xlab(expression(theta)) +
    geom_segment(data = too_large[1, ], aes(x = x, y = y, xend = x, yend = -Inf), lty = "dashed") +
    geom_point(data = too_large, aes(x, y), size = 3, shape = 21, fill = "blue", alpha = .5) +
    geom_curve(data = too_large, aes(x = x, y = y, xend = x2, yend = y2), curvature = 1, lty = "dotted") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = too_large[1, "x"], y = 1, label = "Start", hjust = -0.1, vjust = .8) +
    ggtitle("a) too big")

# プロット比較
gridExtra::grid.arrange(p2, p1, nrow = 1)



