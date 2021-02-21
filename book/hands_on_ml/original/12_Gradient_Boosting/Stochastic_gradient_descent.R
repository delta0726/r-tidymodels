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


# create random walk data
set.seed(123)
x <- sample(seq(3, 5, by = .05), 10, replace = TRUE)
set.seed(123)
y <- seq(2, 28, length.out = 10)

random_walk <- data.frame(
  x = x,
  y = y[order(y, decreasing = TRUE)]
)

optimal <- data.frame(x = 0, y = 0)

# plot
ggplot(df, aes(x, y)) + 
  coord_polar() +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  geom_point(data = random_walk, aes(x, y), size = 3, shape = 21, fill = "blue", alpha = .5) + 
  geom_point(data = optimal, aes(x, y), size = 2, shape = 21, fill = "yellow") + 
  geom_path(data = random_walk, aes(x, y), lty = "dotted") +
  annotate("text", x = random_walk[1, "x"], y = random_walk[1, "y"], label = "Start", hjust = 1, vjust = -1) +
  annotate("text", x = optimal[1, "x"], y = optimal[1, "y"], label = "Minimum", hjust = -.2, vjust = 1) +
  ylim(c(0, 28)) + 
  xlim(-5, 5)