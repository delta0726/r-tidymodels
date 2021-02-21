# ********************************************************************************
# Title   : ロジスティック回帰
# Chapter : 5
# URL     : https://bradleyboehmke.github.io/HOML/logistic-regression.html
# Support : https://koalaverse.github.io/homlr/notebooks/06-regularized-regression.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(glmnet)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)


# 線形回帰
# --- geom_smooth()でLMを使って直線をひく
# --- 不適切
p1 <- 
  ISLR::Default %>%
    mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
    ggplot(aes(balance, prob)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "lm") +
    ggtitle("Linear regression model fit") +
    xlab("Balance") +
    ylab("Probability of Default")

# GLM
# --- geom_smooth()でGMLを使ってロジスティック曲線をひく
# --- Xのすべての値をロジスティック関数で0-1の確率に変換する
# --- S字カーブの関数を描くことができる
p2 <- 
  ISLR::Default %>%
    mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
    ggplot(aes(balance, prob)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggtitle("Logistic regression model fit") +
    xlab("Balance") +
    ylab("Probability of Default")

# 比較
gridExtra::grid.arrange(p1, p2, nrow = 1)

