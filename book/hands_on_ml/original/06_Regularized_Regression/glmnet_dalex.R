# ********************************************************************************
# Title   : glmnetをDalexで解釈する
# Chapter : 6
# URL     :
# ********************************************************************************

library(magrittr)
library(tidyverse)
library(tidyquant)
library(glmnet)
library(DALEX)
library(gridExtra)


# データ準備
D <- 
  titanic %>%
    select('survived', 'gender', 'age', 'class', 'embarked', 'fare', 'sibsp', 'parch') %>%
    filter(complete.cases(.))


# モデルデータ
Y = D$survived == 'yes'
X = D %>% select(-survived) %>% data.matrix


# 正則化回帰
model_ridge <- glmnet(x = X, y = Y, family = "binomial",　alpha = 1)
model_lasso <- glmnet(x = X, y = Y, family = "binomial",　alpha = 0)


# 解釈用モデル
explain_ridge <- model_ridge %>% explain(data = X, y = Y)
explain_lasso <- model_lasso %>% explain(data = X, y = Y)


# 変数重要度
p1 <- explain_ridge %>% variable_importance() %>% plot()
p2 <- explain_lasso %>% variable_importance() %>% plot()
grid.arrange(p1, p2)

# PDP
p1 <- explain_ridge %>% model_profile() %>% plot() + theme_tq()
p2 <- explain_lasso %>% model_profile() %>% plot() + theme_tq()
grid.arrange(p1, p2)

