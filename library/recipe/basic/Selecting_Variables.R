# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/02


library(recipes)
library(modeldata)


# データ準備
data("credit_data")

# 一部抜粋
my_credit_data <-
  credit_data %>%
    select(Status, Seniority, Time, Age, Records)

# データ概要
my_credit_data %>% glimpse()

# レシピの作成
rec <- recipe(Status ~ Seniority + Time + Age + Records, data = my_credit_data)
rec %>% summary(original = TRUE)



# データ型で選択：all_nominal() ---------------------------------------

# 全てのカテゴリカル変数を選択
rec %>%
  step_dummy(all_nominal()) %>%
  prep() %>%
  juice()


# カテゴリカル変数を選択（一部除外）
# --- 列名で除外
rec %>%
  step_dummy(all_nominal(), -Status) %>%
  prep() %>%
  juice()


# カテゴリカル変数を選択（目的変数を除外）
# --- 目的変数(outcome)を除外
rec %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep() %>%
  juice()



# データ型で選択：all_numeric() ---------------------------------------

# 連続変数のみを選択
rec %>%
  step_normalize(all_numeric()) %>%
  prep() %>%
  juice()



# roleで選択：all_predictor() ---------------------------------------

# 説明変数を選択（一部除外）
# --- カテゴリカルデータを除外
rec %>%
  step_normalize(all_predictors(), -Records) %>%
  prep() %>%
  juice()



# roleで選択：all_outcome() ---------------------------------------

# カテゴリカル変数から一部除外して指定
# --- 目的変数(outcome)を除外
rec %>%
  step_dummy(all_outcomes()) %>%
  prep() %>%
  juice()



# 列名で選択：dplyr関数 ---------------------------------------

# 単純な列名選択
rec %>%
  step_normalize(Time) %>%
  prep() %>%
  juice()


# dplyrの列選択の関数を使用
rec %>%
  step_normalize(starts_with("T")) %>%
  prep() %>%
  juice()

