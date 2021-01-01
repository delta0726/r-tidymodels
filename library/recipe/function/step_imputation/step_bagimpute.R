# Title     : step_bagimpute
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/27
# URL       : https://recipes.tidymodels.org/reference/step_bagimpute.html


# ＜ポイント＞
# - impute_with()で説明変数全てを選択してバギングツリーを作成する
#   --- バギングツリーは、欠損値を持つ予測子自体を受け入れることができる
#   --- 数値データとカテゴリカルデータの両方で使うことができる


# ＜構文＞
# step_bagimpute(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   impute_with = imp_vars(all_predictors()),
#   trees = 25,
#   models = NULL,
#   options = list(keepX = FALSE),
#   seed_val = sample.int(10^4, 1),
#   skip = FALSE,
#   id = rand_id("bagimpute")
# )





# 1 準備 ----------------------------------------------------

library(tidyverse)
library(tidymodels)
library(modeldata)
library(skimr)


# データロード
data("credit_data")


# 欠損値確認
# --- 数値とファクターの両方に欠損がある
credit_data %>% vapply(function(x) mean(is.na(x)), c(num = 0))
credit_data %>% skim()


# データ分割
set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)
credit_tr <- credit_data[ in_training, ]
credit_te <- credit_data[-in_training, ]



# 2 データ全体でバギング ----------------------------------------------------

# レシピ作成
impute_rec <-
  recipe(Price ~ ., data = credit_tr) %>%
    step_bagimpute(Status, Home, Marital, Job, Income, Assets, Debt)


# レシピ確認
impute_rec %>% print()


# レシピ適用
imp_models <- impute_rec %>% prep(training = credit_tr)
imputed_te <- imp_models %>% bake(new_data = credit_te, everything())


# 欠損値確認
imputed_te %>% skim()


# レシピ詳細の確認
impute_rec %>% tidy()
impute_rec %>% tidy(number = 1)
imp_models %>% tidy()
imp_models %>% tidy(number = 1)



# 3 データを指定してバギング ----------------------------------------------------

impute_rec <-
  recipe(Price ~ ., data = credit_tr) %>%
    step_bagimpute(Status, Home, Marital, Job, Income, Assets, Debt,
                   impute_with = imp_vars(Time, Age, Expenses),
                   options = list(nbagg = 5, keepX = FALSE))


# レシピ適用
imp_models <- impute_rec %>% prep(training = credit_tr)
imputed_te <- imp_models %>% bake(new_data = credit_te, everything())


# データ確認
credit_te[missing_examples,]
imputed_te[missing_examples, names(credit_te)]


# 欠損値確認
imputed_te %>% skim()


# レシピ詳細の確認
impute_rec %>% tidy()
impute_rec %>% tidy(number = 1)
imp_models %>% tidy()
imp_models %>% tidy(number = 1)

