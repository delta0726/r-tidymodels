# Title     : vfold_cv
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/vfold_cv.html


# ＜ポイント＞
# - 学習データを分割してhold-out法の手続きを複数回繰り返す


# ＜構文＞
# vfold_cv(data, v = 10, repeats = 1, strata = NULL, breaks = 4, ...)





library(tidyverse)
library(tidymodels)


# Check Data
# --- 32レコード
mtcars %>% as_tibble()



#%% 基本的な使い方 ---------------------------------

# V-Fold Cross Validation
X1 <- mtcars %>% vfold_cv(v = 10)
X1 %>% print()


# データの中身
X1$splits
X1$splits[[1]]
X1$splits[[1]] %>% training()
X1$splits[[1]] %>% testing()


# 分割データの取得
# --- training / testing
X1 %>%
  mutate(training = map(splits, training),
         testing  = map(splits, testing))


# 分割データの取得
# --- analysis / Assessment
# --- どちらでも取得可能
X1 %>%
  mutate(analysis   = map(splits, analysis),
         assessment = map(splits, assessment))



#%% repeats引数 ---------------------------------

# クロスバリデーションを行うセット数
X2 <- mtcars %>% vfold_cv(v = 10, repeats = 2)
X2 %>% print()



#%% strata引数 ---------------------------------

# ＜ポイント＞
# - strata引数を指定することで層化抽出サンプリングを行うことができる


# データ
# --- virginicaの一部を削除
iris2 <- iris[1:130, ]
iris2 %>% group_by(Species) %>% tally()


# 関数定義
# --- virginicaが含まれる割合
pct_virginica <- function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        }


# バリデーションデータの作成
# --- strataの指定なし
# --- virginicaが含まれる割合が異なる
set.seed(13)
folds1 <- iris2 %>% vfold_cv(v = 5)
folds1$splits %>% map_dbl(pct_virginica)


# バリデーションデータの作成
# --- strataの指定あり
# --- virginicaが含まれる割合が等しい
set.seed(13)
folds2 <- iris2 %>% vfold_cv(v = 5, strata = "Species")
folds2$splits %>% map_dbl(pct_virginica)



#%% break引数 ---------------------------------

# ＜ポイント＞
# - 使い方がわからないので調査中


set.seed(13)
folds3 <- iris2 %>% vfold_cv(v = 5, strata = "Petal.Length", breaks = 5)
folds3$splits %>% map_dbl(pct_virginica)
