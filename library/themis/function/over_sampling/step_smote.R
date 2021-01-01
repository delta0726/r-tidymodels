# ****************************************************************************
# Title     : step_smote
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/15
# URL       : https://themis.tidymodels.org/reference/step_smote.html
# ****************************************************************************



# ＜概要＞
# - 最近傍法でデータを生成することで特定のカテゴリ変数の出現頻度をコントロールする
#   --- オーバーサンプリング
#   --- over_ratioは{tune}でチューニングが可能


# ＜SMOTEアルゴリズム＞
# - https://qiita.com/eigs/items/8ae0970afe188a1124d1



# ＜概要＞
# step_smote(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   column = NULL,
#   over_ratio = 1,
#   neighbors = 5,
#   skip = TRUE,
#   seed = sample.int(10^5, 1),
#   id = rand_id("smote")
# )


# over_ratio
# - サンプリングの度合いを指定する
# - 1は完全に均衡になるまでサンプリングを増やす


# skip
# - 処理をスキップするかどうかを指定する
# - デフォルトがTRUEになっている
#   --- juice()では適用されるが、bake()では適用されない
#   --- {recipe}の関数の多くはFALSEになっている



# 1 準備 -----------------------------------------------

library(tidyverse)
library(modeldata)
library(recipes)
library(themis)
library(compareDF)


# データロード
data(credit_data)


# データ確認
credit_data %>% as_tibble()
credit_data %>% glimpse()


# カテゴリの確認
# --- オリジナル
credit_data$Status %>% table(useNA = "always")




# 2 アップサンプリングの確認 ---------------------------------------

# アップサンプリング実行
credit_data_up <-
  recipe(Status ~ Age + Income + Assets, data = credit_data) %>%
    step_meanimpute(all_predictors()) %>%
    step_smote(Status) %>%
    prep() %>%
    juice()


# カテゴリ数の確認
credit_data %>% pull(Status) %>% table()
credit_data_up %>% pull(Status) %>% table()


# 重複レコード
# --- オリジナルは重複なし
credit_data %>%
  group_by(Status, Age, Income, Assets) %>%
  tally()


# 重複レコード
# --- 最近傍法を用いているので重複なし
credit_data_up %>%
  group_by(Status, Age, Income, Assets) %>%
  tally()



# 3 レシピの注意点 -----------------------------------------------

# ＜ポイント＞
# - over_ratio引数でオーバーサンプリングの度合いをコントロールする
# - skip引数はデフォルトではTRUEになっている
#   --- bake()の時に適用されないので注意


# レシピ作成
# --- オーバーサンプリング
ds_rec <-
  recipe(Status ~ Age + Income + Assets, data = credit_data) %>%
    step_meanimpute(all_predictors()) %>%
    step_smote(Status) %>%
    prep()


# カテゴリの確認
# --- juicedデータ
# --- bakedデータ
orig   <- credit_data$Status %>% table(useNA = "always")
juiced <- ds_rec %>% juice() %>% pull(Status) %>% table(useNA = "always")
baked  <- ds_rec %>% bake(new_data = credit_data) %>% pull(Status) %>% table(useNA = "always")


# 確認
# --- juicedデータ（skip=TRUEでも処理が適用される）
# --- bakedデータ（skip=TRUEなので処理が適用されない）
data.frame(
  level  = names(orig),
  orig   = as.vector(orig),
  juiced = as.vector(juiced),
  baked  = as.vector(baked)
)




# 4 プロット比較 -------------------------------------------------

# ＜ポイント＞
# - step_upsample()は同じレコードを複製することでサンプリングを増やしている
# - 同じ値なのでプロットでは変化が見えない
#   --- alphaを設定して色の濃さで判別


# 元データ
# --- themis::circle_example
circle_example %>% as_tibble()


# アップサンプリング
circle_example_reciped <-
  recipe(class ~ ., data = circle_example) %>%
    step_smote(class) %>%
    prep() %>%
    juice()


# サンプル数
circle_example %>% dim()
circle_example_reciped %>% dim()


# カテゴリ数
circle_example %>% pull(class) %>% table()
circle_example_reciped %>% pull(class) %>% table()


# 元データ
p1 <-
  circle_example %>%
    ggplot(aes(x, y, color = class)) +
    geom_point(alpha=0.7) +
    labs(title = "Without SMOTE")


# アップサンプリングデータ
p2 <-
  circle_example_reciped %>%
    ggplot(aes(x, y, color = class)) +
    geom_point(alpha=0.7) +
    labs(title = "With SMOTE")


# 比較
grid.arrange(p1, p2, nrow = 1)
