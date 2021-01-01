# ****************************************************************************
# Title     : step_tomek
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/15
# URL       : https://themis.tidymodels.org/reference/step_tomek.html
# ****************************************************************************


# ＜概要＞
# - データセットの行を複製することで特定のカテゴリ変数の出現頻度をコントロールする
#   --- オーバーサンプリング
#   --- over_ratioは{tune}でチューニングが可能



# ＜概要＞
# step_tomek(
#   recipe,
#   ...,
#   role = NA,
#   trained = FALSE,
#   column = NULL,
#   skip = TRUE,
#   seed = sample.int(10^5, 1),
#   id = rand_id("tomek")
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
data(okc)


# データ確認
okc %>% print()
okc %>% glimpse()


# カテゴリの確認
# --- オリジナル
orig <- okc$diet %>% table(useNA = "always")
orig %>% sort(decreasing = TRUE)




# 2 ダウンサンプリングの確認 ---------------------------------------

# ダウンサンプリング実行
# --- step_tomek()を使う際は欠損値の補完が必要
okc_down <-
  recipe(Class ~ age + height, data = okc) %>%
    step_meanimpute(all_predictors()) %>%
    step_tomek(Class) %>%
    prep(training = okc, retain = TRUE) %>%
    juice()


# カテゴリ数の確認
okc %>% pull(Class) %>% table() %>% sort()
okc_down %>% pull(Class) %>% table() %>% sort()


# オリジナルデータ
okc %>%
  group_by(Class, age, height) %>%
  tally()


# ダウンサンプリング・データ
# --- 同じレコードが複製されている
okc_down %>%
  group_by(Class, age, height) %>%
  tally()



# 3 レシピの注意点 -----------------------------------------------

# ＜ポイント＞
# - over_ratio引数でオーバーサンプリングの度合いをコントロールする
# - skip引数はデフォルトではTRUEになっている
#   --- bake()の時に適用されないので注意


# レシピ作成
# --- オーバーサンプリング
up_rec <-
  recipe(Class ~ age + height, data = okc) %>%
    step_meanimpute(all_predictors()) %>%
    step_tomek(Class) %>%
    prep(training = okc, retain = TRUE)


# カテゴリの確認
# --- juicedデータ
# --- bakedデータ
orig   <- okc$Class %>% table(useNA = "always") %>% sort()
juiced <- up_rec %>% juice() %>% pull(Class) %>% table(useNA = "always") %>% sort()
baked  <- up_rec %>% bake(new_data = okc) %>% pull(Class) %>% table(useNA = "always") %>% sort()


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


# ダウンサンプリング
circle_example_reciped <-
  recipe(class ~ ., data = circle_example) %>%
    step_tomek(class) %>%
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
    labs(title = "Without Tomek") +
    xlim(c(1, 15)) +
    ylim(c(1, 15))


# ダウンサンプリングデータ
p2 <-
  circle_example_reciped %>%
    ggplot(aes(x, y, color = class)) +
    geom_point(alpha=0.7) +
    labs(title = "With Tomek") +
    xlim(c(1, 15)) +
    ylim(c(1, 15))


# 比較
grid.arrange(p1, p2, nrow = 1)
