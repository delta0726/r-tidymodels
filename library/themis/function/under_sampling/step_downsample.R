# ****************************************************************************
# Title     : step_downsample
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/15
# URL       : https://themis.tidymodels.org/reference/step_downsample.html
# ****************************************************************************


# ＜概要＞
# - データセットの行を複製することで特定のカテゴリ変数の出現頻度をコントロールする
#   --- オーバーサンプリング
#   --- over_ratioは{tune}でチューニングが可能



# ＜概要＞
# step_downsample(
#   recipe,
#   ...,
#   under_ratio = 1,
#   ratio = NA,
#   role = NA,
#   trained = FALSE,
#   column = NULL,
#   target = NA,
#   skip = TRUE,
#   seed = sample.int(10^5, 1),
#   id = rand_id("downsample")
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
okc_down <-
  recipe( ~ ., data = okc) %>%
    step_downsample(diet) %>%
    prep(training = okc, retain = TRUE) %>%
    juice()


# カテゴリ数の確認
okc %>% pull(diet) %>% table()
okc_down %>% pull(diet) %>% table()


# オリジナルデータ
okc %>%
  group_by(age, diet, height, location, date, Class) %>%
  tally()


# ダウンサンプリング・データ
# --- 同じレコードが複製されている
okc_down %>%
  group_by(age, diet, height, location, date, Class) %>%
  tally()



# 3 レシピの注意点 -----------------------------------------------

# ＜ポイント＞
# - over_ratio引数でオーバーサンプリングの度合いをコントロールする
# - skip引数はデフォルトではTRUEになっている
#   --- bake()の時に適用されないので注意


# レシピ作成
# --- オーバーサンプリング
up_rec <-
  recipe( ~ ., data = okc) %>%
    step_downsample(diet) %>%
    prep(training = okc, retain = TRUE)


# カテゴリの確認
# --- juicedデータ
# --- bakedデータ
orig   <- okc$diet %>% table(useNA = "always")
juiced <- up_rec %>% juice() %>% pull(diet) %>% table(useNA = "always")
baked  <- up_rec %>% bake(new_data = okc) %>% pull(diet) %>% table(useNA = "always")


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
    step_downsample(class) %>%
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
    labs(title = "Without upsample")


# ダウンサンプリングデータ
p2 <-
  circle_example_reciped %>%
    ggplot(aes(x, y, color = class)) +
    geom_point(alpha=0.7) +
    labs(title = "With upsample")


# 比較
grid.arrange(p1, p2, nrow = 1)
