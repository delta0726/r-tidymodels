# ****************************************************************************
# Title     : step_upsample
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/15
# URL       : https://themis.tidymodels.org/reference/step_upsample.html
# ****************************************************************************


# ＜概要＞
# - データセットの行を複製することで特定のカテゴリ変数の出現頻度をコントロールする
#   --- オーバーサンプリング
#   --- over_ratioは{tune}でチューニングが可能



# ＜概要＞
# step_upsample(
#   recipe,
#   ...,
#   over_ratio = 1,
#   ratio = NA,
#   role = NA,
#   trained = FALSE,
#   column = NULL,
#   target = NA,
#   skip = TRUE,
#   seed = sample.int(10^5, 1),
#   id = rand_id("upsample")
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




# 2 アップサンプリングの確認 ---------------------------------------

# アップサンプリング実行
okc_up <-
  recipe( ~ ., data = okc) %>%
    step_upsample(diet, over_ratio = 1) %>%
    prep(training = okc, retain = TRUE) %>%
    juice()


# カテゴリ数の確認
okc %>% pull(diet) %>% table()
okc_up %>% pull(diet) %>% table()


# 重複レコード
# --- オリジナルは重複なし
okc %>%
  group_by(age, diet, height, location, date, Class) %>%
  tally()


# 重複レコード
# --- 同じレコードが複製されている
okc_up %>%
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
    step_upsample(diet, over_ratio = 0.0121) %>%
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


# アップサンプリング
circle_example_reciped <-
  recipe(class ~ ., data = circle_example) %>%
    step_upsample(class) %>%
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


# アップサンプリングデータ
p2 <-
  circle_example_reciped %>%
    ggplot(aes(x, y, color = class)) +
    geom_point(alpha=0.7) +
    labs(title = "With upsample")


# 比較
grid.arrange(p1, p2, nrow = 1)
