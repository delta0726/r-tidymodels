# ****************************************************************************
# Title     : Basic Usage
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/15
# URL       : https://themis.tidymodels.org/index.html
# ****************************************************************************

# ＜ポイント＞
# - {themis}には不均衡データを処理するための前処理を実装している
#    --- アンダーサンプリングとオーバーサンプリング


# ＜参考＞
# - https://qiita.com/eigs/items/8ae0970afe188a1124d1


# ＜目次＞
# 0 準備
# 1 オーバーサンプリングの実行
# 2 オーバーサンプリングの結果検証
# 3 オーバーサンプリングのサンプル比率変更
# 4 アンダーサンプリングのサンプル比率変更


# 0 準備 ---------------------------------------------------------------

# ライブラリ準備
library(tidyverse)
library(recipes)
library(modeldata)
library(themis)
library(gridExtra)
library(magrittr)


# データ準備
# --- stem教育を受けた人のデータ
data(okc)

# データ確認
okc %>% as_tibble()
okc %>% glimpse()


# データ準備
# ---- カテゴリごとのサンプルデータ
example_data <-
  data.frame(class = letters[rep(1:5, 1:5 * 10)],
             x = rnorm(150))

# データ確認
example_data %>% as_tibble()
example_data %>% pull(class) %>% table()

# プロット
# --- カテゴリaからeにかけて順に多くなる
example_data %>%
  ggplot(aes(class)) +
  geom_bar()


# 1 オーバーサンプリングの実行 -------------------------------------------

# ＜ポイント＞
# - SMOTEアルゴリズムによるオーバーサンプリングを行う
# - 少ない方のデータをただ単にコピーするのではなく、KNN を用いてデータを増やす


# ラベルデータの数
# --- stemとotherが不均衡になっている
okc$Class %>% table(useNA = "always") %>% sort()

# レシピ作成
# --- 欠損値を平均で補完
# --- SMOTEアルゴリズムによるオーバーサンプリング
ds_rec <-
  recipe(Class ~ age + height, data = okc) %>%
    step_meanimpute(all_predictors()) %>%
    step_smote(Class) %>%
    prep()

# ラベルデータの数
# --- stepが増加してotherと同じサンプル数になっている
ds_rec %>%
  juice() %>%
  pull(Class) %>%
  table(useNA = "always") %>%
  sort()


# 2 オーバーサンプリングの結果検証 ----------------------------------------

# データ取得
df_raw <- okc %>% select(Class, age, height)
df_rec <- ds_rec %>% juice() %>% select(Class, age, height)

# レコード数の確認
df_raw %>% dim()
df_rec %>% dim()

# 同一レコード数
# --- 実験：同じレコードを増やしているだけならグループ数はレシピ前後で同じになるはず
# --- SMOTEは同じレコードを単純に増えているわけではない
df_raw %>% group_by(Class, age, height) %>% tally() %>% use_series(n) %>% sum()
df_rec %>% group_by(Class, age, height) %>% tally() %>% use_series(n) %>% sum()

# 統計量
# --- データのレンジは一致
df_raw %>% summary()
df_rec %>% summary()

# ヒストグラム
# --- age
p1 <- df_raw %>% ggplot(aes(x = age)) + geom_histogram(alpha=0.5) + ggtitle("raw")
p2 <- df_rec %>% ggplot(aes(x = age)) + geom_histogram(alpha=0.5) + ggtitle("rec")
grid.arrange(p1, p2, nrow = 1)

# ヒストグラム
# --- hight
p1 <- df_raw %>% ggplot(aes(x = height)) + geom_histogram(alpha=0.5) + ggtitle("raw")
p2 <- df_rec %>% ggplot(aes(x = height)) + geom_histogram(alpha=0.5) + ggtitle("rec")
grid.arrange(p1, p2, nrow = 1)

# 散布図
p1 <- df_raw %>% ggplot(aes(x = age, y = height, fill = Class, color = Class)) + geom_point(alpha=0.5)
p2 <- df_rec %>% ggplot(aes(x = age, y = height, fill = Class, color = Class)) + geom_point(alpha=0.5)
grid.arrange(p1, p2)


# 3 オーバーサンプリングのサンプル比率変更 -------------------------------------------

# ＜ポイント＞
# - オーバーサンプリングの各関数にはover_ratio引数が設定されている
# - over_ratioは0-1の範囲で決定される
#   --- 0の場合は処理なし、1の場合は完全に均衡となる


# カテゴリごとのカウント
# --- 元データ
example_data %>% pull(class) %>% table()


# over_ratio = 1 ------------------------------------------------------

# 前処理
# --- 全て最大数と同じになるようにアップサンプリング
df_up_1 <-
  recipe(~., example_data) %>%
    step_upsample(class, over_ratio = 1) %>%
    prep() %>%
    juice()

# データ確認
df_up_1 %>% pull(class) %>% table()

# プロット
# --- サンプル数
df_up_1 %>%
  ggplot(aes(class)) +
  geom_bar()

# プロット作成
# --- 元データの分布を元にアップサンプリング
# --- レンジに変化なし
df1 <- example_data %>% mutate(flag = "raw")
df2 <- df_up_1 %>% mutate(flag = "reciped")
df1 %>%
  bind_rows(df2) %>%
  ggplot(aes(x = class, y = x, group = class)) +
  geom_boxplot() +
  facet_wrap(~flag)



# 3 over_ratio = 0.5 -----------------------------------------------------

# 前処理
# --- 最大数の半分まで増やすようにアップサンプリング
df_up_0.5 <-
recipe(~., example_data) %>%
  step_upsample(class, over_ratio = 0.5) %>%
  prep() %>%
  juice()

# データ確認
df_up_0.5 %>% pull(class) %>% table()

# プロット
# --- サンプル数
df_up_0.5 %>%
  ggplot(aes(class)) +
  geom_bar()

# プロット作成
# --- 元データの分布を元にアップサンプリング
# --- レンジに変化なし
df1 <- example_data %>% mutate(flag = "raw")
df2 <- df_up_0.5 %>% mutate(flag = "reciped")
df1 %>%
  bind_rows(df2) %>%
  ggplot(aes(x = class, y = x, group = class)) +
  geom_boxplot() +
  facet_wrap(~flag)


# 4 アンダーサンプリングのサンプル比率変更 ----------------------------------------

# ＜ポイント＞
# - アンダーサンプリングの各関数には_ratio引数が設定されている


# 3.2.1 over_ratio = 1 ------------------------------------------------

# 前処理
# --- 最小値に一致するようにダウンサンプリング
df_down_1 <-
recipe(~., example_data) %>%
  step_downsample(class, under_ratio = 1) %>%
  prep() %>%
  juice()

# データ確認
df_down_1 %>% pull(class) %>% table()

# プロット
# --- サンプル数
df_down_1 %>%
  ggplot(aes(class)) +
  geom_bar()

# プロット作成
# --- 元データの分布を元にダウンサンプリング
# --- レンジに変化なし
df1 <- example_data %>% mutate(flag = "raw")
df2 <- df_down_1 %>% mutate(flag = "reciped")
df1 %>%
  bind_rows(df2) %>%
  ggplot(aes(x = class, y = x, group = class)) +
  geom_boxplot() +
  facet_wrap(~flag)


# over_ratio = 0.5 ------------------------------------------------

# 前処理
# --- 最小値の2倍まで減らすようにダウンサンプリング
df_down_2 <-
recipe(~., example_data) %>%
  step_downsample(class, under_ratio = 2) %>%
  prep() %>%
  juice()

# データ確認
df_down_2 %>% pull(class) %>% table()

# プロット
# --- サンプル数
df_down_2 %>%
  ggplot(aes(class)) +
  geom_bar()

# プロット
# --- データレンジ
df1 <- example_data %>% mutate(flag = "raw")
df2 <- df_down_2 %>% mutate(flag = "reciped")
df1 %>%
  bind_rows(df2) %>%
  ggplot(aes(x = class, y = x, group = class)) +
  geom_boxplot() +
  facet_wrap(~flag)