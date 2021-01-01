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




# 1 準備 -----------------------------------------------------

# ライブラリ準備
library(tidyverse)
library(recipes)
library(modeldata)
library(themis)
library(gridExtra)


# データ準備
data(okc)


# データ確認
okc %>% as_tibble()
okc %>% glimpse()




# 2-1 簡単な例 -----------------------------------------------------

# ＜ポイント＞
# - SMOTEアルゴリズムによるオーバーサンプリングを行う
# - 少ない方のデータをただ単にコピーするのではなく、KNN を用いてデータを増やす


# ラベルデータの数
# --- 不均衡になっている
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
# --- 前処理後は均衡になっている
# ---
ds_rec %>% juice() %>% pull(Class) %>% table(useNA = "always") %>% sort()



# 2-2 処理の検証 -----------------------------------------------

# データ取得
df_raw <- okc %>% select(Class, age, height)
df_rec <- ds_rec %>% juice() %>% select(Class, age, height)


# 確認
df_raw %>% print()
df_rec %>% print()


# 統計量
# --- データのレンジは一致
df_raw %>% summary()
df_rec %>% summary()


# 散布図
p1 <- df_raw %>% ggplot(aes(x = age, y = height, fill = Class, color = Class)) + geom_point(alpha=0.5)
p2 <- df_rec %>% ggplot(aes(x = age, y = height, fill = Class, color = Class)) + geom_point(alpha=0.5)
grid.arrange(p1, p2)




# 3-1 データ準備 -----------------------------------------------------

# データ準備
example_data <- data.frame(class = letters[rep(1:5, 1:5 * 10)],
                           x = rnorm(150))


# データ確認
example_data %>% as_tibble()
example_data %>% pull(class) %>% table()


# プロット
example_data %>%
  ggplot(aes(class)) +
  geom_bar()


# 3.2 step_upsample() ------------------------------------------

# ＜ポイント＞
# - オーバーサンプリングの各関数にはover_ratio引数が設定されている
# - over_ratioは0-1の範囲で決定される
#   --- 0の場合は処理なし、1の場合は完全に均衡となる


# 3.2.1 over_ratio = 1 ----------------------------------------

# 前処理
# --- オーバーサンプリング
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


# プロット
# --- データレンジ
df1 <- example_data %>% mutate(flag = "raw")
df2 <- df_up_1 %>% mutate(flag = "reciped")
df1 %>%
  bind_rows(df2) %>%
  ggplot(aes(x = class, y = x, group = class)) +
  geom_boxplot() +
  facet_wrap(~flag)



# 3.2.2 over_ratio = 0.5 ----------------------------------------

# 前処理
# --- オーバーサンプリング
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


# プロット
# --- データレンジ
df1 <- example_data %>% mutate(flag = "raw")
df2 <- df_up_0.5 %>% mutate(flag = "reciped")
df1 %>%
  bind_rows(df2) %>%
  ggplot(aes(x = class, y = x, group = class)) +
  geom_boxplot() +
  facet_wrap(~flag)



# 3.3 step_downsample() ------------------------------------------

# ＜ポイント＞
# - アンダーサンプリングの各関数には_ratio引数が設定されている


# 3.2.1 over_ratio = 1 ----------------------------------------

# 前処理
# --- オーバーサンプリング
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


# プロット
# --- データレンジ
df1 <- example_data %>% mutate(flag = "raw")
df2 <- df_down_1 %>% mutate(flag = "reciped")
df1 %>%
  bind_rows(df2) %>%
  ggplot(aes(x = class, y = x, group = class)) +
  geom_boxplot() +
  facet_wrap(~flag)


# 3.2.2 over_ratio = 0.5 ----------------------------------------

# 前処理
# --- オーバーサンプリング
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