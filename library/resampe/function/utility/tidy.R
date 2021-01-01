# Title     : tidy
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/tidy.rsplit.html



# ＜ポイント＞
# - 各FoldのレコードがAnalysis/Assessmentのどちらかを区別して表示
# - リサンプリングが適切に行えているかを視覚的に確認する際に便利



# ＜構文＞
# tidy(x, ...)




library(tidyverse)
library(tidyquant)
library(tidymodels)



# データ確認
mtcars %>% as_tibble()



#%% 単純なクロスバリデーション -------------------------------------

# クロスバリデーション
cv <- mtcars %>% vfold_cv(v = 5)
cv %>% print()


# レコード区分を取得
cv_tidy <- cv %>% tidy() %>% select(Fold, Row, Data) %>% arrange(Fold, Row)
cv_tidy %>% print()


# プロット
# --- Assessmentがすべて異なるレコードであることが確認できる
cv_tidy %>%
  ggplot(aes(x = Fold, y = Row, fill = Data)) +
  geom_tile() +
  scale_fill_brewer() +
  theme_tq()



#%% 繰り返しのあるクロスバリデーション -------------------------------------

# クロスバリデーション
rcv <- mtcars %>% vfold_cv(v = 5, repeats = 2)
rcv %>% print()


# レコード区分を取得
cv_tidy <- rcv %>% tidy() %>% select(Repeat, Fold, Row, Data) %>% arrange(Repeat, Fold, Row)
cv_tidy %>% print()


# プロット
# --- RepeatごとにAssessmentがすべて異なるレコード
cv_tidy %>%
  ggplot(aes(x = Fold, y = Row, fill = Data)) +
  geom_tile() +
  facet_wrap(~Repeat) +
  scale_fill_brewer() +
  theme_tq()



#%% モンテカルロ・クロスバリデーション -------------------------------------

# クロスバリデーション
mccv <- mtcars %>% mc_cv(times = 5)
mccv %>% print()


# レコード区分を取得
mccv_tidy <- mccv %>% tidy() %>% select(Resample, Row, Data) %>% arrange(Resample, Data)
mccv_tidy %>% print()


# プロット
# --- AssessmentはResampleごとに同じレコードが選ばれる可能性がある
mccv_tidy %>%
  ggplot(aes(x = Resample, y = Row, fill = Data)) +
  geom_tile() +
  scale_fill_brewer() +
  theme_tq()



#%% ブートストラップ・クロスバリデーション -------------------------------------

# クロスバリデーション
btcv <- mtcars %>% bootstraps(time = 5)
btcv %>% print()


# レコード区分を取得
btcv_tidy <- btcv %>% tidy() %>% select(Resample, Row, Data) %>% arrange(Resample, Data)
btcv_tidy %>% print()


# プロット
# --- AssessmentはResampleごとに同じレコードが選ばれる可能性がある
btcv_tidy %>%
  ggplot(aes(x = Resample, y = Row, fill = Data)) +
  geom_tile() +
  scale_fill_brewer() +
  theme_tq()



#%% 時系列・クロスバリデーション -------------------------------------

# データ作成
dat <- tibble(day = 1:30)
dat %>% print()

# クロスバリデーション
tscv <- dat %>% rolling_origin(initial = 7, assess = 7, skip = 6, cumulative = FALSE)
tscv %>% print()


# レコード区分を取得
tscv_tidy <- tscv %>% tidy() %>% select(Resample, Row, Data) %>% arrange(Resample, Data)
tscv_tidy %>% print()


# プロット
# --- AssessmentはResampleごとに同じレコードが選ばれる可能性がある
tscv_tidy %>%
  ggplot(aes(x = Resample, y = Row, fill = Data)) +
  geom_tile() +
  scale_fill_brewer() +
  theme_tq()
