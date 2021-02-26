# *****************************************************************************
# Title     : set_engine
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/set_engine.html
# *****************************************************************************


# ＜ポイント＞
# - モデルを実行するエンジンを指定する
# - モデル独自の引数を指定する



# ＜構文＞
# set_engine(object, engine, ...)
#
# object : モデルオブジェクト
# engine : モデルエンジンを文字列で指定
# ...    : 選択した計算エンジンに紐づくオプション引数


library(tidyverse)
library(tidymodels)



#%% 基本操作 ---------------------------------------------


# モデルのみ指定
# --- エンジンは設定されていない
mod1 <- logistic_reg(mixture = 1/3)
mod1 %>% print()
mod1$engine


# モデルにエンジンを追加
# --- {glmnet}独自の引数も指定
mod <-
  logistic_reg(mixture = 1/3) %>%
  set_engine("glmnet", nlambda = 10)


# 確認
mod %>% print()
mod %>% glimpse()
mod$engine
mod$eng_args


# 固有オブジェクトに変換
mod %>%
  translate(engine = "glmnet")




#%% 変数による引数設定 ---------------------------------------------

# モデル独自のパラメータのリスト化
params <- list(nlmbda = 10)


# モデル作成
model <-
  linear_reg(mixture = 1/3) %>%
    list(object = ., engine = "glmnet") %>%
    c(params) %>%
    do.call(set_engine, .)


# 確認
model %>% translate()


# モデル実行
model %>%
  fit(Petal.Length~., data = iris[,1:4])

