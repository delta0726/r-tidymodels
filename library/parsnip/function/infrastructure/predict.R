# *****************************************************************************
# Title     : predict
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/predict.model_fit.html
# *****************************************************************************


# ＜ポイント＞
# - 予測値のベクトルだけでなく、信頼区間なども計算することができる


# ＜構文＞
# predict(object, new_data, type = NULL, opts = list(), ...)




library(tidyverse)
library(tidymodels)



# データ準備
car_tr <- mtcars %>% slice(11:32)
car_pr <- mtcars %>% slice(1:10)  %>% select(-mpg)


# 確認
car_tr %>% dim()
car_pr %>% dim()


# モデル構築
lm_model <-
  linear_reg() %>%
    set_engine("lm") %>%
    fit(mpg ~ ., data = car_tr)


# 予測
# --- 予測データのみ出力
# --- tibble形式で出力される
lm_model %>% predict(car_pr)



# 予測値の信頼区間
# --- 予測値は出力されない
lm_model %>%
  predict(car_pr,
          type = "conf_int",
          level = 0.90)


# 元データを出力
lm_model %>%
  predict(car_pr,
          type = "raw",
          opts = list(type = "terms"))
