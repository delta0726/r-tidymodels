# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/02


# ＜skip引数＞
# - 多くのstep_関数群には｢skip引数｣が付与されている
# -
#


library(recipes)


# レシピの作成
# --- dispは対数変換しない（skip=TRUE）
car_recipe <-
  recipe(mpg ~ ., data = mtcars) %>%
    step_log(disp, skip = TRUE) %>%
    prep(training = mtcars)


# 元データ
mtcars %>% as_tibble() %>% select(disp, hp)


# juice()とbake()で振舞いが異なる --------------------------------------

# ＜ポイント＞
# - 同じ結果(dispは対数変換されない)が期待されるが、以下の２つは結果が異なる
# -

# juice()
# --- dispは対数変換される
car_recipe %>%
  juice() %>%
  head() %>%
  select(disp, hp)


# bake()
# --- dispは対数変換されない
car_recipe %>%
  bake(new_data = mtcars) %>%
  head() %>%
  select(disp, hp)
