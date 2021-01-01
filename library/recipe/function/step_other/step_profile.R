# Title     : step_profile
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/26
# URL       : https://recipes.tidymodels.org/reference/step_profile.html




# ＜ポイント＞
# - dplyr::arrange()を使った並び替えを行う
# - 機械学習アルゴリズムで並び替えが必要なケースは多くない
#   --- 時系列分析の場合に使うことが想定される


# ＜構文＞
# step_profile(
#   recipe,
#   ...,
#   profile = NULL,
#   pct = 0.5,
#   index = 1,
#   grid = list(pctl = TRUE, len = 100),
#   columns = NULL,
#   role = NA,
#   trained = FALSE,
#   skip = FALSE,
#   id = rand_id("profile")
# )




# 1 準備 ---------------------------------------------------------

library(tidyverse)
library(recipes)
library(modeldata)


# データロード
data(okc)


# データ確認
okc %>% print()
okc %>% glimpse()



# 2 レシピ作成 ---------------------------------------------------------

# Setup a grid across date but keep the other values fixed
recipe(~ diet + height + date, data = okc) %>%
  step_profile(-date, profile = vars(date)) %>%
  prep(training = okc) %>%
  juice
#> # A tibble: 100 x 3
#>    diet     height date
#>    <fct>     <dbl> <date>
#>  1 anything     68 2011-06-27
#>  2 anything     68 2011-06-30
#>  3 anything     68 2011-07-04
#>  4 anything     68 2011-07-08
#>  5 anything     68 2011-07-11
#>  6 anything     68 2011-07-15
#>  7 anything     68 2011-07-19
#>  8 anything     68 2011-07-23
#>  9 anything     68 2011-07-26
#> 10 anything     68 2011-07-30
#> # … with 90 more rows


##########

# An *additive* model; not for use when there are interactions or
# other functional relationships between predictors

lin_mod <- lm(mpg ~ poly(disp, 2) + cyl + hp, data = mtcars)

# Show the difference in the two grid creation methods

disp_pctl <- recipe(~ disp + cyl + hp, data = mtcars) %>%
  step_profile(-disp, profile = vars(disp)) %>%
  prep(training = mtcars)

disp_grid <- recipe(~ disp + cyl + hp, data = mtcars) %>%
  step_profile(
    -disp,
    profile = vars(disp),
    grid = list(pctl = FALSE, len = 100)
  ) %>%
  prep(training = mtcars)

grid_data <- juice(disp_grid)
grid_data <- grid_data %>%
  mutate(pred = predict(lin_mod, grid_data),
         method = "grid")

pctl_data <- juice(disp_pctl)
pctl_data <- pctl_data %>%
  mutate(pred = predict(lin_mod, pctl_data),
         method = "percentile")

plot_data <- bind_rows(grid_data, pctl_data)

library(ggplot2)
#>
#> Attaching package: ‘ggplot2’
#> The following object is masked from ‘package:kernlab’:
#>
#>     alpha

ggplot(plot_data, aes(x = disp, y = pred)) +
  geom_point(alpha = .5, cex = 1) +
  facet_wrap(~ method)

