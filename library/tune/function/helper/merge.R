# Title     : merge
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/17
# URL       : https://tune.tidymodels.org/reference/merge.recipe.html



# ＜ポイント＞
# - よくわからないので、再度学習




library(tidyverse)
library(tidymodels)


# データ確認
mtcars %>% as_tibble()
mtcars %>% glimpse()


# チューニンググリッドとレシピの結合 ------------------------------------------

# レシピ作成
# --- チューニングあり
pca_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_knnimpute(all_predictors(), neighbors = tune()) %>%
  step_pca(all_predictors(), num_comp = tune())


# チューニング用のグリッド
pca_grid <-
  tribble(
    ~neighbors, ~num_comp,
             1,         1,
             5,         1,
             1,         2,
             5,         2
  )


# 確認
X_rec <- pca_rec %>% merge(pca_grid)#> # A tibble: 4 x 1
X_rec %>% print()
X_rec$x[[1]] %>% tidy()




# チューニンググリッドとレシピの結合 ------------------------------------------

spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = tune("disp df")) %>%
  step_ns(wt, deg_free = tune("wt df"))

spline_grid <-
  tribble(
    ~"disp df", ~ "wt df",
    3,         3,
    5,         3,
    3,         5,
    5,         5
  )

merge(pca_rec, pca_grid)#> # A tibble: 4 x 1
#>   x
#>   <list>
#> 1 <recipe>
#> 2 <recipe>
#> 3 <recipe>
#> 4 <recipe>


# チューニンググリッドとレシピの結合 ------------------------------------------

# モデル構築
# --- チューニングあり
xgb_mod <-
  boost_tree(trees = tune(), min_n = tune()) %>%
  set_engine("xgboost")


# 確認
xgb_mod %>% print()



set.seed(254)
xgb_grid <-
  xgb_mod %>%
    dials::parameters() %>%
    finalize(iris) %>%
    grid_max_entropy(size = 3)


xgb_mod %>% merge(xgb_grid)


