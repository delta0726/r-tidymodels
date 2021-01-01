# Title     : Classification models using a neural network
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/12
# URL       : https://www.tidymodels.org/learn/models/parsnip-nnet/



library(tidyverse)
library(tidymodels)

# データロード
data(bivariate)


# データ確認
bivariate_train %>% print()
bivariate_train %>% names()
bivariate_train %>% glimpse()


# レコード数の確認
bivariate_train %>% nrow()
bivariate_val %>% nrow()


# プロット
# --- 分布がかなり歪んでいることを確認
# --- XYともに正の値のみ
bivariate_train %>%
  ggplot(aes(x = A, y = B, col = Class)) +
    geom_point(alpha = .2)


# レシピ作成
biv_rec <-
  recipe(Class ~ ., data = bivariate_train) %>%
  step_BoxCox(all_predictors())%>%
  step_normalize(all_predictors()) %>%
  prep(training = bivariate_train, retain = TRUE)


# レシピ適用
val_normalized <-
  biv_rec %>%
    bake(new_data = bivariate_val, all_predictors())


test_normalized <-
  biv_rec %>%
    bake(new_data = bivariate_test, all_predictors())


# モデル構築＆学習
set.seed(57974)
nnet_fit <-
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
    set_mode("classification") %>%
    set_engine("keras", verbose = 0) %>%
    fit(Class ~ ., data = juice(biv_rec))


# 確認
nnet_fit



val_results <-
  bivariate_val %>%
  bind_cols(
    predict(nnet_fit, new_data = val_normalized),
    predict(nnet_fit, new_data = val_normalized, type = "prob")
  )
val_results %>% slice(1:5)
#> # A tibble: 5 x 6
#>       A     B Class .pred_class .pred_One .pred_Two
#>   <dbl> <dbl> <fct> <fct>           <dbl>     <dbl>
#> 1 1061.  74.5 One   Two             0.473    0.527
#> 2 1241.  83.4 One   Two             0.484    0.516
#> 3  939.  71.9 One   One             0.636    0.364
#> 4  813.  77.1 One   One             0.925    0.0746
#> 5 1706.  92.8 Two   Two             0.355    0.645

val_results %>% roc_auc(truth = Class, .pred_One)
#> # A tibble: 1 x 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.815

val_results %>% accuracy(truth = Class, .pred_class)
#> # A tibble: 1 x 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy binary         0.737

val_results %>% conf_mat(truth = Class, .pred_class)
#>           Truth
#> Prediction One Two
#>        One 150  27
#>        Two  52  71
a_rng <- range(bivariate_train$A)
b_rng <- range(bivariate_train$B)
x_grid <-
  expand.grid(A = seq(a_rng[1], a_rng[2], length.out = 100),
              B = seq(b_rng[1], b_rng[2], length.out = 100))
x_grid_trans <- bake(biv_rec, x_grid)

# Make predictions using the transformed predictors but
# attach them to the predictors in the original units:
x_grid <-
  x_grid %>%
  bind_cols(predict(nnet_fit, x_grid_trans, type = "prob"))

ggplot(x_grid, aes(x = A, y = B)) +
  geom_contour(aes(z = .pred_One), breaks = .5, col = "black") +
  geom_point(data = bivariate_val, aes(col = Class), alpha = 0.3)

