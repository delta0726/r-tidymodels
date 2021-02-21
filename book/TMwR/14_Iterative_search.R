#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 14 Iterative search
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/13
# URL       : https://www.tmwr.org/
#***************************************************************************************



data(cells)

svm_rec <-
  recipe(class ~ ., data = cells) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_normalize(all_predictors())

svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_wflow <-
  workflow() %>%
  add_model(svm_spec) %>%
  add_recipe(svm_rec)


cost()
#> Cost (quantitative)
#> Transformer:  log-2
#> Range (transformed scale): [-10, 5]
rbf_sigma()
#> Radial Basis Function sigma (quantitative)
#> Transformer:  log-10
#> Range (transformed scale): [-10, 0]

svm_param <-
  svm_wflow %>%
  parameters() %>%
  update(rbf_sigma = rbf_sigma(c(-7, -1)))


set.seed(234)
start_grid <-
  svm_param %>%
  update(
    cost = cost(c(-6, 1)),
    rbf_sigma = rbf_sigma(c(-6, -4))
  ) %>%
  grid_regular(levels = 2)

set.seed(2)
svm_initial <-
  svm_wflow %>%
  tune_grid(resamples = cell_folds, grid = start_grid, metrics = roc_res)

collect_metrics(svm_initial)


ctrl <- control_bayes(verbose = TRUE)

set.seed(1234)
svm_bo <-
  svm_wflow %>%
  tune_bayes(
    resamples = cell_folds,
    metrics = roc_res,
    initial = svm_initial,
    param_info = svm_param,
    iter = 25,
    control = ctrl
  )

autoplot(svm_bo, type = "performance")

ctrl_sa <- control_sim_anneal(verbose = TRUE, no_improve = 10L)

set.seed(1234)
svm_sa <-
  svm_wflow %>%
  tune_sim_anneal(
    resamples = cell_folds,
    metrics = roc_res,
    initial = svm_initial,
    param_info = svm_param,
    iter = 50,
    control = ctrl_sa
  )

autoplot(svm_sa, type = "performance")
autoplot(svm_sa, type = "parameters")