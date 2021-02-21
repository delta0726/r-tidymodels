# ********************************************************************************
# Title   : Gradient Boosting
# Chapter : 12
# URL     : https://bradleyboehmke.github.io/HOML/gbm.html
# Support : https://koalaverse.github.io/homlr/notebooks/10-bagging.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(gbm)
library(xgboost)
library(vip)
library(h2o)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞
#・バギングは低バイアスで高分散のモデルにより効果的
#・ブースティングは高バイアスで低分散のモデルにより効果的
#・最も強力なアンサンブルアルゴリズムで予測精度は最高級とされる



# 1. データ準備 ----------------------------------------------


# データ取得
ames <- read_csv("data/ames.csv")


# データ分割
set.seed(123) 
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# データ確認
ames_train %>% dim()
ames_test %>% dim()




 
# 3.基本的なGBMモデル  -------------------------------------

# 乱数シード
set.seed(123)

# 文字列をファクターに変換
ames_train_fct <- 
  ames_train %>% 
    mutate_if(is.character, as.factor)


# 文字列をファクターに変換
ames_train_fct <- 
  ames_train %>% 
  select_if(is.numeric)


# 確認
ames_train_fct %>% glimpse()

# モデル構築
ames_gbm1 <- 
  gbm(formula = Sale_Price ~ .,
      data = ames_train,
      distribution = "gaussian",  # SSE loss function
      n.trees = 5000,
      shrinkage = 0.1,
      interaction.depth = 3,
      n.minobsinnode = 10,
      cv.folds = 5
      )


# find index for number trees with minimum CV error
best <- ames_gbm1$cv.error %>% which.min()
best %>% print()

# get MSE and compute RMSE
sqrt(ames_gbm1$cv.error[best])


gbm.perf(ames_gbm1, method = "cv")



# create grid search
hyper_grid <- expand.grid(
  learning_rate = c(0.3, 0.1, 0.05, 0.01, 0.005),
  RMSE = NA,
  trees = NA,
  time = NA
)

# execute grid search
for(i in seq_len(nrow(hyper_grid))) {
  
  # fit gbm
  set.seed(123)  # for reproducibility
  train_time <- system.time({
    m <- gbm(
      formula = Sale_Price ~ .,
      data = ames_train,
      distribution = "gaussian",
      n.trees = 5000, 
      shrinkage = hyper_grid$learning_rate[i], 
      interaction.depth = 3, 
      n.minobsinnode = 10,
      cv.folds = 10 
    )
  })
  
  # add SSE, trees, and training time to results
  hyper_grid$RMSE[i]  <- sqrt(min(m$cv.error))
  hyper_grid$trees[i] <- which.min(m$cv.error)
  hyper_grid$Time[i]  <- train_time[["elapsed"]]
  
}

# results
arrange(hyper_grid, RMSE)



# search grid
hyper_grid <- expand.grid(
  n.trees = 6000,
  shrinkage = 0.01,
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 10, 15)
)

# create model fit function
model_fit <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode) {
  set.seed(123)
  m <- gbm(
    formula = Sale_Price ~ .,
    data = ames_train,
    distribution = "gaussian",
    n.trees = n.trees,
    shrinkage = shrinkage,
    interaction.depth = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    cv.folds = 10
  )
  # compute RMSE
  sqrt(min(m$cv.error))
}

# perform search grid with functional programming
hyper_grid$rmse <- purrr::pmap_dbl(
  hyper_grid,
  ~ model_fit(
    n.trees = ..1,
    shrinkage = ..2,
    interaction.depth = ..3,
    n.minobsinnode = ..4
  )
)

# results
arrange(hyper_grid, rmse)




# 3.確率的GBM  -------------------------------------

# H2Oの起動
h2o.init(max_mem_size = "10g")


# 変数設定
train_h2o  <- ames_train %>% as.h2o()
response   <- "Sale_Price"
predictors <- ames_train %>% colnames() %>% setdiff(response)


# パラメータの設定
hyper_grid <- list(
  sample_rate = c(0.5, 0.75, 1),              # row subsampling
  col_sample_rate = c(0.5, 0.75, 1),          # col subsampling for each split
  col_sample_rate_per_tree = c(0.5, 0.75, 1)  # col subsampling for each tree
)

# 確認
hyper_grid %>% print()


# サーチ戦略の設定
search_criteria <- 
  list(strategy = "RandomDiscrete",
       stopping_metric = "mse",
       stopping_tolerance = 0.001,   
       stopping_rounds = 10,         
       max_runtime_secs = 60*60
       )

# perform grid search 
grid <- 
  h2o.grid(algorithm = "gbm",
           grid_id = "gbm_grid",
           x = predictors, 
           y = response,
           training_frame = train_h2o,
           hyper_params = hyper_grid,
           ntrees = 6000,
           learn_rate = 0.01,
           max_depth = 7,
           min_rows = 5,
           nfolds = 10,
           stopping_rounds = 10,
           stopping_tolerance = 0,
           search_criteria = search_criteria,
           seed = 123
          )

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "gbm_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)

grid_perf


# Grab the model_id for the top model, chosen by cross validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s get performance metrics on the best model
h2o.performance(model = best_model, xval = TRUE)




library(recipes)
xgb_prep <- recipe(Sale_Price ~ ., data = ames_train) %>%
  step_integer(all_nominal()) %>%
  prep(training = ames_train, retain = TRUE) %>%
  juice()

X <- as.matrix(xgb_prep[setdiff(names(xgb_prep), "Sale_Price")])
Y <- xgb_prep$Sale_Price



set.seed(123)
ames_xgb <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "reg:linear",
  early_stopping_rounds = 50, 
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)  

# minimum test CV RMSE
min(ames_xgb$evaluation_log$test_rmse_mean)



# hyperparameter grid
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  rmse = 0,          # a place to dump RMSE results
  trees = 0          # a place to dump required number of trees
)

# grid search
for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "reg:linear",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$rmse[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

# results
hyper_grid %>%
  filter(rmse > 0) %>%
  arrange(rmse) %>%
  glimpse()

# optimal parameter list
params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 3944,
  objective = "reg:linear",
  verbose = 0
)

# variable importance plot
vip::vip(xgb.fit.final) 