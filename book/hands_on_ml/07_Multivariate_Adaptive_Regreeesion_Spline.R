# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 7 Multivariate Adaptive Regression Splines
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/14
# URL       : https://bradleyboehmke.github.io/HOML/mars.html
#           : https://koalaverse.github.io/homlr/notebooks/07-mars.nb.html
# **********************************************************************************


# ＜ポイント＞
# - 6章では正則化した線形モデルを説明したが、これらのモデルは非線形モデルに変換することが可能
#   --- 非線形の項を追加することで可能（二乗項・相互作用効果・元のフィーチャの他の変換）
#   --- 分析者が非線形パターンの性質を事前に知っておく必要がある
#   --- 非線形アルゴリズムでは事前に指定したり知っていたりする必要はない（予測精度最大化の中で勝手に行われる）
# - MARSモデルは非線形性への直感的なステッピングブロックを提供する区分線形モデルを自動的に作成するアルゴリズム

# ＜注意点＞
# -



# ＜目次＞
# 7.1 準備
# 7.2 基本的な考え方
# 7.3 MARSモデルのフィット
# 7.4 チューニング
# 7.5 特徴量の解釈
# 7.6 Attrition Data



# 7.1 準備 ----------------------------------------------

# Helper packages
library(tidyverse)
library(ggplot2)
library(earth)
library(caret)
library(vip)
library(pdp)


# データロード
ames <- AmesHousing::make_ames()


# データ概要
ames %>% as_tibble()
ames %>% glimpse()


# データ分割
set.seed(123)
split      <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train <- split %>% training()
ames_test  <- split %>% testing()



# 7.2 基本的な考え方 ---------------------------------------------

# ＜ポイント＞
# - 線形回帰を用いると、強い仮定を十分に担保できず予測精度が確保できない事態に陥りやすい
#



# 7.2.1 多項回帰 -----------------------

# データ準備
set.seed(123)
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>% filter(x < 6)


# 確認
df %>% as_tibble()


# プロット作成
p1 <-
  df %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("(A) Assumed linear relationship")

p2 <-
  df %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    stat_smooth( method = "lm", se = FALSE, formula = y ~ poly(x, 2, raw = TRUE)) +
    ggtitle("(B) Degree-2 polynomial regression")

p3 <-
  df %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    stat_smooth( method = "lm", se = FALSE, formula = y ~ poly(x, 3, raw = TRUE)) +
    ggtitle("(C) Degree-3 polynomial regression")

# fit step function model (6 steps)
step_fit <- lm(y ~ cut(x, 5), data = df)
step_pred <- predict(step_fit, df)

p4 <-
  df %>%
    cbind(step_pred) %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    geom_line(aes(y = step_pred), size = 1, color = "blue") +
    ggtitle("(D) Step function regression")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)



# 7.2.2 多変量適応回帰スプライン -----------------------

# データ準備
set.seed(123)
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>% filter(x < 6)


# 確認
df %>% as_tibble()


# モデル構築
mars1 <- mda::mars(df$x, df$y, nk = 3, prune = FALSE)
mars2 <- mda::mars(df$x, df$y, nk = 5, prune = FALSE)
mars3 <- mda::mars(df$x, df$y, nk = 7, prune = FALSE)
mars4 <- mda::mars(df$x, df$y, nk = 9, prune = FALSE)


# プロット
p1 <-
  df %>%
    mutate(predicted = as.vector(mars1$fitted.values)) %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    geom_line(aes(y = predicted), size = 1, color = "blue") +
    ggtitle("(A) One knot")

p2 <-
  df %>%
    mutate(predicted = as.vector(mars2$fitted.values)) %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    geom_line(aes(y = predicted), size = 1, color = "blue") +
    ggtitle("(B) Two knots")

p3 <-
  df %>%
    mutate(predicted = as.vector(mars3$fitted.values)) %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    geom_line(aes(y = predicted), size = 1, color = "blue") +
    ggtitle("(C) Three knots")


p4 <-
  df %>%
    mutate(predicted = as.vector(mars4$fitted.values)) %>%
    ggplot(aes(x, y)) +
    geom_point(size = 1, alpha = .2) +
    geom_line(aes(y = predicted), size = 1, color = "blue") +
    ggtitle("(D) Four knots")


gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)




# 7.3 MARSモデルのフィット -----------------------------------------

# Fit a basic MARS model
mars1 <- earth(
  Sale_Price ~ .,
  data = ames_train
)

# Print model summary
print(mars1)

summary(mars1) %>% .$coefficients %>% head(10)



plot(mars1, which = 1)



# Fit a basic MARS model
mars2 <- earth(
  Sale_Price ~ .,
  data = ames_train,
  degree = 2
)

# check out the first 10 coefficient terms
summary(mars2) %>% .$coefficients %>% head(10)



# 7.4 チューニング -----------------------------------------


# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3,
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

head(hyper_grid)



# Cross-validated model
set.seed(123)  # for reproducibility
cv_mars <- train(
  x = subset(ames_train, select = -Sale_Price),
  y = ames_train$Sale_Price,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# View results
cv_mars$bestTune

ggplot(cv_mars)



# 7.5 特徴量の解釈 --------------------------------------------------


# variable importance plots
p1 <- vip(cv_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(cv_mars, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)



# Construct partial dependence plots
p1 <- partial(cv_mars, pred.var = "Gr_Liv_Area", grid.resolution = 10) %>%
  autoplot()

p2 <- partial(cv_mars, pred.var = "Year_Built", grid.resolution = 10) %>%
  autoplot()

p3 <- partial(cv_mars, pred.var = c("Gr_Liv_Area", "Year_Built"),
              grid.resolution = 10) %>%
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE,
              screen = list(z = -20, x = -60))

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)




# 7.6 Attrition Data --------------------------------------------------

# get attrition data
df <- rsample::attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Create training (70%) and test (30%) sets for the rsample::attrition data.
# Use set.seed for reproducibility
set.seed(123)
churn_split <- rsample::initial_split(df, prop = .7, strata = "Attrition")
churn_train <- rsample::training(churn_split)
churn_test  <- rsample::testing(churn_split)


# for reproducibiity
set.seed(123)

# cross validated model
tuned_mars <- train(
  x = subset(churn_train, select = -Attrition),
  y = churn_train$Attrition,
  method = "earth",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# best model
tuned_mars$bestTune


# plot results
ggplot(tuned_mars)


# train logistic regression model
set.seed(123)
glm_mod <- train(
  Attrition ~ .,
  data = churn_train,
  method = "glm",
  family = "binomial",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10)
  )

# train regularized logistic regression model
set.seed(123)
penalized_mod <- train(
  Attrition ~ .,
  data = churn_train,
  method = "glmnet",
  family = "binomial",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
  )

# extract out of sample performance measures
summary(resamples(list(
  Logistic_model = glm_mod,
  Elastic_net = penalized_mod,
  MARS_model = tuned_mars
  )))$statistics$Accuracy %>%
  kableExtra::kable(caption = "Cross-validated accuracy results for tuned MARS and regression models.") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))



