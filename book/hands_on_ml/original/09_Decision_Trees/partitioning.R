# ********************************************************************************
# Title   : 決定木のイメージを確認
# Chapter : 9
# URL     : https://bradleyboehmke.github.io/HOML/DT.html
# Support : https://koalaverse.github.io/homlr/notebooks/09-decision-trees.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(rpart)
library(rpart.plot)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)


# 1.準備 -----------------------------------------------------

# データ作成
set.seed(1112)
df <- tibble::tibble(
  x = seq(from = 0, to = 2 * pi, length = 500),
  y = sin(x) + rnorm(length(x), sd = 0.5),
  truth = sin(x)
)

# レコード数
df %>% nrow()


# 2.モデル構築：1層 -----------------------------------------------------

# モデル構築
# --- maxdepth = 1
fit <- rpart(y ~ x, data = df, control = list(cp = 0, minbucket = 5, maxdepth = 1))
fit %>% print()

# ツリープロット
fit %>% rpart.plot()

# プロット
# --- 散布図と予測値
df %>%
  mutate(pred = predict(fit, df)) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .2, size = 1) +
  geom_line(aes(x, y = truth), color = "blue", size = .75) +
  geom_line(aes(y = pred), color = "red", size = .75) +
  geom_segment(x = 3.1, xend = 3.1, y = -Inf, yend = -.95,
               arrow = arrow(length = unit(0.25,"cm")), size = .25) +
  annotate("text", x = 3.1, y = -Inf, label = "split", hjust = 1.2, vjust = -1, size = 3) +
  geom_segment(x = 5.5, xend = 6, y = 2, yend = 2, size = .75, color = "blue") +
  geom_segment(x = 5.5, xend = 6, y = 1.7, yend = 1.7, size = .75, color = "red") +
  annotate("text", x = 5.3, y = 2, label = "truth", hjust = 1, size = 3, color = "blue") +
  annotate("text", x = 5.3, y = 1.7, label = "decision boundary", hjust = 1, size = 3, color = "red")



# 3.モデル構築：3層 -----------------------------------------------------

# モデル構築
# --- maxdepth = 3
fit <- rpart(y ~ x, data = df, control = list(cp = 0, minbucket = 5, maxdepth = 3))

# ツリープロット
fit %>% rpart.plot()

# プロット
# --- 散布図と予測値
df %>%
  mutate(pred = predict(fit, df)) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .2, size = 1) +
  geom_line(aes(x, y = truth), color = "blue", size = .75) +
  geom_line(aes(y = pred), color = "red", size = .75)




# 4.irisの事例 -----------------------------------------------------


# モデル構築
iris_fit <- rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

# ツリープロット
iris_fit %>% rpart.plot()

# decision boundary
iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, color = Species, shape = Species)) +
    geom_point(show.legend = FALSE) +
    annotate("rect", xmin = -Inf, xmax = 5.44, ymin = 2.8, ymax = Inf, alpha = .75, fill = "orange") +
    annotate("text", x = 4.0, y = 4.4, label = "setosa", hjust = 0, size = 3) +
    annotate("rect", xmin = -Inf, xmax = 5.44, ymin = 2.79, ymax = -Inf, alpha = .75, fill = "grey") +
    annotate("text", x = 4.0, y = 2, label = "versicolor", hjust = 0, size = 3) +
    annotate("rect", xmin = 5.45, xmax = 6.15, ymin = 3.1, ymax = Inf, alpha = .75, fill = "orange") +
    annotate("text", x = 6, y = 4.4, label = "setosa", hjust = 1, vjust = 0, size = 3) +
    annotate("rect", xmin = 5.45, xmax = 6.15, ymin = 3.09, ymax = -Inf, alpha = .75, fill = "grey") +
    annotate("text", x = 6.15, y = 2, label = "versicolor", hjust = 1, vjust = 0, fill = "grey", size = 3) +
    annotate("rect", xmin = 6.16, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .75, fill = "green") +
    annotate("text", x = 8, y = 2, label = "virginica", hjust = 1, vjust = 0, fill = "green", size = 3)




# 5.深さをどれくらいにするか：複雑すぎるモデル ---------------------------------

# モデル構築
# --- maxdepth = 50
fit <- rpart(y ~ x, data = df, control = list(cp = 0, minbucket = 1, maxdepth = 50))

# ツリープロット
fit %>% rpart.plot()

# プロット
# --- 散布図と予測値
# --- 複雑すぎる
df %>%
  mutate(pred = predict(fit, df)) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .2, size = 1) +
  geom_line(aes(x, y = truth), color = "blue", size = 0.75) +
  geom_line(aes(y = pred), color = "red", size = 0.75)



# アーリーストッピング -----------------------------------------------------

hyper_grid <- expand.grid(
  maxdepth = c(1, 5, 15),
  minbucket = c(1, 5, 15)
)
results <- data.frame(NULL)

for(i in seq_len(nrow(hyper_grid))) {
  ctrl <- list(cp = 0, maxdepth = hyper_grid$maxdepth[i], minbucket = hyper_grid$minbucket[i])
  fit <- rpart(y ~ x, data = df, control = ctrl) 
  
  predictions <- mutate(
    df, 
    minbucket = factor(paste("Min node size =", hyper_grid$minbucket[i]), ordered = TRUE),
    maxdepth = factor(paste("Max tree depth =", hyper_grid$maxdepth[i]), ordered = TRUE)
  )
  predictions$pred <- predict(fit, df)
  results <- rbind(results, predictions)
  
}


# プロット
# --- 散布図と予測値
ggplot(results, aes(x, y)) +
  geom_point(alpha = .2, size = 1) +
  geom_line(aes(x, y = truth), color = "blue", size = .75) +
  geom_line(aes(y = pred), color = "red", size = 1) +
  facet_grid(minbucket ~ maxdepth)


# 6.階層別シミュレーション -----------------------------------------------------


# 複雑な決定木 -----
ctrl <- list(cp = 0, minbucket = 1, maxdepth = 50)
fit <- rpart(y ~ x, data = df, control = ctrl)

p1 <- df %>%
  mutate(pred = predict(fit, df)) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3, size = 2) +
  geom_line(aes(x, y = truth), color = "blue", size = 1) +
  geom_line(aes(y = pred), color = "red", size = 1)


# 単純な決定木 -----
fit2 <- rpart(y ~ x, data = df)

p2 <- df %>%
  mutate(pred2 = predict(fit2, df)) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3, size = 2) +
  geom_line(aes(x, y = truth), color = "blue", size = 1) +
  geom_line(aes(y = pred2), color = "red", size = 1)+

gridExtra::grid.arrange(p1, p2, nrow = 1)

