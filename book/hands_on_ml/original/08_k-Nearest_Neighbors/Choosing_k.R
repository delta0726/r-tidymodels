# ********************************************************************************
# Title   : K近傍法
# Theme   : Choosing k
# Chapter : 9
# URL     : https://bradleyboehmke.github.io/HOML/knn.html
# Support : https://koalaverse.github.io/homlr/notebooks/08-knn.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(caret)
library(ggmap)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞




# 1. データ準備 ----------------------------------------------


# データ取得
mnist <- dslabs::read_mnist()
names(mnist)

# データ分割
set.seed(123)
index <- mnist$train$images %>% nrow() %>% sample(size = 10000)
mnist_x <- mnist$train$images[index, ]
mnist_y <- mnist$train$labels[index] %>% factor()

# データ確認
mnist_x %>% dim()
mnist_y %>% length()




# 2.プロット ----------------------------------------------

mnist_x %>%
  as.data.frame() %>%
  map_df(sd) %>%
  gather(feature, sd) %>%
  ggplot(aes(sd)) +
  geom_histogram(binwidth = 1)



# 3. クロスバリデーション ----------------------------------------------

nzv <- nearZeroVar(mnist_x)
par(mfrow = c(1, 4))
i <- 2
image(1:28, 1:28, matrix(mnist$test$images[i,], nrow=28)[ , 28:1], 
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="", 
      xaxt="n", yaxt="n", main = "(A) Example image \nfor digit 2")
i <- 7
image(1:28, 1:28, matrix(mnist$test$images[i,], nrow=28)[ , 28:1], 
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="", 
      xaxt="n", yaxt="n", main = "(B) Example image \nfor digit 4")


i <- 9
image(1:28, 1:28, matrix(mnist$test$images[i,], nrow=28)[ , 28:1], 
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="", 
      xaxt="n", yaxt="n", main = "(C) Example image \nfor digit 5")
image(matrix(!(1:784 %in% nzv), 28, 28), col = gray(seq(0, 1, 0.05)), 
      xaxt="n", yaxt="n", main = "(D) Typical variability \nin images.")


# Rename features
colnames(mnist_x) <- paste0("V", 1:ncol(mnist_x))

# Remove near zero variance features manually
nzv <- nearZeroVar(mnist_x)
index <- setdiff(1:ncol(mnist_x), nzv)
mnist_x <- mnist_x[, index]



# Use train/validate resampling method
cv <- trainControl(
  method = "LGOCV", 
  p = 0.7,
  number = 1,
  savePredictions = TRUE
)

# Create a hyperparameter grid search
hyper_grid <- expand.grid(k = seq(3, 25, by = 2))

# Execute grid search
knn_mnist <- train(
  mnist_x,
  mnist_y,
  method = "knn",
  tuneGrid = hyper_grid,
  preProc = c("center", "scale"),
  trControl = cv
)

ggplot(knn_mnist)



# Create confusion matrix
cm <- confusionMatrix(knn_mnist$pred$pred, knn_mnist$pred$obs)
cm$byClass[, c(1:2, 11)]  # sensitivity, specificity, & accuracy



# Top 20 most important features
vi <- varImp(knn_mnist)
vi



# Get median value for feature importance
imp <- vi$importance %>%
  rownames_to_column(var = "feature") %>%
  gather(response, imp, -feature) %>%
  group_by(feature) %>%
  summarize(imp = median(imp))

# Create tibble for all edge pixels
edges <- tibble(
  feature = paste0("V", nzv),
  imp = 0
)

# Combine and plot
imp <- rbind(imp, edges) %>%
  mutate(ID  = as.numeric(str_extract(feature, "\\d+"))) %>%
  arrange(ID)
image(matrix(imp$imp, 28, 28), col = gray(seq(0, 1, 0.05)), 
      xaxt="n", yaxt="n")



# Get a few accurate predictions
set.seed(9)
good <- knn_mnist$pred %>%
  filter(pred == obs) %>%
  sample_n(4)

# Get a few inaccurate predictions
set.seed(9)
bad <- knn_mnist$pred %>%
  filter(pred != obs) %>%
  sample_n(4)

combine <- bind_rows(good, bad)

# Get original feature set with all pixel features
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
X <- mnist$train$images[index,]

# Plot results
par(mfrow = c(4, 2), mar=c(1, 1, 1, 1))
layout(matrix(seq_len(nrow(combine)), 4, 2, byrow = FALSE))
for(i in seq_len(nrow(combine))) {
  image(matrix(X[combine$rowIndex[i],], 28, 28)[, 28:1], 
        col = gray(seq(0, 1, 0.05)),
        main = paste("Actual:", combine$obs[i], "  ", 
                     "Predicted:", combine$pred[i]),
        xaxt="n", yaxt="n") 
}
  
  
  
  
  