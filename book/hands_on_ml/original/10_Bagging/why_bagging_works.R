# ********************************************************************************
# Title   : バギング
# Chapter : 10
# URL     : https://bradleyboehmke.github.io/HOML/bagging.html
# Support : https://koalaverse.github.io/homlr/notebooks/10-bagging.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(rpart)
library(rpart.plot)
library(ipred)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# ＜ポイント＞
#・バギングとは、ブートストラップを使用して作成したモデルの平均化のことをいう
#・バギングの背景には｢群衆の叡智｣の効果の存在がある



# 0.データ作成 ----------------------------------------------------------

# Simulate some nonlinear monotonic data
set.seed(123) 
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>% filter(x < 4.5)

# データ数
df %>% dim()



# 1.Polynominal Regression -----------------------------------------------

# パラメータ
bootstrap_n <- 100
bootstrap_results <- NULL


# デバッグ用
i <- 1


# モデルを作成
# --- ブートストラップ
for(i in seq_len(bootstrap_n)) {

  # データサンプリング
  #　--- 同じレコードを含む
  set.seed(i)
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]

  # 確認
  # --- 元データと同じレコード数
  df_sim %>% nrow()
  
  # モデル構築
  fit <- lm(y ~ I(x^3), data = df_sim)
  
  # 予測の作成
  df_sim$predictions <- fit %>% predict(df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}


# ブートストラップの確認
# --- シミュレーションモデルごとにプロット
bootstrap_results %>% 
  group_by(model) %>% 
  tally()


# プロット作成
# --- 低分散ベース学習器(多項式回帰)は同じような結果にしかならない
bootstrap_results %>% 
  ggplot(aes(x, predictions)) +
  geom_point(data = df, aes(x, y), alpha = .25) +
  geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2)


#プロット作成
# --- 比較用
p1 <- 
  bootstrap_results %>% 
    ggplot(aes(x, predictions)) +
    geom_point(data = df, aes(x, y), alpha = .25) +
    geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2) +
    stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "line") +
    scale_y_continuous("Response", limits = c(-2, 2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
    ggtitle("A) Polynomial regression")



# 2.MARS -----------------------------------------------

# パラメータ
bootstrap_n <- 100
bootstrap_results <- NULL


# モデルを作成
# --- ブートストラップ
for(i in seq_len(bootstrap_n)) {

  # データサンプリング
  #　--- 同じレコードを含む
  set.seed(i)
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]
  
  # 確認
  # --- 元データと同じレコード数
  df_sim %>% nrow()
  
  # モデル構築
  fit <- earth::earth(y ~ x, data = df_sim)
  
  # 予測の作成
  df_sim$predictions <- fit %>% predict(df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}


# プロット作成
# --- 低分散ベース学習器(MARS回帰)は同じような結果にしかならない
bootstrap_results %>% 
  ggplot(aes(x, predictions)) +
  geom_point(data = df, aes(x, y), alpha = .25) +
  geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2)


#プロット作成
# --- 比較用
p2 <- 
  bootstrap_results %>% 
    ggplot(aes(x, predictions)) +
    geom_point(data = df, aes(x, y), alpha = .25) +
    geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2) +
    stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "line") +
    scale_y_continuous(NULL, limits = c(-2, 2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
    ggtitle("B) MARS")


# 3.Decision Tree -----------------------------------------------

# パラメータ
bootstrap_n <- 100
bootstrap_results <- NULL

# モデルを作成
# --- ブートストラップ
for(i in seq_len(bootstrap_n)) {

  # データサンプリング
  #　--- 同じレコードを含む
  set.seed(i)
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]
  
  # 確認
  # --- 元データと同じレコード数
  df_sim %>% nrow()
  
  # モデル構築
  fit <- rpart::rpart(y ~ x, data = df_sim)
  
  # 予測の作成
  df_sim$predictions <- predict(fit, df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}


# プロット作成
# --- 高分散学習器(決定木)ではシミュレーションの分散が大幅に増加
bootstrap_results %>% 
  ggplot(aes(x, predictions)) +
  geom_point(data = df, aes(x, y), alpha = .25) +
  geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2)


#プロット作成
# --- 比較用
p3 <- 
  bootstrap_results %>% 
    ggplot(aes(x, predictions)) +
    geom_point(data = df, aes(x, y), alpha = .25) +
    geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2) +
    stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "line") +
    scale_y_continuous(NULL, limits = c(-2, 2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
    ggtitle("C) Decision trees")


# 4.プロット比較 -----------------------------------------------

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


