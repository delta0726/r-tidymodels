# ********************************************************************************
# Title   : ランダムフォレスト
# Chapter : 11
# URL     : https://bradleyboehmke.github.io/HOML/random-forest.html
# Support : https://koalaverse.github.io/homlr/notebooks/11-random-forests.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(ranger)
library(h2o)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)
library(ggRandomForests)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞
#・決定木のバギングを基本としたアルゴリズム
#・相関関係のないツリーを大規模に作成してパフォーマンス改善を目指している
#・比較的少ないパラメータチューニングで良好なパフォーマンスを実現
#・バギングはすべての変数を用いるがRFは使用する変数をサンプリング（高次元解析が可能）




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


# 特徴量の数
n_features <- ames_train %>% names() %>% setdiff("Sale_Price") %>% length()
n_features



# 2.単純なモデルの実行 ----------------------------------------------

# モデル構築
# --- 小規模モデル
# --- mtry = floor(n_features / 3)
ames_rf1 <- 
  ranger(Sale_Price ~ ., 
         data = ames_train,
         mtry = floor(n_features / 3),
         respect.unordered.factors = "order",
         seed = 123
         )


# 確認
ames_rf1 %>% print()

# サマリー
ames_rf1 %>% summary()

# RMSE
default_rmse <- ames_rf1$prediction.error %>% sqrt()
default_rmse %>% print()



# 3-1.ハイパーパラメター(木の数) ----------------------------------------------

# 特徴量の数
n_features <- ames_train %>% ncol() - 1

# チューニンググリッドの作成
tuning_grid <- expand.grid(
  trees = seq(10, 1000, by = 20),
  rmse  = NA
)

# チューニング数
tuning_grid %>% nrow()

# グリッドサーチ
for(i in seq_len(nrow(tuning_grid))) {
  
  # モデル構築
  # --- num.trees = tuning_grid$trees[i]
  fit <- ranger(
    formula = Sale_Price ~ ., 
    data = ames_train, 
    num.trees = tuning_grid$trees[i],
    mtry = floor(n_features / 3),
    respect.unordered.factors = 'order',
    verbose = FALSE,
    seed = 123
  )
  
  # Extract OOB RMSE
  tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
  
}


# プロット
tuning_grid %>% 
  ggplot(aes(trees, rmse)) +
    geom_line(size = 1) +
    ylab("OOB Error (RMSE)") +
    xlab("Number of trees")



# 3-2.ハイパーパラメター(分岐に用いる変数の数) -----------------------------

# ＜mtry＞
# - 分類問題の場合は特徴量の数の平方根あたりに設定
# - 回帰問題の場合は特徴量の数の1/3あたりに設定
 

# チューニンググリッドの作成
# --- 特徴量2-80(最大)
tuning_grid <- expand.grid(
  trees = seq(10, 1000, by = 20),
  mtry  = floor(c(seq(2, 80, length.out = 5), 26)),
  rmse  = NA
)

# チューニング数
tuning_grid %>% nrow()


# グリッドサーチ
for(i in seq_len(nrow(tuning_grid))) {
  # モデル構築
  # --- tuning_grid$mtry[i]
  fit <- ranger(
    formula    = Sale_Price ~ ., 
    data       = ames_train, 
    num.trees  = tuning_grid$trees[i],
    mtry       = tuning_grid$mtry[i],
    respect.unordered.factors = 'order',
    verbose    = FALSE,
    seed       = 123
  )
  
  tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
  
}

labels <- tuning_grid %>%
  filter(trees == 990) %>%
  mutate(mtry = as.factor(mtry))

# プロット
tuning_grid %>%
  mutate(mtry = as.factor(mtry)) %>%
  ggplot(aes(trees, rmse, color = mtry)) +
  geom_line(size = 1, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = labels, aes(trees, rmse, label = mtry), nudge_x = 50, show.legend = FALSE) +
  ylab("OOB Error (RMSE)")
  
  


# 3-3.ハイパーパラメター(ツリーの複雑さ) -----------------------------

# ＜min.node.size＞
# - 分類問題の場合はデフォルトで1に設定
# - 回帰問題の場合はデフォルトで5に設定


# チューニンググリッドの作成
  tuning_grid <- expand.grid(
    min.node.size = 1:20,
    run_time  = NA,
    rmse = NA
  )


# チューニング数
tuning_grid %>% nrow()


# グリッドサーチ
# --- ノードサイズ
# --- min.node.size
for(i in seq_len(nrow(tuning_grid))) {
  fit_time <- system.time({
    # モデル構築
    fit <- ranger(
      formula    = Sale_Price ~ ., 
      data       = ames_train, 
      num.trees  = 1000,
      mtry       = 26,
      min.node.size = tuning_grid$min.node.size[i],
      respect.unordered.factors = 'order',
      verbose    = FALSE,
      seed       = 123
    )
  })
  
  tuning_grid$run_time[i] <- fit_time[[3]]
  tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
  
}

min_node_size <- tuning_grid %>% 
  mutate(
    error_first = first(rmse),
    runtime_first = first(run_time),
    `Error Growth` = (rmse / error_first) - 1,
    `Run Time Reduction` = (run_time / runtime_first) - 1
  )

p1 <-  
  min_node_size %>% 
    ggplot(aes(min.node.size, `Error Growth`)) +
    geom_smooth(size = 1, se = FALSE, color = "black") +
    scale_y_continuous("Percent growth in error estimate", labels = scales::percent) +
    xlab("Minimum node size") +
    ggtitle("A) Impact to error estimate")

p2 <-  
  min_node_size %>% 
    ggplot(aes(min.node.size, `Run Time Reduction`)) +
    geom_smooth(size = 1, se = FALSE, color = "black") +
    scale_y_continuous("Reduction in run time", labels = scales::percent) +
    xlab("Minimum node size") +
    ggtitle("B) Impact to run time")

gridExtra::grid.arrange(p1, p2, nrow = 1)
  xlab("Number of trees")

  

# 3-4.ハイパーパラメター(サンプリングスキーム) -----------------------------
  
  # チューニンググリッドの作成
  tuning_grid <- expand.grid(
    sample.fraction = seq(.05, .95, by = .05),
    replace  = c(TRUE, FALSE),
    rmse = NA
  )
  
  # チューニング数
  tuning_grid %>% nrow()
  
  # グリッドサーチ
  # --- sample.fraction
  # ---replace
  for(i in seq_len(nrow(tuning_grid))) {
    fit <- ranger(
      formula    = Sale_Price ~ ., 
      data       = ames_train, 
      num.trees  = 1000,
      mtry       = 26,
      sample.fraction = tuning_grid$sample.fraction[i],
      replace = tuning_grid$replace[i],
      respect.unordered.factors = 'order',
      verbose    = FALSE,
      seed       = 123
    )
    
    tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
    
  }
  
  tuning_grid %>%
    ggplot(aes(sample.fraction, rmse, color = replace)) +
    geom_line(size = 1) +
    scale_x_continuous("Sample Fraction", breaks = seq(.1, .9, by = .1), labels = scales::percent) +
    ylab("OOB Error (RMSE)") +
    scale_color_discrete("Sample with Replacement") +
    theme(legend.position = c(0.8, 0.85),
          legend.key = element_blank(),
          legend.background = element_blank())
  
  
  
# 5.チューニング戦略 ----------------------------------------------

  # ＜ポイント＞
  # - ranger自体はグリッドサーチの仕組みを持たない
  # - ここではパターンを作成してループで逐次処理を行う
    
  # ハイパーパラメターの設定
  # --- グリッドをデータフレームにしておく
  hyper_grid <- expand.grid(
    mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
    min.node.size = c(1, 3, 5, 10), 
    replace = c(TRUE, FALSE),                               
    sample.fraction = c(.5, .63, .8),                       
    rmse = NA                                               
  )
  
  # グリッド数
  hyper_grid %>% dim()
  
  
  # デカルトグリッドサーチ
  # --- RMSのみを保持
  # --- モデル自体は保持しない
  for(i in seq_len(nrow(hyper_grid))) {
    # モデル構築
    # --- ハイパーパラメターを適用
    fit <- ranger(
      formula         = Sale_Price ~ ., 
      data            = ames_train, 
      num.trees       = n_features * 10,
      mtry            = hyper_grid$mtry[i],
      min.node.size   = hyper_grid$min.node.size[i],
      replace         = hyper_grid$replace[i],
      sample.fraction = hyper_grid$sample.fraction[i],
      verbose         = FALSE,
      seed            = 123,
      respect.unordered.factors = 'order',
    )
    # export OOB error 
    hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
  }
  
  # assess top 10 models
  hyper_grid %>%
    arrange(rmse) %>%
    mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
    head(10)
  
  # プロット
  hyper_grid$rmse %>% sort() %>% plot()
  

  
# 6.重要度分析 -----------------------------------------------
  
  
  # モデル構築
  # --- 重要度の出力を設定
  # --- importance = "impurity"
  rf_impurity <- ranger(
    formula = Sale_Price ~ ., 
    data = ames_train, 
    num.trees = 2000,
    mtry = 32,
    min.node.size = 1,
    sample.fraction = .80,
    replace = FALSE,
    importance = "impurity",
    respect.unordered.factors = "order",
    verbose = FALSE,
    seed  = 123
  )
  
  # モデル構築
  # --- 重要度の出力を設定
  # --- importance = "permutation"
  rf_permutation <- ranger(
    formula = Sale_Price ~ ., 
    data = ames_train, 
    num.trees = 2000,
    mtry = 32,
    min.node.size = 1,
    sample.fraction = .80,
    replace = FALSE,
    importance = "permutation",
    respect.unordered.factors = "order",
    verbose = FALSE,
    seed  = 123
  )
  
  
  # 変数重要度の取得  
  p1 <- rf_impurity %>% vip::vip(num_features = 25, bar = FALSE)
  p2 <- rf_permutation %>% vip::vip(num_features = 25, bar = FALSE)
  
  # プロット
  gridExtra::grid.arrange(p1, p2, nrow = 1)
  