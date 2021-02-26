# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 18 Generalized Low Rank Models
# Objective : TODO
# Created by: Owner
# Created on: 2020/10/23
# URL       : https://bradleyboehmke.github.io/HOML/GLRM.html
#           : https://koalaverse.github.io/homlr/notebooks/18-glrm.nb.html
#           : https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glrm.html
# **********************************************************************************


# ＜PCAの問題点＞
# - PCAの適用は数値データに限定される
# - PCAで構築されたPCは本質的に線形であるため、パフォーマンスが低下する可能性がある
#   --- 線形回帰が非線形関係をキャプチャする際に持つ欠陥によく似ている
#   --- 行列因数分解法(Regular-SVD / Max factorization)などで対応


# ＜ポイント＞
# - GLRMは、任意の数の欠測値を持つ数値/カテゴリ/順序/ブール型などの混合データを処理できる
#   --- PCAが数値データに限定される問題を解消
# - 次元削減にGLRMを使用することに焦点を当てているが、以下のような活用方法もある
#   --- クラスタリング / 欠測データの補完 / 計算メモリの削減 / 速度向上



# ＜目次＞
# 18.1 準備
# 18.2 アイデア
# 18.3 低ランクを見つける
# 18.4.H2OによるGLRM



# 18.1 準備 ---------------------------------------

# ＜ポイント＞
# - 次元削減ではデータ内の欠損値はすべて削除または補完してNAがない状態にしておく
# - データはすべて数値（カテゴリデータはダミー変換で数値化）
# - データはすべて基準化しておく（各アルゴリズムが実装済のことが多い）



library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(h2o)
library(Hmisc)


# カレントディレクトリ設定
setwd("I:/Project/R/tidymodels/book/ml_hans_on")


# データ取得
# --- 食料品店から2,000トランザクションで購入したアイテムと数量を識別
my_basket <- read_csv("data/my_basket.csv")


# データ確認
my_basket %>% print()
my_basket %>% glimpse()




# 18.2 アイデア ---------------------------------------

# ＜ポイント＞
# - 元のデータのすべての行と列に対して凝縮されたベクトル表現を生成することでデータセットの次元削減
#   --- GLRMはAを数値行列XおよびYに分解することで構成
# - 元の｢行列A｣を、｢行列X｣と｢行列Y｣に分解する
#   --- Xの原型は、より小さな次元の空間に投影された各観測値を表す
#   --- Yの原型は、より小さな次元の空間に投影された各特徴を表します。
# - 結果として得られる原型は、精神的にはPCAのPCと似ている
#   --- GLRMによって生成された原型よりも直線的かつ直交的に動作する場合はPCAと同じ縮小された特徴セットが生成される
#   --- ただし、それらが線形でない場合、GLRMは必ずしも直交していないアーキタイプを提供します


# 次元圧縮の事例
# --- 元のmtcarsは(32, 11)をk=3のランクに圧縮
# --- X(32, 3)とY(3, 11)の行列に分解する
mtcars %>% head()
mtcars %>% dim()



# ＜疑問点＞
# - GLRMはどのようにして原型値を生成しますか？
# - kに適切な値をどのように選択しますか？



# 18.3 低ランクを見つける ---------------------------------------

# 18.3.1 交互最小化 ****************

# ＜ポイント＞
# - 交互の最小化は、XとYの各特徴の損失関数を最小化することを単に交互に行います
#   --- 本質的に、ランダムな値が最初にXとYの原型値に設定されます
#   --- XとYそれぞれで勾配降下法により損失関数の改善が行われる
#   --- 勾配降下は停止条件に抵触するまで続けられる





# 18.4.H2OによるGLRM ----------------------------------------------

# H2Oの起動
h2o.no_progress()
h2o.init(max_mem_size = "5g")


# H2Oフレームに変換
my_basket.h2o <- my_basket %>% as.h2o()


# GLRM分析
# --- 2次損失関数を使用して基本的なGLRM分析を実行
# --- 正則化の大きさ(gamma_x、gamma_y)はチューニングする価値がある
# --- 特徴量に序数が含まれる場合は、multi_loss =“ Ordinal”の方が適切な場合がある
# --- 大規模データセットを使用している場合は、min_step_sizeを調整して学習プロセスをスピードアップできる
basic_glrm <-
  my_basket.h2o %>%
    h2o.glrm(k = 20,
             loss = "Quadratic",
             regularization_x = "None",
             regularization_y = "None",
             transform = "STANDARDIZE",
             max_iterations = 2000,
             seed = 123)


# サマリー
basic_glrm %>% summary()
basic_glrm %>% glimpse()


# プロット
# --- オブジェクトの圧縮速度
basic_glrm %>% plot()



# 主成分の重要度(分散の説明割合)
basic_glrm@model$importance



# プロット
# --- CVE：Cumulative Proportion（累積分散量）
# --- PVE：Proportion of Variance（個別分散量）
data.frame(
  PC  = basic_glrm@model$importance %>% seq_along(),
  PVE = basic_glrm@model$importance %>% .[2,] %>% unlist(),
  CVE = basic_glrm@model$importance %>% .[3,] %>% unlist()
  ) %>%
  gather(metric, variance_explained, -PC) %>%
  ggplot(aes(PC, variance_explained)) +
  geom_point() +
  facet_wrap(~ metric, ncol = 1, scales = "free")




# 各特徴量とアーキタイプの関係
basic_glrm@model$archetypes %>% t() %>% .[1:5, 1:5]


p1 <- 
  basic_glrm@model$archetypes %>% 
    t() %>%
    as.data.frame() %>%
    mutate(feature = row.names(.)) %>%
    ggplot(aes(Arch1, reorder(feature, Arch1))) +
    geom_point()

p2 <- 
  basic_glrm@model$archetypes %>% 
    t() %>%
    as.data.frame() %>%
    mutate(feature = row.names(.)) %>%
    ggplot(aes(Arch1, Arch2, label = feature)) +
    geom_text()

gridExtra::grid.arrange(p1, p2, nrow = 1)






# 4.GLRMの最適なkを探す ----------------------------------------------

# Re-run model with k = 8
k8_glrm <-
  my_basket.h2o %>%
    h2o.glrm(k = 8,
             loss = "Quadratic",
             regularization_x = "None",
             regularization_y = "None",
             transform = "STANDARDIZE",
             max_iterations = 2000,
             seed = 123)


# Reconstruct to see how well the model did
my_reconstruction <-
  k8_glrm %>%
    h2o.reconstruct(my_basket.h2o, reverse_transform = TRUE)

# Raw predicted values
my_reconstruction[1:5, 1:5]


# Round values to whole integers
my_reconstruction[1:5, 1:5] %>% round(0)


# Use non-negative regularization
k8_glrm_regularized <-
  my_basket.h2o %>%
    h2o.glrm(k = 8,
             loss = "Quadratic",
             regularization_x = "NonNegative",
             regularization_y = "NonNegative",
             gamma_x = 0.5,
             gamma_y = 0.5,
             transform = "STANDARDIZE",
             max_iterations = 2000,
             seed = 123
             )

# Show predicted values
k8_glrm_regularized %>% predict(my_basket.h2o)[1:5, 1:5]


# Compare regularized versus non-regularized loss
par(mfrow = c(1, 2))
plot(k8_glrm)
plot(k8_glrm_regularized)



# Split data into train & validation
split <- h2o.splitFrame(my_basket.h2o, ratios = 0.75, seed = 123)
train <- split[[1]]
valid <- split[[2]]

# Create hyperparameter search grid
params <- expand.grid(
  regularization_x = c("None", "NonNegative", "L1"),
  regularization_y = c("None", "NonNegative", "L1"),
  gamma_x = seq(0, 1, by = .25),
  gamma_y = seq(0, 1, by = .25),
  error = 0,
  stringsAsFactors = FALSE
)

# Perform grid search
for(i in seq_len(nrow(params))) {

  # Create model
  glrm_model <- h2o.glrm(
    training_frame = train,
    k = 8,
    loss = "Quadratic",
    regularization_x = params$regularization_x[i],
    regularization_y = params$regularization_y[i],
    gamma_x = params$gamma_x[i],
    gamma_y = params$gamma_y[i],
    transform = "STANDARDIZE",
    max_runtime_secs = 1000,
    seed = 123
  )

  # Predict on validation set and extract error
  validate <- h2o.performance(glrm_model, valid)
  params$error[i] <- validate@metrics$numerr
}

# Look at the top 10 models with the lowest error rate
params %>%
  arrange(error) %>%
  head(10)


# Apply final model with optimal hyperparamters
final_glrm_model <- h2o.glrm(
  training_frame = my_basket.h2o,
  k = 8,
  loss = "Quadratic",
  regularization_x = "L1",
  regularization_y = "NonNegative",
  gamma_x = 1,
  gamma_y = 0.25,
  transform = "STANDARDIZE",
  max_iterations = 2000,
  seed = 123
)

# New observations to score
new_observations <- as.h2o(sample_n(my_basket, 2))

# Basic scoring
predict(final_glrm_model, new_observations) %>% round(0)


