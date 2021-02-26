# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 8 K-Nearest Neighbors
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/27
# URL       : https://bradleyboehmke.github.io/HOML/knn.html
#           : https://koalaverse.github.io/homlr/notebooks/08-knn.nb.html
# **********************************************************************************


# ＜ポイント＞
# - k近傍法は各観測値の予測を、他の予測値の｢類似性｣に基づいて決定している
# - k近傍法のアルゴリズムは｢遅延学習｣としてカテゴリされる
#   --- 大規模データセットにおいては計算コストた急激に高まる
#   --- {FNN}などk近傍法の高速化を目的としたライブラリも存在する


# ＜遅延学習＞
# - 遅延学習とは、教師データを丸暗記するタイプのアルゴリズムを指す
#   --- 通常学習: 新規データを分類するためのパラメータを算出する
#   --- 遅延学習: 評価の際に参照するための教師データを取り込むのみ
# - モデル訓練はアルゴリズムを作り出すのではないため速い
# - モデル評価は実際に近傍計算が入るため時間がかかる


# ＜活用チャネル＞
# - 協調フィルタリングのアルゴリズムとして使用されることが多い
# - 前処理におけるでも利用されることが多い
#  --- 欠損値補完（knn Inpute）
#  --- サンプル不均衡の調整(SMOTEなど)


# ＜参考＞
# K近傍法の特徴について調べてみた
# https://qiita.com/Tokky0425/items/d28021eb1c2a710ec9f9


# ＜目次＞
# 1 準備
# 2 類似性の測定
# 3 kを選択
# 4 MNISTの例



# 1 準備 -----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(modeldata)
library(magrittr)
library(caret)
library(FNN)
library(ggmap)
library(rsample)


# 準備：attritionデータ ****************************************************

# データロード
data(attrition)

# データ確認
attrition %>% as_tibble()
attrition %>% glimpse()

# データ加工
attrit <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

# データ分割
set.seed(123)
churn_split <- initial_split(attrit, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)


# 準備：amesデータ ********************************************************

# データロード
ames <- AmesHousing::make_ames()

# データ概要
ames %>% as_tibble()
ames %>% glimpse()

# データ分割
set.seed(123)
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# 準備：MNISTデータ ********************************************************

# データロード
mnist <- dslabs::read_mnist()

# データ概要
mnist %>% glimpse()



# 2 類似性の測定 -------------------------------------------------------------------------

# 地図で見る距離尺度 ********************************************************

# ＜概要＞
# - Amesデータを地図に落とし込んで距離を確認


# 前処理
df <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_nzv(all_nominal()) %>%
    step_integer(matches("Qual|Cond|QC|Qu")) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes()) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    prep(training = ames_train, retain = TRUE) %>%
    juice() %>%
    select(-Sale_Price)

# データ確認
df %>% as_tibble()
df %>% glimpse()

# knnの実行
home <- 30
k <- 10
index <- df[-home, ] %>% knnx.index(df[home, ], k = k) %>% as.vector()
knn_homes <- ames_train[c(home, index), ]

# プロット作成
# --- マップデータ（Figure8.1）
knn_homes %>%
  select(Longitude, Latitude) %>%
  mutate(desc = factor(c('House of interest', rep('Closest neighbors', k)),
                       levels = c('House of interest', 'Closest neighbors'))) %>%
  qmplot(Longitude, Latitude, data = .,
         maptype = "toner-background", darken = .7, color = desc, size = I(2.5)) +
  theme(legend.position = "top",
        legend.title = element_blank())



# 距離尺度 ********************************************************

# ＜ポイント＞
# - ｢ユーグリッド距離｣や｢マンハッタン距離｣が最もよく使われる

# データ抽出
two_houses <-
  ames_train %>%
    select(Gr_Liv_Area, Year_Built) %>%
    slice(1:2)

# 距離行列
# --- ユーグリッド距離
two_houses %>% dist(method = "euclidean")

# 距離行列
# --- マンハッタン距離
two_houses %>% dist(method = "manhattan")

# プロット作成
# --- ユーグリッド距離のイメージ
p1 <-
  two_houses %>%
    ggplot(aes(Gr_Liv_Area, Year_Built)) +
    geom_point() +
    geom_line(lty = "dashed") +
    ggtitle("(A) Euclidean distance")

# プロット作成
# --- マンハッタン距離のイメージ
p2 <-
  two_houses %>%
    ggplot(aes(Gr_Liv_Area, Year_Built)) +
    geom_point() +
    geom_step(lty = "dashed") +
    ggtitle("(B) Manhattan distance")

# プロット比較
gridExtra::grid.arrange(p1, p2, nrow = 1)


# スケーリングの重要性 ********************************************************

# ＜ポイント＞
# - 距離尺度はスケーリングや異常値処理が必須
# - 単位の異なる指標を評価する際には、適切なスケーリングを行っているかを確認
# - カテゴリカル変数の場合はOne-Hot Encodingが必要となる
#   --- k近傍法は数値データしか扱えない


# 関数定義
# --- 共通のデータ抽出
extract_data_1 <- function(df, home, Bedroom_AbvGr, Year_Built){
  result <-
    df %>%
      mutate(id = row_number()) %>%
      select(Bedroom_AbvGr, Year_Built, id) %>%
      filter(Bedroom_AbvGr == !!Bedroom_AbvGr & Year_Built == !!Year_Built) %>%
      slice(1) %>%
      mutate(home = home) %>%
      select(home, everything())

  return(result)
}

# 関数定義
# --- 共通のデータ抽出
extract_data_2 <- function(df, id, home){
  result <-
    df %>%
      mutate(id = row_number()) %>%
      filter(id == !!id) %>%
      select(Bedroom_AbvGr, Year_Built, id) %>%
      mutate(home = home) %>%
      select(home, everything())

  return(result)
}

# データ抽出
home1 <- ames %>% extract_data_1("home1", 4, 2008)
home2 <- ames %>% extract_data_1("home2", 2, 2008)
home3 <- ames %>% extract_data_1("home3", 3, 1998)

# 対象フィールド
# --- "Bedroom_AbvGr"と"Year_Built"の距離を測定
# --- 数値水準が全く異なる点に注意
features <- c("Bedroom_AbvGr", "Year_Built")
home1[, features]

# 距離行列
# --- 基準化なし
home1[, features] %>% rbind(home2[, features]) %>% dist()
home1[, features] %>% rbind(home3[, features]) %>% dist()


# 前処理適用
# --- Zスコアでスケーリング
scaled_ames <-
  recipe(Sale_Price ~ ., ames_train) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(training = ames, retain = TRUE) %>%
    juice()

# データ抽出
home1_std <- scaled_ames %>% extract_data_2(home1$id, "home1")
home2_std <- scaled_ames %>% extract_data_2(home2$id, "home2")
home3_std <- scaled_ames %>% extract_data_2(home3$id, "home3")

# 距離行列
# --- 基準化あり
home1_std[, features] %>% rbind(home2_std[, features]) %>% dist()
home1_std[, features] %>% rbind(home3_std[, features]) %>% dist()



# 3 kの選択 --------------------------------------------------

# ＜ポイント＞
# - k近傍法はkの選択に対して非常に敏感となる
#   --- kは距離に応じたサンプルの数を示す（k=1なら一番近いサンプル）
#   --- kの値が小さいと通常はオーバーフィットとなり、大きいと学習不足となる
# - 最適なkに対するセオリーはなく、データセットの特性に応じて決まる
#   --- データセットにおける列選択(特徴量選択)が肝要となる


# 前処理
# --- カテゴリカル変数の数値変換
# --- スケーリング
blueprint <-
  recipe(Attrition ~ ., data = churn_train) %>%
    step_nzv(all_nominal()) %>%
    step_integer(contains("Satisfaction")) %>%
    step_integer(WorkLifeBalance) %>%
    step_integer(JobInvolvement) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes())

# クロスバリデーション設定
# --- {caret}を使用
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# グリッド作成
# --- kの値で20パターンを作成
hyper_grid <- expand.grid(
  k = seq(1, nrow(churn_train) / 3, length.out = 20) %>% floor()
)

# 学習
# --- クロスバリデーションあり
knn_grid <-
  blueprint %>%
    train(data = churn_train,
          method = "knn",
          trControl = cv,
          tuneGrid = hyper_grid,
          metric = "ROC")

# プロット作成
# --- kとROCの関係
knn_grid %>% ggplot()



# 4 MNISTの例 --------------------------------------------------

# データ抽出
set.seed(123)
index <- mnist$train$images %>% nrow() %>% sample(size = 10000)
mnist_x <- mnist$train$images[index, ]
mnist_y <- factor(mnist$train$labels[index])

# データ確認
mnist_x %>% dim()
mnist_y %>% length()

# プロット作成
# --- 列ごとに標準偏差を算出
# --- 標準偏差の分布を見ることで情報量の分布を確認
mnist_x %>%
  as.data.frame() %>%
  map_df(sd) %>%
  gather(feature, sd) %>%
  ggplot(aes(sd)) +
  geom_histogram(binwidth = 1)


# 疎データの確認
# --- 分散ゼロ付近の列番号
nzv <- mnist_x %>% nearZeroVar()

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
nzv <- mnist_x %>% nearZeroVar()
index <- setdiff(1:ncol(mnist_x), nzv)
mnist_x <- mnist_x[, index]


# クロスバリデーション設定
# --- {caret}を使用
cv <- trainControl(
  method = "LGOCV",
  p = 0.7,
  number = 1,
  savePredictions = TRUE
)

# グリッド作成
# --- kの値で20パターンを作成
hyper_grid <- seq(3, 25, by = 2) %>% expand.grid()


# 学習
# --- クロスバリデーションあり
knn_mnist <- train(
  mnist_x,
  mnist_y,
  method = "knn",
  tuneGrid = hyper_grid,
  preProc = c("center", "scale"),
  trControl = cv
)

# プロット作成
# --- kとROCの関係
knn_mnist %>% ggplot()

# 混合行列の作成
cm <- confusionMatrix(knn_mnist$pred$pred, knn_mnist$pred$obs)
cm$byClass[, c(1:2, 11)]  # sensitivity, specificity, & accuracy

# 変数重要度
vi <- knn_mnist %>% varImp()
vi


# 変数重要度の中央値
imp <-
  vi$importance %>%
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




