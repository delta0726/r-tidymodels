# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 9 Decision Trees
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/2
# URL       : https://bradleyboehmke.github.io/HOML/DT.html
#           : https://koalaverse.github.io/homlr/notebooks/09-decision-trees.nb.html
#           : https://www1.doshisha.ac.jp/~mjin/R/Chap_19/19.html
# **********************************************************************************



# ＜目次＞
# 9.1 準備
# 9.2 決定木の構造
# 9.3 パーティショニング
# 9.4 どのくらいの深さが適切か
# 9.5 事例：Ames housing
# 9.6 特徴量の解釈
# 9.7 まとめ




# 9.1 準備 ----------------------------------------------

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



# 9.2 決定木の構造 ----------------------------------------------

# ＜ポイント＞
# - ツリーモデルは分割ルールを使用して類似の応答値を持つ多数の小さな領域に分割する(重複なしの分割)
#   --- 最も一般的なのはCARTアルゴリズム（特徴量を水準でYes/Noに分割していく）
#   --- 再帰的にパーティショニングを行う
#   --- 回帰問題の場合は総SSE、分類問題の場合はクロスエントロピーを目的関数とする


# - 決定木はパラメトリックモデルのような前処理(単位統一/正規化)などを必要としないアルゴリズム
#   --- 最適な分割ポイントの位置のみをシフトすることでパーティショニング
#   --- 外れ値に対しても頑健（パーティショニングだけしかしないため）
#   --- 欠損値を代入する必要はない

# - モデルの予測力は高くない
#   --- max_depthの大きいモデルはバリアンスが高くなる傾向にある（アーリーストッピングが必要）
#   --- ランダムフォレスト等のアンサンブル学習モデルのほうが強力でよく使われる




# 9.3 パーティショニング -----------------------------------------------

# ＜ポイント＞
# - 特徴量を1つに限定することで決定木の可視化を行う
# - 決定木は同じ特徴量を何回も使う可能性があるので停止基準が必要（3層のケース）
#   --- 特徴量の少しの違いで結果が異なる可能性があることが分かる（バリアンスが高まる）


# データ作成
# --- truthの値に対してrnom()で揺らぎを与えたものをyとする
# --- モデルはyからtruthを導き出すことができるか？
set.seed(1112)
df <- tibble::tibble(
  x = seq(from = 0, to = 2 * pi, length = 500),
  y = sin(x) + rnorm(length(x), sd = 0.5),
  truth = sin(x)
)


# データ確認
df %>% as_tibble()
df %>% nrow()



# 9.3.1 1階層のケース -------------------------

# モデル構築
# --- maxdepth = 1
fit <- rpart(y ~ x, data = df, control = list(cp = 0, minbucket = 5, maxdepth = 1))
fit %>% print()


# 予測値の取得
df_pred <- df %>% mutate(pred = predict(fit, df))


# ツリープロット
fit %>% rpart.plot()


# 散布図
# --- 青：truth（真の値）
# --- 赤：pred（決定木の予測値）
df_pred %>%
  ggplot(aes(x = x, y = y)) +
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




# 9.3.2 3階層のケース -------------------------

# モデル構築
# --- maxdepth = 3
fit <- rpart(y ~ x, data = df, control = list(cp = 0, minbucket = 5, maxdepth = 3))
fit %>% print()


# 予測値の取得
df_pred_3 <- df %>% mutate(pred = predict(fit, df))


# ツリープロット
fit %>% rpart.plot()


# 散布図
# --- 青：truth（真の値）
# --- 赤：pred（決定木の予測値）
df_pred_3 %>%
  mutate(pred = predict(fit, df)) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .2, size = 1) +
  geom_line(aes(x, y = truth), color = "blue", size = .75) +
  geom_line(aes(y = pred), color = "red", size = .75)



# 9.3.3.irisの事例 -----------------------------------------------------


# モデル構築
iris_fit <- rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)
iris_fit %>% print()


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




# 9.4 どのくらいの深さが適切か ---------------------------------

# ＜ポイント＞
# - ツリーの階層を深くすると、訓練データに対するオーバーフィッティングが生じる（自明）
# - 複雑さを制御する方法として以下の2つの方法がある
#   --- 1. アーリーストッピング(max_depth)
#   --- 2. プルーニング(cp)



# データ作成
# --- truthの値に対してrnom()で揺らぎを与えたものをyとする
# --- モデルはyからtruthを導き出すことができるか？
set.seed(1112)
df <- tibble::tibble(
  x = seq(from = 0, to = 2 * pi, length = 500),
  y = sin(x) + rnorm(length(x), sd = 0.5),
  truth = sin(x)
)


# 9.4.0 複雑なモデルの事例 ---------------------------------

# データ確認
df %>% as_tibble()
df %>% nrow()


# モデル構築
# --- maxdepth = 50
fit <- rpart(y ~ x, data = df, control = list(cp = 0, minbucket = 1, maxdepth = 50))
fit %>% print()


# 予測値の取得
df_pred_50 <- df %>% mutate(pred = predict(fit, df))


# ツリープロット
fit %>% rpart.plot()


# プロット
# --- 見るからに複雑すぎる
df_pred_50 %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .2, size = 1) +
  geom_line(aes(x, y = truth), color = "blue", size = 0.75) +
  geom_line(aes(y = pred), color = "red", size = 0.75)




# 9.4.1 アーリーストッピング ---------------------------------

# ＜ポイント＞
# - ｢tree_depth｣を制限することでアーリーストッピングを適用することができる
#   --- 設定しないと1つになるまで分割を繰り返す

# - ｢tree_depth｣はバリアンス、｢minbucket｣はバイアスをコントロールする
#   --- minbucketは{parsnip}では｢min_n｣と表現されている


# チューニング・グリッド
# --- maxdepth  : 最大の深さ
# --- minbucket : 使用する特徴量の数
hyper_grid <- expand.grid(
  maxdepth = c(1, 5, 15),
  minbucket = c(1, 5, 15)
)


# 確認
hyper_grid %>% print()


# シミュレーション準備
# --- データフレームの初期化
results <- data.frame(NULL)


# シミュレーション
# --- チューニング・パラメータごとの予測値を取得
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


# 確認
results %>% print()


# プロット
# --- maxdepth ：大きいほどバリアンスが大きくなる（小さいほどバリアンスが小さくなる）
# --- minbucket：大きいほどバイアスが小さくなる（小さいほどバイアスが大きくなる=学習不足）
results %>%
  ggplot(aes(x = x, y = y)) +
    geom_point(alpha = .2, size = 1) +
    geom_line(aes(x, y = truth), color = "blue", size = .75) +
    geom_line(aes(y = pred), color = "red", size = 1) +
    facet_grid(minbucket ~ maxdepth)



# 9.4.2 プルーニング ---------------------------------

# ＜ポイント＞
# - 深さを明示的に指定する代わりに複雑なツリーを枝刈りして最適なサブツリーを見つけることもできる
#   --- ｢cp(複雑度)｣でコントロールする
#   --- cpに対してペナルティ付きエラーが最も低い最小の剪定木を見つける
#   --- cpが大きいとが大きいと、木がはるかに小さくなる


# 複雑な決定木 -----

# モデル構築
# --- maxdepth = 50
# --- cp = 0
fit <- rpart(y ~ x, data = df, control = list(cp = 0, minbucket = 1, maxdepth = 50))
fit %>% print()


# 予測値の取得
df_pred_50 <- df %>% mutate(pred = predict(fit, df))


# ツリープロット
fit %>% rpart.plot()


# 散布図
# --- 境界線のイメージ
p1 <-
  df_pred_50 %>%
    ggplot(aes(x, y)) +
    geom_point(alpha = .3, size = 2) +
    geom_line(aes(x, y = truth), color = "blue", size = 1) +
    geom_line(aes(y = pred), color = "red", size = 1)



# プルーニングした決定木 -----

# モデル構築
# --- maxdepth = 50
# --- cp = 0.01
fit2 <- rpart(y ~ x, data = df, control = list(cp = 0.01, minbucket = 1, maxdepth = 50))
fit2 %>% print()


# 予測値の取得
df_pred_cp <- df %>% mutate(pred2 = predict(fit2, df))


# ツリープロット
fit2 %>% rpart.plot()


# 散布図
# --- 境界線のイメージ
p2 <-
  df_pred_cp %>%
    ggplot(aes(x, y)) +
    geom_point(alpha = .3, size = 2) +
    geom_line(aes(x, y = truth), color = "blue", size = 1) +
    geom_line(aes(y = pred2), color = "red", size = 1)


gridExtra::grid.arrange(p1, p2, nrow = 1)




# 9.5 事例：Ames housing ----------------------------------------------

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


# データ確認
ames_train %>% dim()
ames_test %>% dim()


# モデル用データ
X <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
Y <- ames_train$Sale_Price %>% log()



# 2.モデル1 ----------------------------------------------

# モデル構築
ames_dt1 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova"
)


# 確認
ames_dt1 %>% print()


# ツリープロット
ames_dt1 %>% rpart.plot()


# 複雑度(cp)のプロット
ames_dt1 %>% plotcp()



# 3.モデル2 ----------------------------------------------

# モデル構築
ames_dt2 <- rpart(
  formula = Sale_Price ~ .,
  data    = ames_train,
  method  = "anova", 
  control = list(cp = 0, xval = 10)
)


# ツリープロット
ames_dt2 %>% rpart.plot()


# 複雑度(cp)のプロット
ames_dt2 %>% plotcp()
abline(v = 11, lty = "dashed")


# rpart cross validation results
ames_dt1$cptable




# 4.モデル3 ----------------------------------------------


# クロスバリデーション
ames_dt3 <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)


ames_dt3 %>% ggplot()



# 9.6 特徴量の解釈 ----------------------------------------------

# ＜ポイント＞
# - 損失関数(SSE)の減少をもとに変数重要度を定義する
#   --- 全ての特徴量の損失関数の合計削減量を基準に相対的な変数重要度を確認することができる

# - PDP(Partial Dependence Plot)からも有効なインプリケーションが得られる
#   --- {DALEX}{IML}などが提供


# 変数重要度
ames_dt3 %>% vip(num_features = 40, bar = FALSE)


# 部分依存プロット
# --- 特定の説明変数と目的変数の関係性を示している
p1 <- ames_dt3 %>% pdp::partial(pred.var = "Gr_Liv_Area") %>% autoplot()
p2 <- ames_dt3 %>% pdp::partial(pred.var = "Year_Built") %>% autoplot()
gridExtra::grid.arrange(p1, p2, ncol = 3)

