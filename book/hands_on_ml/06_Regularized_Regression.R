# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 6 Regularized Regression
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/12
# URL       : https://bradleyboehmke.github.io/HOML/regularized-regression.html
#           : https://koalaverse.github.io/homlr/notebooks/06-regularized-regression.nb.html
# **********************************************************************************



# ＜ポイント＞
# - 線形モデルは仮定を全て満した状態における推定された係数はバリアンスが最も低くなっている
#   --- 一般的に特徴量が増えると、仮定が崩れて訓練データに対するオーバーフィットが生じる
#   --- サンプル外に対する予測力が維持できない
# - 正規化回帰は、多くの特徴量を持つ大規模データセットに対して従来のGLMに比べて多くの大きな利点を提供する
#   --- n > p 問題が発生しない
#   --- 多重共線性の影響を最小限に抑える
#   --- 自動化された特徴量選択を行う
#   --- ハイパーパラメータが比較的少ない


# ＜注意点＞
# - 欠損値を自動的に処理することはできない (前処理が必要)
# - 外れ値に対する頑健性はない (ロバスト回帰のような処理はない)
# - 線形関係を想定している (相互効果を含めることは可能)



# ＜目次＞
# 6.1 準備
# 6.2 正則化をする理由
# 6.3 実装
# 6.4 チューニング
# 6.5 特徴量の解釈
# 6.6 Attrition Data



# 6.1 準備 ----------------------------------------------


library(tidyverse)
library(tidymodels)
library(magrittr)
library(modeldata)
library(glmnet)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)


# データロード
ames <- AmesHousing::make_ames()


# データ分割
set.seed(123)
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# データ確認
ames_train %>% dim()
ames_test %>% dim()




# 6.2 正則化をする理由 ----------------------------------------------


# ＜ポイント＞
# - OLS回帰の目的は実績値と予測値の平方誤差(SSE)の合計を最小化する超平面を見つけること
#   --- 誤差の正規性、等分散性、多重共線性、n>pなどの前提がある
#   --- テキスト等の機械学習ではpが非常に多くなるケースもある（違反の可能性）
# - ステップワイズによる特徴量選択
#   --- 計算効率が悪い
#   --- 必ずしも最適な特徴量を抽出しない
# - 正則化回帰による特徴量選択
#   --- 無関係な機能の効果をゆっくりとゼロに押し上げ、場合によっては、係数全体をゼロにします
#   --- すべての係数推定の合計サイズを制約する
#   --- この制約は、係数の大きさと変動を減らすのに役立ち、モデルの分散を減らす



# ＜正則化回帰＞
# - minimize(SSE+P)
# - このペナルティパラメータは係数のサイズを制約する
# - 係数を増加できる唯一の方法は、SSEの比較可能な減少が得られる係数のみに限られる



# ＜parsnipにおける扱い＞
# - linear_reg()とlogistic_reg()で正則化回帰を行うことができる
# - ハイパーパラメータは以下の２種類
#   --- penalty : {glmnet}のlambda
#   --- mixture : {glmnet}のalpha（0:リッジ、1：ラッソ、0-1：Elastic Net）



# 6.2.0 線形回帰の限界 --------------------------------------------------


# データサンプリング
# --- Gr_Liv_Areaの水準で抽出
# --- サンプル数を50％絞り込む
ames_sub <-
  ames_train %>%
    filter(between(Gr_Liv_Area, 1000, 3000)) %>%
    sample_frac(0.5)


# 線形回帰
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_sub)


# 予測値等の取得
df_augument <- model1 %>% broom::augment()
df_augument %>% print()


# 散布図
# --- 回帰の前提である残差の｢正規性｣｢等分散性｣が保持されていない
# --- 特徴量が増えてくると仮定に合わない特徴量も増えてくる
df_augument %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) +
  geom_segment(aes(x = Gr_Liv_Area, y = Sale_Price, xend = Gr_Liv_Area, yend = .fitted),
               alpha = 0.3) +
  geom_point(size = 1, color = "red") +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar)




# 6.2.1 リッジ・ペナルティ --------------------------------------------------

# ＜ポイント＞
# - L2ノルムによりペナルティのサイズが決定されている
#   --- lambda=0の場合、目的関数はSSEを最小化する通常のOLS回帰目的関数に等しくなる
#   --- labmdaが大きくなるにつれて制約が強くなる
# - 変数選択は実施されず、最終モデルはすべての回帰係数を保持される
#   --- 重要度の低い係数にはペナルティがかかり、ノイズは減らされる
#   --- すべての変数を使いたい場合に適している


# データ準備
boston_df <- pdp::boston
boston_df %>% glimpse()


# モデル用データ
boston_train_x <- model.matrix(cmedv ~ ., boston_df)[, -1]
boston_train_y <- boston_df$cmedv


# リッジ回帰
# --- alpha=0（リッジ回帰）
# --- モデル用データからモデルを定義（従来の回帰モデルの記法がそのまま使えない）
boston_ridge <-
  glmnet(x = boston_train_x,
         y = boston_train_y,
         alpha = 0)


# リッジ回帰
# --- {glmnetUtils}の記法
# --- 従来の回帰モデルの記法で記述できる
boston_ridge_2 <-
  glmnetUtils::glmnet(cmedv ~ . - lon, data = boston_df, alpha = 0)


# アウトプット
# --- print()で表示されるデータを再現
# --- lambdaの水準ごとの結果を示してくれている（最終的にハイパーパラメータのlambdaを一意に決める）
tibble(Df = boston_ridge$df,
       Dev = boston_ridge$dev.ratio,
       Lambda = boston_ridge$lambda)


# アウトプット(検証用)
# --- {glmnetUtils}の結果
tibble(Df = boston_ridge_2$df,
       Dev = boston_ridge_2$dev.ratio,
       Lambda = boston_ridge_2$lambda)


# 格納データ
boston_ridge %>% glimpse()


# ラムダとペナルティ
lam <-
  boston_ridge$lambda %>%
    as.data.frame() %>%
    mutate(penalty = boston_ridge$a0 %>% names()) %>%
    set_colnames(c("lambda", "penalty")) %>%
    as_tibble()


# プロット用データ
results <-
  boston_ridge$beta %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    gather(penalty, coefficients, -rowname) %>%
    left_join(lam) %>%
    as_tibble()


# プロット用ラベル
result_labels <-
  results %>%
  group_by(rowname) %>%
  filter(lambda == min(lambda)) %>%
  ungroup() %>%
  top_n(5, wt = abs(coefficients)) %>%
  mutate(var = paste0("x", 1:5))


# データ確認
lam %>% print()
results %>% print()
result_labels %>% print()


# プロット
ggplot() +
  geom_line(data = results,
            aes(lambda, coefficients, group = rowname, color = rowname),
            show.legend = FALSE) +
  scale_x_log10() +
  geom_text(data = result_labels,
            aes(lambda, coefficients, label = var, color = rowname),
            nudge_x = -.06, show.legend = FALSE)


# プロット
# --- お手軽
boston_ridge %>% autoplot(xvar = c("lambda"))




# 6.2.2 ラッソ・ペナルティ --------------------------------------------------

# ＜ポイント＞
# - L2ノルムによりペナルティのサイズが決定されている
#   --- lambda=0の場合、目的関数はSSEを最小化する通常のOLS回帰目的関数に等しくなる
#   --- labmdaが大きくなるにつれて制約が強くなる
# - 変数選択は実施されず、最終モデルはすべての回帰係数を保持される
#   --- 重要度の低い係数にはペナルティがかかり、ノイズは減らされる
#   --- すべての変数を使いたい場合に適している

#・重要度の低い変数の係数をゼロとする変数選択が行われる
#・モデル検証やメリハリのあるモデル構築に有用


# ラッソ回帰
# --- alpha=1（ラッソ回帰）
boston_lasso <- glmnet(x = boston_train_x,
                       y = boston_train_y,
                       alpha = 1)


# ラッソ回帰
# --- {glmnetUtils}の記法
# --- 従来の回帰モデルの記法で記述できる
boston_lasso_2 <-
  glmnetUtils::glmnet(cmedv ~ . - lon, data = boston_df, alpha = 1)



# アウトプット
# --- print()で表示されるデータを再現
# --- lambdaの水準ごとの結果を示してくれている（最終的にハイパーパラメータのlambdaを一意に決める）
tibble(Df = boston_lasso$df,
       Dev = boston_lasso$dev.ratio,
       Lambda = boston_lasso$lambda)


# アウトプット(検証用)
# --- {glmnetUtils}の結果
tibble(Df = boston_lasso_2$df,
       Dev = boston_lasso_2$dev.ratio,
       Lambda = boston_lasso_2$lambda)


# 格納データ
boston_ridge %>% glimpse()


# ラムダとペナルティ
lam <-
  boston_lasso$lambda %>%
  as.data.frame() %>%
  mutate(penalty = boston_lasso$a0 %>% names()) %>%
  set_colnames(c("lambda", "penalty")) %>%
  as_tibble()


# プロット用データ
results <-
  boston_lasso$beta %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  gather(penalty, coefficients, -rowname) %>%
  left_join(lam) %>%
  as_tibble()


# プロット用ラベル
result_labels <-
  results %>%
  group_by(rowname) %>%
  filter(lambda == min(lambda)) %>%
  ungroup() %>%
  top_n(5, wt = abs(coefficients)) %>%
  mutate(var = paste0("x", 1:5))


# プロット
ggplot() +
  geom_line(data = results,
            aes(lambda, coefficients, group = rowname, color = rowname),
            show.legend = FALSE) +
  scale_x_log10() +
  geom_text(data = result_labels,
            aes(lambda, coefficients, label = var, color = rowname),
            nudge_x = -.06, show.legend = FALSE)


# プロット
# --- お手軽
boston_lasso %>% autoplot(xvar = c("lambda"))




## Elasti Net(弾性ネット) ----

#・LASSOとRIDGEを組み合わせたもの
#・リッジ回帰のメリハリをつけて強化するという側面を持つ


# 弾性ネット回帰
boston_elastic <- glmnet(x = boston_train_x,
                         y = boston_train_y,
                         alpha = 0.2)


# 弾性ネット回帰
# --- {glmnetUtils}の記法
# --- 従来の回帰モデルの記法で記述できる
boston_elastic_2 <-
  glmnetUtils::glmnet(cmedv ~ . - lon, data = boston_df, alpha = 0.2)


# アウトプット
# --- print()で表示されるデータを再現
# --- lambdaの水準ごとの結果を示してくれている（最終的にハイパーパラメータのlambdaを一意に決める）
tibble(Df = boston_elastic$df,
       Dev = boston_elastic$dev.ratio,
       Lambda = boston_elastic$lambda)


# アウトプット(検証用)
# --- {glmnetUtils}の結果
tibble(Df = boston_elastic_2$df,
       Dev = boston_elastic_2$dev.ratio,
       Lambda = boston_elastic_2$lambda)


# ラムダとペナルティ
lam <-
  boston_elastic$lambda %>%
  as.data.frame() %>%
  mutate(penalty = boston_elastic$a0 %>% names()) %>%
  set_colnames(c("lambda", "penalty")) %>%
  as_tibble()


# プロット用データ
results <-
  boston_elastic$beta %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  gather(penalty, coefficients, -rowname) %>%
  left_join(lam) %>%
  as_tibble()


# プロット用ラベル
result_labels <-
  results %>%
  group_by(rowname) %>%
  filter(lambda == min(lambda)) %>%
  ungroup() %>%
  top_n(5, wt = abs(coefficients)) %>%
  mutate(var = paste0("x", 1:5))


# プロット
ggplot() +
  geom_line(data = results,
            aes(lambda, coefficients, group = rowname, color = rowname),
            show.legend = FALSE) +
  scale_x_log10() +
  geom_text(data = result_labels,
            aes(lambda, coefficients, label = var, color = rowname),
            nudge_x = -.06, show.legend = FALSE)


# プロット
# --- お手軽
boston_elastic %>% autoplot(xvar = c("lambda"))




# 6.3 実装 ----------------------------------------------

# ＜ポイント＞
# - 正則化回帰は{glmnet}に加えて{h2o}などでも実装されている
#   --- {glmnet}は非常に高速
# - glmnet()はデフォルトでは100個のlambdaを適用してシミュレーションを提供する
#   --- lambdaの値を一意に定めて入力することもできる
#   --- クロスバリデーションを行っているわけではない


# データ準備
X <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
Y <- log(ames_train$Sale_Price)


# リッジ回帰
ridge <- glmnet(x = X, y = Y, alpha = 0)


# プロット
ridge %>% plot(xvar = "lambda")


# ラムダの確認
ridge$lambda %>% head()
ridge$lambda %>% plot_histogram()


# 回帰係数
# --- lambdaが小さい場合
# --- ペナルティが小さいので回帰係数は大きい
ridge %>% coef() %>% .[c("Latitude", "Overall_QualVery_Excellent"), 100]


# 回帰係数
# --- lambdaが大きい場合
# --- ペナルティが大きいので回帰係数は小さい
ridge %>% coef() %>% .[c("Latitude", "Overall_QualVery_Excellent"), 1]



# 6.4 チューニング ----------------------------------------------

# 6.4.1 lambdaのチューニング ---------------------------------------

# ＜ポイント＞
# - ハイパーパラメータであるlambdaをクロスバリデーションによるチューニングを行う
# - Lassoは64個の特徴量でRidgeの294個とほぼ同じ推定精度
#   --- 特徴量の取捨選択ができている


# データ準備
X <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
Y <- log(ames_train$Sale_Price)


# クロスバリデーション
# --- 10fold
# --- デフォルトではMSEを損失関数として用いる
# --- 損失関数が最小＆低SEとなるlambdaを出力する
ridge <- cv.glmnet(x = X, y = Y, alpha = 0)
lasso <- cv.glmnet(x = X, y = Y, alpha = 1)


# 確認
ridge %>% print()
lasso %>% print()


# プロット
# - どちらのモデルでもペナルティ(lambda)が大きくなるにつれてMSEが上昇してしまう
#   --- 訓練データへのオーバーフィットを示唆
# - 赤い点線は交差検証の平均値、レンジは交差検証の標準誤差を示す
# --- 上部の数値はモデル内の特徴量の数を示している
# --- 縦の破線はlambdaが最小(or2番目に最小)となる値を示す
par(mfrow = c(1, 2))
ridge %>% plot(main = "Ridge penalty\n\n")
lasso %>% plot(main = "Lasso penalty\n\n")


# MSEの最小値
ridge$cvm %>% min()
lasso$cvm %>% min()


# MSEの最小値
ridge$lambda.min
lasso$lambda.min


# 1-SE rule
ridge$cvm[ridge$lambda == ridge$lambda.1se]
lasso$cvm[lasso$lambda == lasso$lambda.1se]


# プロット
boston_elastic %>% autoplot(xvar = c("lambda"))




# 6.4.2 alphaの水準による変化 ---------------------------------------

# ＜ポイント＞
# - alpha(mixture)もハイパーパラメータなのでチューニングの余地がある
#   --- 定性的に決めてもよい


# モデル
lasso    <- glmnet(X, Y, alpha = 1.0)
elastic1 <- glmnet(X, Y, alpha = 0.25)
elastic2 <- glmnet(X, Y, alpha = 0.75)
ridge    <- glmnet(X, Y, alpha = 0.0)


# プロット
p1 <- lasso %>% autoplot(xvar = "lambda", main = "Lasso (alpha=1)") + theme(legend.position = 'none')
p2 <- elastic1 %>% autoplot(xvar = "lambda", main = "Elastic (alpha=0.25)") + theme(legend.position = 'none')
p3 <- elastic2 %>% autoplot(xvar = "lambda", main = "Elastic (alpha=0.75)") + theme(legend.position = 'none')
p4 <- ridge %>% autoplot(xvar = "lambda", main = "Ridge (alpha=0)") + theme(legend.position = 'none')
grid.arrange(p1, p2, p3, p4)




# 6.4.3 alphaの水準による変化 ---------------------------------------

# 乱数シード
set.seed(123)

# グリッドサーチ
cv_glmnet <-
  train(x = X, y = Y,
        method = "glmnet",
        preProc = c("zv", "center", "scale"),
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 10)


# 確認
# --- lambdaとalphaを同時にチューニング
cv_glmnet %>% print()


# チューニング結果
# --- lowest RMSE
cv_glmnet$bestTune


# プロット
cv_glmnet %>% ggplot()


# predict sales price on training data
pred <-cv_glmnet %>%  predict(X)
RMSE(exp(pred), exp(Y))





# 6.5 特徴量の解釈 ----------------------------------------------

# モデル準備
lasso    <- glmnet(X, Y, alpha = 1.0)
elastic1 <- glmnet(X, Y, alpha = 0.25)
elastic2 <- glmnet(X, Y, alpha = 0.75)
ridge    <- glmnet(X, Y, alpha = 0.0)


# プロット1
p1 <- lasso %>% vip(num_features = 20) + ggtitle("Lasso alpha=1")
p2 <- elastic1 %>% vip(num_features = 20) + ggtitle("Elastic1 alpha=0.25")
p3 <- elastic2 %>% vip(num_features = 20) + ggtitle("Elastic2 alpha=0.75")
p4 <- ridge %>% vip(num_features = 20) + ggtitle("Ridge alpha=0")
grid.arrange(p1, p2, p3, p4)



# プロット2
p1 <- lasso %>% vip(num_features = 20) %>% .$data %>% mutate(type = "lasso 1")
p2 <- elastic1 %>% vip(num_features = 20) %>% .$data %>% mutate(type = "elastic1 0.75")
p3 <- elastic2 %>% vip(num_features = 20) %>% .$data %>% mutate(type = "elastic2 0.25")
p4 <- ridge %>% vip(num_features = 20) %>% .$data %>% mutate(type = "ridge 0")

p1 %>%
  bind_rows(p2) %>%
  bind_rows(p3) %>%
  bind_rows(p4) %>%
  ggplot(aes(x = Variable, y = Importance, color = type, fill = type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~type)


# クロスバリデーション
cv_glmnet %>% vip(num_features = 20)




# 6.5.2 Partial Dependent Plot -----------------------

p1 <-
  cv_glmnet %>%
    pdp::partial(pred.var = "Gr_Liv_Area", grid.resolution = 20) %>%
    mutate(yhat = exp(yhat)) %>%
    ggplot(aes(Gr_Liv_Area, yhat)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p2 <-
  cv_glmnet %>%
    pdp::partial(pred.var = "Overall_QualExcellent") %>%
    mutate(yhat = exp(yhat),
           Overall_QualExcellent = factor(Overall_QualExcellent)) %>%
    ggplot(aes(Overall_QualExcellent, yhat)) +
    geom_boxplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)


p3 <-
  cv_glmnet %>%
    pdp::partial(pred.var = "First_Flr_SF", grid.resolution = 20) %>%
    mutate(yhat = exp(yhat)) %>%
    ggplot(aes(First_Flr_SF, yhat)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p4 <-
  cv_glmnet %>%
    pdp::partial(pred.var = "Garage_Cars") %>%
    mutate(yhat = exp(yhat)) %>%
    ggplot(aes(Garage_Cars, yhat)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

grid.arrange(p1, p2, p3, p4, nrow = 2)




# 6.6 Attrition Data ----------------------------------------------

# ＜ポイント＞
# - 分類データにおける正則化回帰の事例
#   --- family = "binomial"
#   --- {glmnet}と同様に{caret}でも使用可能


# データロード
data(attrition)


# データ準備
df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)


# 確認
df %>% as_tibble()
df$Attrition %>% table()


# データ分割
set.seed(123)
churn_split <- initial_split(df, prop = .7, strata = "Attrition")
train <- churn_split %>% training()
test  <- churn_split %>% testing()


# ロジスティック回帰
# --- 10fold Cross-Validation
set.seed(123)
glm_mod <- train(
  Attrition ~ .,
  data = train,
  method = "glm",
  family = "binomial",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10)
  )


# 正則化回帰
# --- 10fold Cross-Validation
set.seed(123)
penalized_mod <- train(
  Attrition ~ .,
  data = train,
  method = "glmnet",
  family = "binomial",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
  )


# データ抽出
list(logistic_model = glm_mod,
     penalized_model = penalized_mod) %>%
  resamples() %>%
  summary() %>%
  use_series(statistics) %>%
  use_series(Accuracy)
