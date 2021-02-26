# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 4 Linear Regression
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/9
# URL       : https://bradleyboehmke.github.io/HOML/linear-regression.html
#           : https://koalaverse.github.io/homlr/notebooks/04-linear-regression.nb.html
# **********************************************************************************


# ＜ポイント＞
# - 線形回帰は教師あり学習を行うための最も単純なアルゴリズムの一つ
# - 線形回帰は比較的つよい前提を持つので、使う際には前提を念頭におく必要がある
# - PCAなどの次元削減のアルゴリズムと融合することができる



# ＜目次＞
# 4.1 準備
# 4.2 単純な線形回帰
# 4.3 重回帰
# 4.4 モデル精度の評価
# 4.5 モデルの注意事項
# 4.6 PCR回帰
# 4.7 PLS回帰
# 4.8 モデル解釈




# 4.1 準備 ---------------------------------------------------

library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(glmnet)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(isoband)
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




# 4.2 単純な線形回帰 ---------------------------------------------------

# ＜ポイント＞
# - 1次の線形回帰においては最適なラインを定義するためb0とb1を推定する必要がある



# モデリング
# --- 1変数のみを説明変数とした線形回帰
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)
model1 %>% print()



# 4.2.1 残差プロット ---------------------------------------------------

# ＜ポイント＞
# - 線形回帰では残差が最も小さくなるように回帰直線が定義される


# 個別データの取得
df_arg <- model1 %>% broom::augment()
df_arg %>% print()


# 散布図 + 回帰直線
p1 <-
  df_arg %>%
    ggplot(aes(Gr_Liv_Area, Sale_Price)) +
    geom_point(size = 1, alpha = 0.3) +
    geom_smooth(se = FALSE, method = "lm") +
    scale_y_continuous(labels = scales::dollar) +
    ggtitle("Fitted regression line")


# 散布図 + 回帰直線 + 残差
p2 <-
  df_arg %>%
    ggplot(aes(Gr_Liv_Area, Sale_Price)) +
    geom_segment(aes(x = Gr_Liv_Area, y = Sale_Price, xend = Gr_Liv_Area, yend = .fitted),
                 alpha = 0.3) +
    geom_point(size = 1, alpha = 0.3) +
    geom_smooth(se = FALSE, method = "lm") +
    scale_y_continuous(labels = scales::dollar) +
    ggtitle("Fitted regression line (with residuals)")


# プロット比較
grid.arrange(p1, p2, nrow = 1)




# 4.2.1 推定値 ---------------------------------------------------

# ＜ポイント＞
# - 線形回帰は解釈が明解なので直観的な理解につながる
# - 線形回帰の欠点は誤差分散の推定値が出力されないこと
#   --- 最尤推定の方法を使用することで算出可能


# サマリー
# - b0= 8732.94
# - b1 = 114.88
# - 地上の居住空間が1平方フィート増えるごとに平均販売価格が114.88増加すると推定
model1 %>% summary()


# RMSE
model1 %>% sigma()


# MSE
sigma(model1)^2



# 4.2.2 信頼区間 ---------------------------------------------------

# 信頼区間
model1 %>% confint(level = 0.95)






# 4.3 重回帰 ---------------------------------------------------

# ＜ポイント＞
# - 線形回帰モデルは変数を追加することで拡張することができる
# - 予測モデリングにおいて｢相互効果｣は非常に一般的な概念



# 4.3.1 重回帰モデル ------------------------

# ＜ポイント＞
# - 現実的には、予測にあたり説明変数は複数存在するのが一般的
# - その際には、多重線形回帰モデル(MLR)を用いる
#   --- Rでは、変数を+で区切ることにより、複数の線形回帰モデルを適合させることができます。
# - プロットでは直線的な等高線が描かれる


# 重回帰モデル
# --- 2変数
model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
model2 %>% print()



# 4.3.2 変数間の相互効果 ------------------------

# ＜ポイント＞
# - モデルに複数の変数の相互効果を含めることもできる
# - 相互効果は一般的に小さくなる
# - プロットでは曲線的な等高線が描かれる（相互効果は曲率を示す）
# - 予測モデリングにおいて｢相互効果｣は非常に一般的な概念


# 重回帰モデル
# --- 2変数 + 相互効果あり
# --- 次のように表記することもできる : lm(Sale_Price ~ Gr_Liv_Area * Year_Built, data = ames_train)
model3 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Gr_Liv_Area:Year_Built, data = ames_train)
model3 <- lm(Sale_Price ~ (Gr_Liv_Area + Year_Built)^2, data = ames_train)
model3 %>% print()
model3 %>% coef() %>% round(3)



# 4.3.3 プロットによる比較 ------------------------

# モデル構築
# --- fit1：相互効果なし
# --- fit2：相互効果あり
fit1 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
fit2 <- lm(Sale_Price ~ Gr_Liv_Area * Year_Built, data = ames_train)


# Regression plane data
plot_grid <- expand.grid(
  Gr_Liv_Area = seq(from   = min(ames_train$Gr_Liv_Area),
                    to     = max(ames_train$Gr_Liv_Area),
                    length = 100),
  Year_Built = seq(from    = min(ames_train$Year_Built),
                   to      = max(ames_train$Year_Built),
                   length  = 100)
  )


# 予測データの作成
# --- fit1：相互効果なし
# --- fit2：相互効果あり
plot_grid$y1 <- fit1 %>% predict(newdata = plot_grid)
plot_grid$y2 <- fit2 %>% predict(newdata = plot_grid)


# 確認
plot_grid %>% as_tibble()


# Level plots
p1 <-
  plot_grid %>%
    ggplot(aes(x = Gr_Liv_Area, y = Year_Built, z = y1, fill = y1)) +
    geom_tile() +
    geom_contour(color = "white") +
    viridis::scale_fill_viridis(name = "Predicted\nvalue", option = "inferno") +
    theme_bw() +
    ggtitle("Main effects only")

p2 <-
  plot_grid %>%
    ggplot(aes(x = Gr_Liv_Area, y = Year_Built, z = y2, fill = y1)) +
    geom_tile() +
    geom_contour(color = "white") +
    viridis::scale_fill_viridis(name = "Predicted\nvalue", option = "inferno") +
    theme_bw() +
    ggtitle("Main effects with two-way interaction")

# プロット
grid.arrange(p1, p2, nrow = 1)



# データ確認
# --- 81列
ames_train %>% dim()
ames_train %>% glimpse()



# 4.3.4 全変数をモデルに投入 ------------------------

# ＜ポイント＞
# - Rでは｢.｣を使うことで、データフレームの全変数をモデルに投入することができる
#   --- 理論的にはデータの行数よりも多くならないようにする必要がある



#
model3 <- lm(Sale_Price ~ ., data = ames_train)
model3 %>% glance()


# 回帰係数の一覧
model3 %>% tidy()




# 4.4 モデル精度の評価 -------------------------------

# ＜ポイント＞
#・最良モデルの定義として｢RMSE｣を導入する
#・最良モデルをクロスバリデーションとRMSEを用いて決定する



# 4.4.1 クロスバリデーション --------------------------

# ＜ポイント＞
# - ここでは{caret}のtrain()でクロスバリデーションを行っている
#   --- 全て内部的に処理されていて何をしているのか分かりにくい（その分1つの関数で表現できる）
#   --- {tune}はプロセスが分かるように配慮されている


# ＜プロセス＞
# 1. データセットを10Fold-Cross Validationとなるように分割してリスト化
# 2. 各Foldの訓練データでモデルを作成、検証データでモデル評価
# 3. 各FoldでRMSEなどの統計量を計算
# 4. FoldごとのRMSEの平均値と標準誤差から最終的に評価
#    --- {caret}の場合は平均値を最終出力としている


# model 1 CV
# ---  Sale_Price ~ Gr_Liv_Area
set.seed(123)
cv_model1 <- train(
  form = Sale_Price ~ Gr_Liv_Area,
  data = ames_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

# Foldごとの統計量
cv_model1$resample
cv_model1$resample$RMSE %>% mean()

# モデル出力
cv_model1 %>% print()



# 4.4.2 モデルごとの比較 --------------------------

# model 1 CV
# ---  Sale_Price ~ Gr_Liv_Area
set.seed(123)
cv_model1 <- train(
  form = Sale_Price ~ Gr_Liv_Area,
  data = ames_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)


# model 2 CV
# --- Sale_Price ~ Gr_Liv_Area + Year_Built
set.seed(123)
cv_model2 <- train(
  Sale_Price ~ Gr_Liv_Area + Year_Built,
  data = ames_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)


# model 3 CV
# --- Sale_Price ~ .
set.seed(123)
cv_model3 <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)


# モデルごとの比較
# --- model3のRMSEが最も低く最良モデル
# --- caret::resamples()
list(model1 = cv_model1,
     model2 = cv_model2,
     model3 = cv_model3) %>%
  resamples() %>%
  summary()



# 4.5 モデルの注意事項 ---------------------------------------------------

# ＜ポイント＞
# - 線形回帰モデルは強い仮定を置いたうえで行っている
#   --- 仮定に違反すると正しい解釈が行えない可能性があるので要注意
#   --- 回帰モデルの仮定を常に考えたうえでモデルを使うことが求められる



# 4.5.1 線形性の仮定 --------------------------------------------

# ＜ポイント＞
# - 線形回帰は予測子と応答変数の間に線形関係を想定している
#   --- 対数変換などにより、非線形な関係は線形関係に予め戻しておく必要がある
#   --- 以下の例ではY軸を対数変換している


# プロット1
# --- 元のデータ
# --- 非線形なのでloessであてはめ
p1 <-
  ames_train %>%
    ggplot(aes(Year_Built, Sale_Price)) +
    geom_point(size = 1, alpha = .4) +
    geom_smooth(se = FALSE) +
    scale_y_continuous("Sale price", labels = scales::dollar) +
    xlab("Year built") +
      ggtitle(paste("Non-transformed variables with a\n",
                    "non-linear relationship."))

# プロット2
# --- Y軸を対数変換
# --- lmとloessで当てはめ
p2 <-
  ames_train %>%
    ggplot(aes(Year_Built, Sale_Price)) +
    geom_point(size = 1, alpha = .4) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE) +
    scale_y_log10("Sale price", labels = scales::dollar,
                  breaks = seq(0, 400000, by = 100000)) +
    xlab("Year built") +
    ggtitle(paste("Transforming variables can provide a\n",
                  "near-linear relationship."))

# プロット比較
# --- 対数変換で非線形性を緩和しているが、まだ非線形性は残っている
grid.arrange(p1, p2, nrow = 1)



# 4.5.2 等分散性の仮定 --------------------------------------------


# ＜ポイント＞
# - 線形回帰では誤差項の分散がYに水準に対して一定であることを仮定している
#   --- ｢線形性の仮定｣と同様に、変数変換で解決できることが多い
#   --- 等分散性が担保できない場合は｢信頼区間｣が説明力を失う


# 残差データを取得
# --- CV1(1変数モデル)を使用
# --- CV1は｢4.4.2｣で計算
df1 <- cv_model1$finalModel %>% augment(data = ames_train)
df1 %>% select(MS_SubClass, starts_with("."))


# 残差データを取得
# --- CV3(全変数モデル)を使用
# --- CV3は｢4.4.2｣で計算
df2 <- cv_model3$finalModel %>% augment(data = ames_train)
df2 %>% select(MS_SubClass, starts_with("."))


# 散布図1
# --- 予測値と残差をプロット
# --- 残差は等分散ではない
p1 <-
  df1 %>%
    ggplot(aes(.fitted, .resid)) +
    geom_point(size = 1, alpha = .4) +
    xlab("Predicted values") +
    ylab("Residuals") +
    ggtitle("Model 1", subtitle = "Sale_Price ~ Gr_Liv_Area")


# 散布図2
# --- 予測値と残差をプロット
# --- 残差は小さくなっているが等分散とは言い切れない
p2 <-
  df2 %>%
    ggplot(aes(.fitted, .resid)) +
    geom_point(size = 1, alpha = .4)  +
    xlab("Predicted values") +
    ylab("Residuals") +
    ggtitle("Model 3", subtitle = "Sale_Price ~ .")


# プロット比較
# --- プロット1：残差は等分散ではない
# --- プロット2：残差は小さくなっているが等分散とは言い切れない
grid.arrange(p1, p2, nrow = 1)



# 4.5.3 残差の無相関性 --------------------------------------------

# ＜ポイント＞
# - 線形回帰では誤差項が夢想間であることを仮定している


# サンプルに番号をつける
# --- サンプルを順に並べるため
df1 <- cv_model1$finalModel %>% augment(data = ames_train) %>% mutate(id = row_number())
df2 <- cv_model3$finalModel %>% augment(data = ames_train) %>% mutate(id = row_number())


# プロット1
# ---  Sale_Price ~ Gr_Liv_Area
p1 <-
  df1 %>%
    ggplot(aes(id, .resid)) +
    geom_point(size = 1, alpha = .4) +
    xlab("Row ID") +
    ylab("Residuals") +
    ggtitle("Model 1", subtitle = "Correlated residuals.")


# プロット2
# ---  Sale_Price ~ .
p2 <-
  df2 %>%
    ggplot(aes(id, .resid)) +
    geom_point(size = 1, alpha = .4) +
    xlab("Row ID") +
    ylab("Residuals") +
    ggtitle("Model 3", subtitle = "Uncorrelated residuals.")


# プロット比較
# --- プロット1：パターンが存在する（Gr_Liv_Areaを使うと、サンプル順に似たレコードが出現する）
# --- プロット2：全ての変数を使うと緩和される
grid.arrange(p1, p2, nrow = 1)




# 4.5.4 予測値とサンプル数の関係 --------------------------------------------

# ＜ポイント＞
# - 特徴の数が観測の数(p)がサンプル数(n)を超えるとOLS推定を定義することができない
#   --- あまり見ないケースだが、特徴量が異常に多く取得できる場合には起こりえる
#   --- 変数削減で対応する
#   --- 正則化回帰はp>nの制約がないので代替案として有力




# 4.5.5.多重共線性 --------------------------------------------

# ＜ポイント＞
# - 多重共線性は2つ異常の説明変数が非常に高い相関を持つことで発生する
#   --- recipe::step_corr()などを使って前処理段階で対処する必要がある
#   --- PCA回帰やPLS回帰も代替案として有力


# ＜解釈＞
# - 実は、amesデータセットは相関の高い特徴量が複数含まれている
#   --- Garage_AreaとGarage_Carsは0.89の相関を持ち、どちらもSale_Priceと強く関連している
#   --- これらの変数の両方が含まれている完全なモデルを見ると、
#   Garage_Carsは統計的に有意であることがわかりますが、Garage_Areaはそうではありません。


# 相関係数
# --- ｢Garage_Area｣と｢Garage_Cars｣の相関係数はかなり高い
ames_train %>%
  select(Garage_Area, Garage_Cars) %>%
  cor()


# 変数サマリー
# --- どちらも説明力を持っている
cv_model3 %>%
  summary() %>%
  broom::tidy() %>%
  filter(term %in% c("Garage_Area", "Garage_Cars"))


# モデル構築
# --- 10Fold Cross-Validation
# --- Garage_Carsをデータから除外
set.seed(123)
mod_wo_Garage_Cars <- train(
  Sale_Price ~ .,
  data = select(ames_train, -Garage_Cars),
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)


# 変数サマリー
# --- ｢Garage_Cars｣を除いたことで、｢Garage_Area｣の説明力がより高まった。
summary(mod_wo_Garage_Cars) %>%
  broom::tidy() %>%
  filter(term == "Garage_Area")





# 4.6 PCR回帰 ---------------------------------------------------

# ＜ポイント＞
# - 主成分分析を使うと、データセットから相関がゼロとなる特徴量(主成分)を作り出すことができる
#   --- 多重共線性問題を完全に排除することができる
# - {PLS}や{caret}を用いて実行することができる
#   --- {caret}の場合は交差検証も合わせて行うことができる


# ＜問題点＞
# - PCAステップによって生成される新しい特徴量同士は直交化しているため多重共線性は生じない
# - ただし、PCAの処理は応答変数との関係性について何も保証していない
#   --- PC特徴量が応答変数との関係性を示しているかは不明


# ＜手順＞
# - Step1: 元のデータセットをPCAにより主成分に分解
# - Step2: PCを説明変数として線形回帰を行う


# モデル精度の検証
# --- PC20まで使用
# --- 変数のゼロ分散フィルタを行っている（PCAでは分散の低い特徴量が出るため）
set.seed(123)
cv_model_pcr <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)


# RMSE
# --- PC20に近いところが表示される
# --- PCをなるべく使うほうが情報量が多いため
cv_model_pcr$bestTune


# プロット
# --- RMSEは34000程度までしか下がらない
cv_model_pcr %>% ggplot()




# 4.7 PLS回帰 ---------------------------------------------------

# ＜ポイント＞
# - PLSは特徴量の分散最大化に加えて、応答変数との相関をPC1から順に高くなるように変換する
#   --- 部分最小二乗(PLS)は教師あり次元削減手順と見なすことができる
#   --- 4.7.1でPC1と応答変数の線形関係が維持されている


# 4.7.1 PCRとの比較 ---------------------------------------

# ＜ポイント＞
# - PCRとPLSの説明変数と応答変数の関係を確認する
#   --- PLSの場合、PC1が応答変数に対して線形関係を維持してい
#   --- {recipe}でstep_pls()として提供されている


library(AppliedPredictiveModeling)
library(recipes)
library(tidyr)


# データ準備
data(solubility)
df <- cbind(solTrainX, solTrainY)


# データ作成
# --- step_pca()
pca_df <-
  recipe(solTrainY ~ ., data = df) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_pca(all_predictors()) %>%
    prep(training = df, retain = TRUE) %>%
    juice() %>%
    select(PC1, PC2, solTrainY) %>%
    rename(`PCR Component 1` = "PC1", `PCR Component 2` = "PC2") %>%
    gather(component, value, -solTrainY)


# データ作成
# --- step_pls()
pls_df <-
  recipe(solTrainY ~ ., data = df) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_pls(all_predictors(), outcome = "solTrainY") %>%
    prep(training = df, retain = TRUE) %>%
    juice() %>%
    rename(`PLS Component 1` = "PLS1", `PLS Component 2` = "PLS2") %>%
    gather(component, value, -solTrainY)


# 散布図
# --- 主成分(PC)と応答変数の関係
# --- PCRは応答変数と主成分との関係があいまい
# --- PLSは応答変数と主成分が線形関係を維持している
pca_df %>%
  bind_rows(pls_df) %>%
  ggplot(aes(value, solTrainY)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "lm", se = FALSE, lty = "dashed") +
  facet_wrap(~ component, scales = "free") +
  labs(x = "PC Eigenvalues", y = "Response")




# 4.7.2 PLSの実行 ---------------------------------------

# モデル精度の検証
# --- PC20まで使用
set.seed(123)
cv_model_pls <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)


# RMSE
# --- PC10がベスト
# --- PCRよりも小さいPCで完結する
cv_model_pls$bestTune


# プロット
# --- RMSEは26000程度まで下がる
# --- PCRよりも予測精度が高い
cv_model_pls %>% ggplot()




# 4.8 モデル解釈 -----------------------------

# 4.8.1 変数重要度分析 --------------------------

# ＜ポイント＞
# - 線形回帰の多くのモデルでは特徴量のt値によって変数重要度を確認することが可能
# - 以下の場合、機械学習で用いる変数重要度分析を行うほうがよい
#   --- モデルに相互効果を含む
#   --- PCAなどの複雑な変換が行われる


# 変数重要度
cv_model_pls %>% vip(num_features = 20, method = "model")



# 4.8.1 Partial Dependence Plot --------------------------

p1 <-
  cv_model_pls %>%
    pdp::partial(pred.var = "Gr_Liv_Area", grid.resolution = 20) %>%
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p2 <-
  cv_model_pls %>%
    pdp::partial(pred.var = "First_Flr_SF", grid.resolution = 20) %>%
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p3 <-
  cv_model_pls %>%
    pdp::partial(pred.var = "Total_Bsmt_SF", grid.resolution = 20) %>%
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

p4 <-
  cv_model_pls %>%
    pdp::partial(pred.var = "Garage_Cars", grid.resolution = 4) %>%
    autoplot() +
    scale_y_continuous(limits = c(0, 300000), labels = scales::dollar)

grid.arrange(p1, p2, p3, p4, nrow = 2)




