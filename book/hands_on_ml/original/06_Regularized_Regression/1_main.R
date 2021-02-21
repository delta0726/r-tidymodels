# ********************************************************************************
# Title   : 正則化回帰
# Chapter : 6
# URL     : https://bradleyboehmke.github.io/HOML/regularized-regression.html
# Support : https://koalaverse.github.io/homlr/notebooks/06-regularized-regression.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(magrittr)
library(recipes)
library(rsample)
library(glmnet)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# 0. ポイント整理 ----------------------------------------------

# ＜正則化回帰＞
# ・変数重要度を考慮した回帰が可能となる
# ・Lasso回帰は変数選択の要素を持つ
# ・ElasticNet回帰でRidge回帰の厳しさをコントロールすることができる
# ・IMLやDALEXといった変数解釈フレームワークでの検証も可能


# ＜注意点＞
# ・欠損値を自動的に処理することはできない (前処理が必要)
# ・外れ値に対する頑健性はない (ロバスト回帰のような処理はない)
# ・線形関係を想定している (相互効果を含めることは可能)




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


# モデル用データ
X <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
Y <- ames_train$Sale_Price %>% log()



# 2. 線形回帰の問題点 ----------------------------------------------

# データサンプリング
ames_sub <- 
  ames_train %>%
    filter(Gr_Liv_Area > 1000 & Gr_Liv_Area < 3000) %>%
    sample_frac(.5)


# 線形回帰
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_sub)


# プロット
# --- 回帰の前提である残差の｢正規性｣｢等分散性｣が保持されていない
# --- 誤差に規則性がある場合、回帰が見逃している規則性がある可能性
model1 %>%
  broom::augment() %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  geom_segment(aes(x = Gr_Liv_Area, y = Sale_Price,
                   xend = Gr_Liv_Area, yend = .fitted), 
               alpha = 0.3) +
  geom_point(size = 1, color = "red") +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) + 
  theme_tq()




# 3. 実装  ----------------------------------------------

# リッジ回帰
ridge <- glmnet(x = X, y = Y, alpha = 0)
ridge %>% summary()


# ラムダの確認
ridge$lambda %>% head()
ridge$lambda %>% plot_histogram()


# 回帰係数
ridge %>% coef()



# 5. チューニング  ----------------------------------------------

# Apply CV ridge regression to Ames data
ridge <- cv.glmnet(x = X, y = Y, alpha = 0)
lasso <- cv.glmnet(x = X, y = Y, alpha = 1)

# 確認
ridge %>% summary()
lasso %>% summary()

# plot results
par(mfrow = c(1, 2))
ridge %>% plot(main = "Ridge penalty\n\n")
lasso %>% plot(main = "Lasso penalty\n\n")

# プロット
# --- お手軽
ridge %>% autoplot()
lasso %>% autoplot()


# 6. 比較  ----------------------------------------------

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



# 7. 最適なラムダ  ----------------------------------------------

# 乱数シード
set.seed(123)

# グリッドサーチ 
cv_glmnet <- 
  train(x = X, y = Y, 
        method = "glmnet", 
        preProc = c("zv", "center", "scale"), 
        trControl = trainControl(method = "cv", number = 10), 
        tuneLength = 10)


# チューニング結果
# --- lowest RMSE
cv_glmnet$bestTune


# プロット
cv_glmnet %>% ggplot()


# predict sales price on training data
pred <-cv_glmnet %>%  predict(X)
RMSE(exp(pred), exp(Y))





# 8. 変数重要度  ----------------------------------------------

# 準備 ---- 

# モデル
lasso    <- glmnet(X, Y, alpha = 1.0) 
elastic1 <- glmnet(X, Y, alpha = 0.25) 
elastic2 <- glmnet(X, Y, alpha = 0.75) 
ridge    <- glmnet(X, Y, alpha = 0.0)

# プロット1 ---- 

p1 <- lasso %>% vip(num_features = 20) + ggtitle("Lasso alpha=1")
p2 <- elastic1 %>% vip(num_features = 20) + ggtitle("Elastic1 alpha=0.25")
p3 <- elastic2 %>% vip(num_features = 20) + ggtitle("Elastic2 alpha=0.75")
p4 <- ridge %>% vip(num_features = 20) + ggtitle("Ridge alpha=0")
grid.arrange(p1, p2, p3, p4)



# プロット2 ---- 

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

# プロット3 ----

# クロスバリデーション
cv_glmnet %>% vip(num_features = 20)


# 9. 変数感応度  ----------------------------------------------


# 準備 ---- 

# モデル
lasso    <- glmnet(X, Y, alpha = 1.0) 
elastic1 <- glmnet(X, Y, alpha = 0.25) 
elastic2 <- glmnet(X, Y, alpha = 0.75) 
ridge    <- glmnet(X, Y, alpha = 0.0)




