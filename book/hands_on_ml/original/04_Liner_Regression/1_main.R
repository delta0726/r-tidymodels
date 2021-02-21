# ********************************************************************************
# Title   : 線形回帰
# Chapter : 4
# URL     : https://bradleyboehmke.github.io/HOML/linear-regression.html
# Support : https://koalaverse.github.io/homlr/notebooks/06-regularized-regression.nb.html
# ********************************************************************************


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
library(DALEX)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞
#・線形回帰モデルは基本的に説明変数が多いほど説明力が上がる
#・これは、係数と予測結果の解釈に齟齬が生じる可能性を示唆する




# 1. データ準備 ----------------------------------------------

# ・このデータは｢Sec2｣「Sec4」で使用する
# 


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




# 2. 単純な線形回帰 ----------------------------------------------

# モデリング
# --- 1変数のみを説明変数とした線形回帰
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)


# サマリー
model1 %>% summary() 


# RMSE
model1 %>% sigma()


# MSE
sigma(model1)^2


# 信頼区間
model1 %>% confint(level = 0.95)



# 3. プロット ----------------------------------------------

# ＜ポイント＞
#・残差の等分散性が維持されていないことを確認
#・OLS回帰は回帰の前提が維持できているかに関係なく直線を定義してしまう

# 散布図 + 回帰直線
p1 <- 
  model1 %>%
  broom::augment() %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Fitted regression line")


# 散布図 + 回帰直線 + 残差
p2 <- 
  model1 %>%
  broom::augment() %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  geom_segment(aes(x = Gr_Liv_Area, y = Sale_Price,
                   xend = Gr_Liv_Area, yend = .fitted), 
               alpha = 0.3) +
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Fitted regression line (with residuals)")


# Side-by-side plots
grid.arrange(p1, p2, nrow = 1)




# 4. 多重線形回帰  ----------------------------------------------


## モデリング ------

# 重回帰モデル
# --- 2変数
model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
model2 %>% coef


# 重回帰モデル
# --- 相互効果あり
model3 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Gr_Liv_Area:Year_Built, data = ames_train)
model3 <- lm(Sale_Price ~ (Gr_Liv_Area + Year_Built)^2, data = ames_train)
model3 %>% coef


## 相互効果 ------

# モデル構築
# --- fit1：相互効果なし
# --- fit2：相互効果あり
fit1 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train)
fit2 <- lm(Sale_Price ~ Gr_Liv_Area * Year_Built, data = ames_train)

# Regression plane data
plot_grid <- expand.grid(
  Gr_Liv_Area = seq(from   = min(ames_train$Gr_Liv_Area), 
                    to 　　= max(ames_train$Gr_Liv_Area), 
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



## 全パターン ------

# データ確認
# --- 81列
ames_train %>% dim()
ames_train %>% glimpse()


# モデリング　
# --- 変数を｢.｣で指定すると、カテゴリ変数はダミー変数化してモデルに導入される
# --- 小さいモデル(iris)
iris_model <- lm(Sepal.Length ~ ., data = iris) 
iris_model %>% tidy()


# --- この章のモデル
model3 <- lm(Sale_Price ~ ., data = ames_train) 
model3 %>% glance()


# 回帰係数の一覧
model3 %>% tidy()



# 5. モデル精度の評価 ---------------------------------------------

# ＜ポイント＞
#・最良なモデルをクロスバリデーションで探す
#・｢最良｣の定義として｢RMSE｣を使用する


## クロスバリデーション -----

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



## 結果比較 -----

# 全変数を導入したModel3のRMSEが最も低く最良であることがわかる
summary(resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2, 
  model3 = cv_model3
)))



# 6. モデルの懸念 ---------------------------------------------

# ＜ポイント＞
#・線形回帰モデルで説明変数を多くすると線形回帰の前提に違反する可能性が出る
#・これは、係数と予測結果の解釈に齟齬が生じる可能性を示唆する


## 1.線形関係 ----

# プロット1
# --- 元のデータ
# --- 非線形であてはめ
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
# --- データを対数変換
# --- 線形であてはめ
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



## 2.等分散性 ----

# 残差データを取得
# --- CV1(1変数モデル)を使用
df1 <- cv_model1$finalModel %>% augment(data = ames_train)
df1 %>% select(MS_SubClass, starts_with("."))


# 残差データを取得
# --- CV3(全変数モデル)を使用
df2 <- cv_model3$finalModel %>% augment(data = ames_train)
df2 %>% select(MS_SubClass, starts_with("."))

# プロット1
# --- 残差は等分散ではない
p1 <- 
  df1 %>% 
    ggplot(aes(.fitted, .resid)) + 
    geom_point(size = 1, alpha = .4) +
    xlab("Predicted values") +
    ylab("Residuals") +
    ggtitle("Model 1", subtitle = "Sale_Price ~ Gr_Liv_Area")

# プロット2
# --- 残差は小さくなっているが等分散とは言い切れない
p2 <- 
  df2 %>% 
    ggplot(aes(.fitted, .resid)) + 
    geom_point(size = 1, alpha = .4)  +
    xlab("Predicted values") +
    ylab("Residuals") +
    ggtitle("Model 3", subtitle = "Sale_Price ~ .")

# プロット比較
grid.arrange(p1, p2, nrow = 1)



## 3.自己相関 ----

# サンプルに番号をつける
df1 <- cv_model1$finalModel %>% augment(data = ames_train) %>% mutate(id = row_number())
df2 <- cv_model3$finalModel %>% augment(data = ames_train) %>% mutate(id = row_number())

# プロット1
# --- サンプルの並びに規則性がありそう
p1 <- 
  df1 %>% 
    ggplot(aes(id, .resid)) + 
    geom_point(size = 1, alpha = .4) +
    xlab("Row ID") +
    ylab("Residuals") +
    ggtitle("Model 1", subtitle = "Correlated residuals.")

# プロット2
# --- 規則性は緩和されている
p2 <- 
  df2 %>% 
    ggplot(aes(id, .resid)) + 
    geom_point(size = 1, alpha = .4) +
    xlab("Row ID") +
    ylab("Residuals") +
    ggtitle("Model 3", subtitle = "Uncorrelated residuals.")

# プロット比較
grid.arrange(p1, p2, nrow = 1)


## 4.サンプル数 ----

# 自明



## 5.多重共線性 ----

# fit with two strongly correlated variables
summary(cv_model3) %>%
  broom::tidy() %>%
  filter(term %in% c("Garage_Area", "Garage_Cars"))

# model without Garage_Area
set.seed(123)
mod_wo_Garage_Cars <- train(
  Sale_Price ~ ., 
  data = select(ames_train, -Garage_Cars), 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(mod_wo_Garage_Cars) %>%
  broom::tidy() %>%
  filter(term == "Garage_Area")


