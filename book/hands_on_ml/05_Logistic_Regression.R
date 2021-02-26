# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : Chapter 5 Logistic Regression
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/11
# URL       : https://bradleyboehmke.github.io/HOML/logistic-regression.html
#           : https://koalaverse.github.io/homlr/notebooks/05-logistic-regression.nb.html
# ***************************************************************************************


# ＜ポイント＞
# - 応答変数がバイナリ変数(TRUE/FALSE)の場合はロジスティック回帰を使う
# - ロジスティック回帰は線形回帰と同様に多くの仮定を持つ（係数の線形性、多重共線性）
# - 予測するクラスが2つ以上ある場合を多項分類とよび、 ロジスティック回帰の多項式拡張が存在する
#   --- 仮定は増加することで、多くの場合に係数推定の安定性は低下する



# ＜目次＞
# 5.1 準備
# 5.2 ロジスティック回帰を使う理由
# 5.3 単純なロジスティック回帰
# 5.4 多項ロジスティック回帰
# 5.5 モデル精度の評価
# 5.6 モデルの注意事項
# 5.7 モデル解釈



# 5.1 準備 ---------------------------------------------------

# ＜ポイント＞
#
#


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
data(attrition)


# データ加工
df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)


# データ分割
set.seed(123)
churn_split <- df %>% initial_split(prop = .7, strata = "Attrition")
churn_train <- churn_split %>% training()
churn_test  <- churn_split %>% testing()



# 5.2 ロジスティック回帰を使う理由 ---------------------------------------------------

# ＜ポイント＞
# - クレジットカード顧客のデフォルトデータがあり、残高とデフォルトの関係を導き出したい
#   --- デフォルトを1、その他を0とする
# - 線形回帰の場合はデフォルト確率を予測していることとなる
#   --- ゼロに近い予測の場合にマイナスとなる
#   --- 非常に大きい予測の場合に1を超えることがある
# - すべての値に対して0-1の間の出力を提供する関数を使用してモデル化する必要がある
#   --- ロジット変換をp(x)に適用することで線形方程式とする



# データ確認
df_default <-
  ISLR::Default %>%
    mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
    as_tibble()


# 確認
df_default %>% print()


# 線形回帰
# --- geom_smooth()でlmを使って直線をひく
# --- 不適切
p1 <-
  df_default %>%
    ggplot(aes(balance, prob)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "lm") +
    ggtitle("Linear regression model fit") +
    xlab("Balance") +
    ylab("Probability of Default")


# GLM
# --- geom_smooth()でGMLを使ってロジスティック曲線をひく
# --- Xのすべての値をロジスティック関数で0-1の確率に変換する
# --- S字カーブの関数を描くことができる
p2 <-
  df_default %>%
    ggplot(aes(balance, prob)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggtitle("Logistic regression model fit") +
    xlab("Balance") +
    ylab("Probability of Default")


# 比較
gridExtra::grid.arrange(p1, p2, nrow = 1)




# 5.3 単純なロジスティック回帰 ----------------------------------------------

# ＜ポイント＞
# - glm()はロジスティック回帰と単純な線形回帰の両方を含むモデルのクラスである一般化線形モデルに適合する
#   --- familyで確率分布を設定する
#   --- 初期設定はfamily = "gaussian"（正規分布）



# モデル構築
# --- family = "binomial"（二項分布）
# --- model1 ： 給与
# --- model2 ： 残業時間
model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = churn_train)
model2 <- glm(Attrition ~ OverTime, family = "binomial", data = churn_train)


# データ加工
raw_data <- churn_train %>% mutate(prob = ifelse(Attrition == "Yes", 1, 0))
churn_train2 <- model2 %>% augment(raw_data) %>% mutate(.fitted = exp(.fitted))


# 確認
churn_train2 %>% as_tibble()
churn_train2 %>% glimpse()


# 散布図
# --- 離職率(Y)と給与(X)の関係
# --- あまり関係がなさそう
p1 <-
  churn_train2 %>%
    ggplot(aes(x = MonthlyIncome, y = prob)) +
    geom_point(alpha = 0.15) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggtitle("Predicted probabilities for model1") +
    xlab("Monthly Income") +
    ylab("Probability of Attrition")


# プロット作成
# --- 離職率の推定値(Y)と残業(X)の関係
# --- 関係ありそう
p2 <-
  churn_train2 %>%
    ggplot(aes(x = OverTime, y = .fitted, color = OverTime)) +
    geom_boxplot(show.legend = FALSE) +
    geom_rug(sides = "b", position = "jitter", alpha = 0.2, show.legend = FALSE) +
    ggtitle("Predicted probabilities for model2") +
    xlab("Over Time") +
    scale_y_continuous("Probability of Attrition", limits = c(0, 1))


# プロット
gridExtra::grid.arrange(p1, p2, nrow = 1)



# 回帰係数サマリー
# --- 係数がかなり小さい
model1 %>% tidy()
model2 %>% tidy()


# 回帰係数
# --- 指数関数に変換
# --- OverTimeYesとの関係が強い
model1 %>% coef() %>% exp()
model2 %>% coef() %>% exp()


# 信頼区間
model1 %>% confint()
model2 %>% confint()




# 5.4 多項ロジスティック回帰 ----------------------------------------------

# ＜ポイント＞
# - 複数の予測子を使用してバイナリ応答を予測することもできる


# モデル構築
model3 <- glm(Attrition ~ MonthlyIncome + OverTime, family = "binomial", data = churn_train)


# 回帰係数サマリー
model3 %>% tidy()


# 予測値をデータセットに追加
churn_train3 <- churn_train %>% mutate(prob = ifelse(Attrition == "Yes", 1, 0))
churn_train3 <- model3 %>% augment(churn_train3) %>% mutate(.fitted = exp(.fitted))


# プロット作成
churn_train3 %>%
  ggplot(aes(x = MonthlyIncome, y = prob, color = OverTime)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    ggtitle("Predicted probabilities for model3") +
    xlab("Monthly Income") +
    ylab("Probability of Attrition")




# 5.5 モデル精度の評価 ----------------------------------------------

# クロスバリデーション ----

# モデル1
# --- Attrition ~ MonthlyIncome
set.seed(123)
cv_model1 <- train(Attrition ~ MonthlyIncome,
                   data = churn_train,
                   method = "glm",
                   family = "binomial",
                   trControl = trainControl(method = "cv", number = 10))

# モデル2
# ---Attrition ~ MonthlyIncome + OverTime
set.seed(123)
cv_model2 <- train(Attrition ~ MonthlyIncome + OverTime,
                   data = churn_train,
                   method = "glm",
                   family = "binomial",
                   trControl = trainControl(method = "cv", number = 10))

# モデル3
# ---Attrition ~ .
set.seed(123)
cv_model3 <- train(Attrition ~ .,
                   data = churn_train,
                   method = "glm",
                   family = "binomial",
                   trControl = trainControl(method = "cv", number = 10))


# extract out of sample performance measures
list(model1 = cv_model1,
     model2 = cv_model2,
     model3 = cv_model3) %>%
  resamples() %>%
  summary() %>%
  use_series(statistics) %>%
  use_series(Accuracy)



# predict class
pred_class <- cv_model3 %>% predict(churn_train)

# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = "Yes"),
  reference = relevel(churn_train$Attrition, ref = "Yes")
)


library(ROCR)

# Compute predicted probabilities
m1_prob <- cv_model1 %>% predict(churn_train, type = "prob") %>% use_series(Yes)
m3_prob <- cv_model3 %>% predict(churn_train, type = "prob") %>% use_series(Yes)

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <-
  m1_prob %>%
    prediction(churn_train$Attrition) %>%
    performance(measure = "tpr", x.measure = "fpr")

perf2 <-
  m3_prob %>%
    prediction(churn_train$Attrition) %>%
    performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
perf1 %>% plot(col = "black", lty = 2)
perf2 %>% plot(add = TRUE, col = "blue")


legend(0.8, 0.2, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)




# 5.5.2 PLS回帰によるロジスティック回帰 --------------------------

# モデル訓練
# --- method = "pls"
# --- クロスバリデーション
set.seed(123)
cv_model_pls <-
  train(Attrition ~ .,
        data = churn_train,
        method = "pls",
        family = "binomial",
        trControl = trainControl(method = "cv", number = 10),
        preProcess = c("zv", "center", "scale"),
        tuneLength = 16)


# Model with lowest RMSE
cv_model_pls$bestTune


# Plot cross-validated RMSE
cv_model_pls %>% ggplot()



# 5.6 モデルの注意事項 ----------------------------------------------



# 5.7 モデル解釈 ----------------------------------------------

# 5.7.1 変数重要度 -----------------------------------------

# 重要度プロット
cv_model3 %>% vip(num_features = 20)



# 5.7.2 Partial Dependence Plot -----------------------------------------

# ラッパー関数
pred.fun <- function(object, newdata) {
  Yes <- mean(predict(object, newdata, type = "prob")$Yes)
  as.data.frame(Yes)
}


# プロット作成
p1 <-
  cv_model3 %>%
    pdp::partial(pred.var = "OverTime", pred.fun = pred.fun) %>%
    autoplot(rug = TRUE) +
    ylim(c(0, 1))

p2 <-
  cv_model3 %>%
    pdp::partial(pred.var = "JobSatisfaction", pred.fun = pred.fun) %>%
    autoplot() +
    ylim(c(0, 1))

p3 <-
  cv_model3 %>%
    pdp::partial(pred.var = "NumCompaniesWorked", pred.fun = pred.fun, gr = 10) %>%
    autoplot() +
    scale_x_continuous(breaks = 0:9) +
    ylim(c(0, 1))


p4 <-
  cv_model3 %>%
    pdp::partial(pred.var = "EnvironmentSatisfaction", pred.fun = pred.fun) %>%
    autoplot() +
    ylim(c(0, 1))


grid.arrange(p1, p2, p3, p4, nrow = 2)