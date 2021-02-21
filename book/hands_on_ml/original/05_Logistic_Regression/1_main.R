# ********************************************************************************
# Title   : ロジスティック回帰
# Chapter : 5
# URL     : https://bradleyboehmke.github.io/HOML/logistic-regression.html
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
#・応答変数がバイナリ変数(TRUE/FALSE)の場合はロジスティック回帰を使う
#・ロジスティック回帰も線形回帰と同様に等分散性など多くの仮定に縛られる
#・ロジスティック回帰の多項拡張は存在するが、仮定がさらに増えるので精度が改善しにくい
#・線形回帰と同様にPCR回帰やPLS回帰を用いたロジスティック回帰が使える



# 1. データ準備 ----------------------------------------------

# ・このデータは｢Sec2｣「Sec4」で使用する
# 


# データ取得
Attrition <- 
  read_csv("data/Attrition.csv") %>% 
    mutate_if(is.ordered, factor, ordered = FALSE) %>% 
    mutate(Attrition = ifelse(Attrition == "Yes", 1, 0))


# データ分割
set.seed(123) 
churn_split  <- Attrition %>% initial_split(prop = 0.7, strata = "Attrition")
churn_train  <- churn_split %>% training()
churn_test   <- churn_split %>% testing()


# データ確認
churn_split %>% print()
churn_train %>% dim()
churn_test %>% dim()




# 2. 単純なロジスティック回帰 ----------------------------------------------

# モデル構築
# --- glm()による単回帰モデル
# --- リンク関数にbinomial(二項分布)を指定する
# --- model1:給与　model2：残業時間
model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = churn_train)
model2 <- glm(Attrition ~ OverTime, family = "binomial", data = churn_train)



churn_train <- churn_train %>% mutate(prob = ifelse(Attrition == "Yes", 1, 0))
churn_train2 <- broom::augment(model2, churn_train) %>% mutate(.fitted = exp(.fitted))


# プロット作成
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




# 3. 多項ロジスティック回帰 ----------------------------------------------

# モデル構築
model3 <- glm(
  Attrition ~ MonthlyIncome + OverTime,
  family = "binomial", 
  data = churn_train
)


# 回帰係数サマリー
model3 %>% tidy()


# 予測値をデータセットに追加
churn_train3 <- churn_train %>% mutate(prob = Attrition)
churn_train3 <- broom::augment(model3, churn_train3) %>% mutate(.fitted = exp(.fitted))


# プロット作成
churn_train3 %>% 
  ggplot(aes(x = MonthlyIncome, y = prob, color = OverTime)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    ggtitle("Predicted probabilities for model3") +
    xlab("Monthly Income") +
    ylab("Probability of Attrition")




# 4. モデル精度の評価 ----------------------------------------------

# クロスバリデーション ----

# モデル1
# --- Attrition ~ MonthlyIncome
set.seed(123)
cv_model1 <- train(
  Attrition ~ MonthlyIncome, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# モデル2
# ---Attrition ~ MonthlyIncome + OverTime
set.seed(123)
cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime, 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# モデル3
# ---Attrition ~ .
set.seed(123)
cv_model3 <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

# extract out of sample performance measures
summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3
    )
  )
)$statistics


# predict class
pred_class <- cv_model3 %>% predict(churn_train) %>% round()

# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = "Yes"), 
  reference = relevel(churn_train$Attrition, ref = "Yes")
)


library(ROCR)

# Compute predicted probabilities
m1_prob <- predict(cv_model1, churn_train, type = "prob")$Yes
m3_prob <- predict(cv_model3, churn_train, type = "prob")$Yes

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf2 <- prediction(m3_prob, churn_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = "black", lty = 2)
plot(perf2, add = TRUE, col = "blue")


legend(0.8, 0.2, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)



# 5.PLS回帰によるロジスティック回帰 --------------------------

# モデル訓練
# --- method = "pls"
# --- クロスバリデーション
set.seed(123)
cv_model_pls <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 16
)

# Model with lowest RMSE
cv_model_pls$bestTune



# Plot cross-validated RMSE
cv_model_pls %>% ggplot()


# 5.変数重要度 -----------------------------------------

# 重要度プロット
cv_model3 %>% vip(num_features = 20)



# 6.Partial Dependence Plot -----------------------------------------

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


