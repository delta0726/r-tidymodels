# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 10 Bagging
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/7
# URL       : https://bradleyboehmke.github.io/HOML/bagging.html
#           : https://koalaverse.github.io/homlr/notebooks/10-bagging.nb.html
# **********************************************************************************


# ＜ポイント＞
# - バギングはブートストラップ法で訓練データをリサンプリングして複数組成してアンサンブルする方法
#   --- ｢群衆の叡智｣の考え方（簡単な予測を集めることで改善を図る）
#   --- 学習器はそれぞれが独立しているので、並列化による計算にが可能

# - バギングは弱学習器(不安定で分散の多い学習器)に特に適している
#   --- 弱学習器を平均化することでバリアンスを減らす効果を期待する
#   --- 決定木やKNN(k近傍法)などが特に有効
#   --- 結果が安定した学習器でバギングはあまり効果がない

# - 学習器ごとに使用しているレコードは異なるものの、特徴量は全て同じものを使用している
#   --- 同じ特徴量が重視されるモデルもできてしまう（ツリー間の相関を生む）
#   --- ランダムフォレスト(11章)はこの問題を解消したより高度な手法



# ＜目次＞
# 10.1 準備
# 10.2 バギングが機能する理由と時期
# 10.3 バギングの実装
# 10.4 簡単な並列化
# 10.5 特徴量の解釈



# 10.1 準備 ----------------------------------------------


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(rpart)
library(rpart.plot)
library(ipred)
library(caret)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)




# 10.2 バギングが機能する理由と時期 ------------------------

# ＜ポイント＞
# - バギングはバリアンスの大きい弱学習器において有効であることを確認する
#   --- 以下の例では決定木でバギングが効果的に機能していることが確認できる
#   --- バギングは大量のブートストラップを行うので、個別の学習器の計算コストが低い必要がある


# データ準備
set.seed(123)
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>% filter(x < 4.5)


# データ確認
df %>% as_tibble()
df %>% ggplot(aes(x = x, y = y)) + geom_point()



# 10.2.1 多項式回帰 --------------------------------


# 多項式回帰(polynomial model)
# --- リサンプリング(ブートストラップ)
bootstrap_n <- 100
bootstrap_results <- NULL
for(i in seq_len(bootstrap_n)) {

  # リサンプリング
  set.seed(i)
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]

  # モデル学習&予測
  fit <- lm(y ~ I(x^3), data = df_sim)
  df_sim$predictions <- predict(fit, df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}


# 結果確認
# --- 358個のモデルで予測値を取得
bootstrap_results %>% head()
bootstrap_results %>% group_by(model) %>% tally()


# プロット
# --- 元データの散布図
# --- 予測の平均値
p1 <-
  bootstrap_results %>%
    ggplot(aes(x, predictions)) +
    geom_point(data = df, aes(x, y), alpha = .25) +
    geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2) +
    stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "line") +
    scale_y_continuous("Response", limits = c(-2, 2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
    ggtitle("A) Polynomial regression")


# 10.2.2 MARSモデル --------------------------------

# MARSモデル(Multivariate adaptive regression splines)
# --- リサンプリング(ブートストラップ)
bootstrap_n <- 100
bootstrap_results <- NULL
for(i in seq_len(bootstrap_n)) {

  # リサンプリング
  set.seed(i)
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]

  # モデル学習&予測
  fit <- earth::earth(y ~ x, data = df_sim)
  df_sim$predictions <- predict(fit, df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}


# 結果確認
# --- 358個のモデルで予測値を取得
bootstrap_results %>% head()
bootstrap_results %>% group_by(model) %>% tally()


# プロット
# --- 元データの散布図
# --- 予測の平均値
p2 <-
  bootstrap_results %>%
    ggplot(aes(x, predictions)) +
    geom_point(data = df, aes(x, y), alpha = .25) +
    geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2) +
    stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "line") +
    scale_y_continuous(NULL, limits = c(-2, 2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
    ggtitle("B) MARS")



# 10.2.3 決定木 --------------------------------

# 決定木
# --- リサンプリング(ブートストラップ)
bootstrap_n <- 100
bootstrap_results <- NULL
for(i in seq_len(bootstrap_n)) {

  # リサンプリング
  set.seed(i)
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]

  # モデル学習&予測
  fit <- rpart::rpart(y ~ x, data = df_sim)
  df_sim$predictions <- predict(fit, df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}


# 結果確認
# --- 358個のモデルで予測値を取得
bootstrap_results %>% head()
bootstrap_results %>% group_by(model) %>% tally()


# プロット
# --- 元データの散布図
# --- 予測の平均値
p3 <-
  bootstrap_results %>%
    ggplot(aes(x, predictions)) +
    geom_point(data = df, aes(x, y), alpha = .25) +
    geom_line(aes(group = model), show.legend = FALSE, size = .5, alpha = .2) +
    stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "line") +
    scale_y_continuous(NULL, limits = c(-2, 2), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
    ggtitle("C) Decision trees")




# 10.2.4 まとめ --------------------------------

# ＜ポイント＞
# - 多項回帰やMARSモデルはバリアンスが少ない（線のブレが小さい）
# - 決定木はバリアンスが大きい（線のブレが大きい）
# - バギングはバリアンスの大きい決定木で効果的であることが分かる


# 比較
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)





# 10.3 バギングの実装 ----------------------------------------------


# ＜ポイント＞
# - bagging()は{ipred}で提供されており、nbagg引数でブートストラップ回数を設定する
# - ツリーの数は多いほうがよいが、一定水準を超えると分散の減少幅は小さくなりる


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


# 乱数シード
set.seed(123)



# 10.3.1 バギングの実行 ----------------------------

# ＜ポイント＞
# - ipred::bagging()は、{rpart}の決定木をブートストラップ法でバギングしている

# ＜参考＞
# https://www.rdocumentation.org/packages/ipred/versions/0.9-9/topics/bagging


# バギング
# ---
# --- nbagg引数でブートストラップ回数を設定する
ames_bag1 <-
 bagging(formula = Sale_Price ~ .,
         data = ames_train,
         nbagg = 100,
         coob = TRUE,
         control = rpart.control(minsplit = 2, cp = 0)
         )


# 確認
ames_bag1 %>% print()



# 10.3.2 シミュレーション：ツリー数を変更 ----------------------------

# ＜ポイント＞
# - rangerを使って決定木のバギングでシミュレーション
#   --- ipred::bagging()は遅いため


# ツリー数のベクトル
ntree <- seq(1, 200, by = 2)
ntree %>% head()
ntree %>% length()


# RMSEの格納変数
rmse <- vector(mode = "numeric", length = length(ntree))
rmse %>% length()


# シミュレーション
for (i in seq_along(ntree)) {

  set.seed(123)
  model <-
    ranger::ranger(formula = Sale_Price ~ .,
                   data    = ames_train,
                   num.trees = ntree[i],
                   mtry = ncol(ames_train) - 1,
                   min.node.size = 1)

  # RMSEの取得
  rmse[i] <- sqrt(model$prediction.error)
}


# プロットデータ
bagging_errors <- data.frame(ntree, rmse)
bagging_errors %>% as_tibble()


# プロット
# --- ツリー数を増やすことによる精度改善には限界がある
# --- 個別の選定された決定木よりも良好な結果となる
bagging_errors %>%
  ggplot(aes(ntree, rmse)) +
    geom_line() +
    geom_hline(yintercept = 41019, lty = "dashed", color = "grey50") +
    annotate("text", x = 100, y = 41385, label = "Best individual pruned tree", vjust = 0, hjust = 0, color = "grey50") +
    annotate("text", x = 100, y = 26750, label = "Bagged trees", vjust = 0, hjust = 0) +
    ylab("RMSE") +
    xlab("Number of trees")




# 10.3.3 caretによるクロスバリデーション ----------------------------------------------

# バギング + クロスバリデーション
# --- 所要時間：26分
# --- Bagged CART: http://topepo.github.io/caret/train-models-by-tag.html#bagging
ames_bag2 <- 
  train(Sale_Price ~ .,
        data = ames_train,
        method = "treebag",
        #trControl = trainControl(method = "cv", number = 10),
        nbagg = 200,  
        control = rpart.control(minsplit = 2, cp = 0)
        )

ames_bag2




# 10.4 簡単な並列化 -----------------------------------------------

# Create a parallel socket cluster
cl <- makeCluster(8)
registerDoParallel(cl)


# バギングを並列化して実行
predictions <- foreach(
  icount(160), 
  .packages = "rpart", 
  .combine = cbind
) %dopar% {
  # bootstrap copy of training data
  index <- sample(nrow(ames_train), replace = TRUE)
  ames_train_boot <- ames_train[index, ]  
  
  # fit tree to bootstrap copy
  bagged_tree <- rpart(
    Sale_Price ~ ., 
    control = rpart.control(minsplit = 2, cp = 0),
    data = ames_train_boot
  ) 
  
  predict(bagged_tree, newdata = ames_test)
}

predictions[1:5, 1:7]


# Shutdown parallel cluster
stopCluster(cl)






# 10.5 特徴量の解釈 -----------------------------------------------

# 変数重要度分析
ames_bag2 %>% vip::vip(num_features = 40, bar = FALSE)


# 感応度分析
# --- Partial Dependent Plot (PDP)
p1 <-
  ames_bag2 %>%
    pdp::partial(pred.var = "Lot_Area",
                 grid.resolution = 20) %>%
    autoplot()

p2 <-
  ames_bag2 %>%
    pdp::partial(pred.var = "Lot_Frontage",
                 grid.resolution = 20) %>%
    autoplot()

gridExtra::grid.arrange(p1, p2, nrow = 1)