# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 12 Gradient Boosting
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/7
# URL       : https://bradleyboehmke.github.io/HOML/gbm.html
#           : https://koalaverse.github.io/homlr/notebooks/12-gbm.nb.html
#           : https://www.r-bloggers.com/2020/05/using-xgboost-with-tidymodels/
# **********************************************************************************


# ＜ポイント＞
# - 勾配ブースティングマシン(GBM)は浅いツリーを順に作成して弱い個所を重点的に学習する
#   --- 高い予測精度を持つ（kaggleの上位モデルの常連）
#   --- ハイパーパラメータが非常に多い
#   --- 計算量も非常に多い
# - GBMは予測精度はファーストインクラスのアルゴリズムでツールボックスには不可欠な存在



# ＜目次＞
# 12.1 準備
# 12.2 どのように動作するのか
# 12.3 単純なGBM
# 12.4 確率的GBM
# 12.5 XGBoost
# 12.6 特徴量の解釈




# 12.1 準備 ----------------------------------------------

library(tidyverse)
library(tidymodels)
library(broom)
library(magrittr)
library(gbm)
library(xgboost)
library(rpart)
library(vip)
library(h2o)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



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



# 12.2 どのように動作するのか ----------------------------------------------------------

# 12.2.1 連続的なアンサンブル -----------------------------

# ＜ポイント＞
# - 弱い学習器を最初に作って、そのツリーが犯した最大の間違いを修正しながら逐次的に学習器をつなげていく
#   --- 学習器は自由にプラグインすることが可能
#   --- 弱学習器は浅い決定木が使われることが多い（1-6分割程度）
#   --- 次のモデルにエラー残差を学習させることで、残差を修正する学習器を生成する



# データ作成
# --- シミュレーション
set.seed(1112)
df <- tibble::tibble(x = seq(from = 0, to = 2 * pi, length = 1000),
                     y = sin(x) + rnorm(length(x), sd = 0.5),
                     truth = sin(x))


# データ確認
df %>% print()
df %>% ggplot(aes(x = x, y = y)) + geom_point()


# 関数定義
# --- {rpart}の決定木をブーストさせる
rpartBoost <- function(x, y, data, num_trees = 100, learn_rate = 0.1, tree_depth = 6) {

  # 変数定義
  x <- data[[deparse(substitute(x))]]
  y <- data[[deparse(substitute(y))]]
  G_b_hat <- matrix(0, nrow = length(y), ncol = num_trees + 1)
  r <- y


  for(tree in seq_len(num_trees)) {
    # 決定木の作成
    g_b_tilde <- rpart(r ~ x, control = list(cp = 0, maxdepth = tree_depth))

    # ラベルの再定義
    # --- 直前の予測と学習率を用いて
    g_b_hat <- learn_rate * predict(g_b_tilde)
    G_b_hat[, tree + 1] <- G_b_hat[, tree] + matrix(g_b_hat)
    r <- r - g_b_hat
    colnames(G_b_hat) <- paste0("tree_", c(0, seq_len(num_trees)))
  }

  # データ結合
  df %>%
    cbind(as.data.frame(G_b_hat)) %>%
    gather(tree, prediction, starts_with("tree")) %>%
    mutate(tree = stringr::str_extract(tree, "\\d+") %>% as.numeric())
}


# 決定木ブースティングの実行
boost_df <- rpartBoost(x, y, data = df, num_trees = 2^10, learn_rate = 0.05, tree_depth = 1)


# 確認
boost_df %>% as_tibble()
boost_df %>% group_by(tree) %>% tally()


# Plot boosted tree sequence
boost_df %>%
  filter(tree %in% c(0, 2^c(0:10))) %>%
  ggplot(aes(x, prediction)) +
    ylab("y") +
    geom_point(data = df, aes(x, y), alpha = .1) +
    geom_line(data = df, aes(x, truth), color = "blue") +
    geom_line(colour = "red", size = 1) +
    facet_wrap(~ tree, nrow = 3)



# 12.2.2 最急降下法 -----------------------------

# ＜ポイント＞
# - 決定木を含む回帰の多くのアルゴリズムは損失関数を設定して、残差の関数を最小限に抑えることに重点を置いている
#   --- 回帰問題ではSSE、MSE、RMSEなどが用いられる（微分可能な指標であることが重要）


# データ作成
x <- seq(-5, 5, by = .05)
y <- x^2 + 3
df <- data.frame(x, y)


# 確認
df %>% as_tibble()


# ステップ設定
step <- 5
step_size <- 0.2


# 勾配降下法
for(i in seq_len(18)) {
  next_step <- max(step) + round(diff(range(max(step), which.min(df$y))) * step_size, 0)
  step <- c(step, next_step)
  next
}


# データ抽出
# --- ステップごと
steps <-
   df %>%
    slice(step) %>%
     mutate(x2 = lag(x), y2 = lag(y)) %>%
     dplyr::slice(1:18)


# プロット
df %>%
  ggplot(aes(x, y)) +
    geom_line(size = 1.5, alpha = .5) +
    theme_classic() +
    scale_y_continuous("Loss function", limits = c(0, 30)) +
    xlab(expression(theta)) +
    geom_segment(data = df[c(5, which.min(df$y)), ], aes(x = x, y = y, xend = x, yend = -Inf), lty = "dashed") +
    geom_point(data = filter(df, y == min(y)), aes(x, y), size = 4, shape = 21, fill = "yellow") +
    geom_point(data = steps, aes(x, y), size = 3, shape = 21, fill = "blue", alpha = .5) +
    geom_curve(data = steps, aes(x = x, y = y, xend = x2, yend = y2), curvature = 1, lty = "dotted") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = df[5, "x"], y = 1, label = "Initial value", hjust = -0.1, vjust = .8) +
    annotate("text", x = df[which.min(df$y), "x"], y = 1, label = "Minimium", hjust = -0.1, vjust = .8) +
    annotate("text", x = df[5, "x"], y = df[5, "y"], label = "Learning step", hjust = -.8, vjust = 0)




# 12.2.3 学習率の違い -----------------------------

# create too small of a learning rate
step <- 5
step_size <- .05
for(i in seq_len(10)) {
  next_step <- max(step) + round(diff(range(max(step), which.min(df$y))) * step_size, 0)
  step <- c(step, next_step)
  next
}

too_small <-
   df %>%
     dplyr::slice(step) %>%
     mutate(x2 = lag(x),
            y2 = lag(y))



# create too large of a learning rate
too_large <- df[round(which.min(df$y) * (1 + c(-.9, -.6, -.2, .3)), 0), ] %>%
  mutate(x2 = lag(x), y2 = lag(y))
# plot


# 学習率：高
p1 <-
  df %>%
    ggplot(aes(x, y)) +
    geom_line(size = 1.5, alpha = .5) +
    theme_classic() +
    scale_y_continuous("Loss function", limits = c(0, 30)) +
    xlab(expression(theta)) +
    geom_segment(data = too_small[1, ], aes(x = x, y = y, xend = x, yend = -Inf), lty = "dashed") +
    geom_point(data = too_small, aes(x, y), size = 3, shape = 21, fill = "blue", alpha = .5) +
    geom_curve(data = too_small, aes(x = x, y = y, xend = x2, yend = y2), curvature = 1, lty = "dotted") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = df[5, "x"], y = 1, label = "Start", hjust = -0.1, vjust = .8) +
    ggtitle("b) too small")


# 学習率：低
p2 <-
  df %>%
    ggplot(aes(x, y)) +
    geom_line(size = 1.5, alpha = .5) +
    theme_classic() +
    scale_y_continuous("Loss function", limits = c(0, 30)) +
    xlab(expression(theta)) +
    geom_segment(data = too_large[1, ], aes(x = x, y = y, xend = x, yend = -Inf), lty = "dashed") +
    geom_point(data = too_large, aes(x, y), size = 3, shape = 21, fill = "blue", alpha = .5) +
    geom_curve(data = too_large, aes(x = x, y = y, xend = x2, yend = y2), curvature = 1, lty = "dotted") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    annotate("text", x = too_large[1, "x"], y = 1, label = "Start", hjust = -0.1, vjust = .8) +
    ggtitle("a) too big")

# プロット比較
gridExtra::grid.arrange(p2, p1, nrow = 1)



# 12.3 単純なGBM -----------------------------

# ＜ポイント＞
# - ブースティングアルゴリズムには複数のバリエーションがあり、元々は分類問題に焦点を当てられている
# - 1990年代を通じて多くのアプローチが開発され、最も成功したのはAdaBoostアルゴリズムだった
# - 2000年にフリードマンはAdaBoostを重要な統計的概念に関連付けた（損失関数や加法モデリング）
#   ---  これにより、ブースティングフレームワークを回帰問題と複数の損失関数に一般化することができた



# 12.3.1 ハイパーパラメータ -----------------------------

# ＜ポイント＞
# - 単純なGBMのハイパーパラメータは2つのカテゴリを持つ
#   --- ブースティングハイパーパラメータ（Number of trees / Learning rate）
#   --- ツリー固有のハイパーパラメータ（Tree depth / min_n）


# ＜Number of trees＞
# - シーケンスまたはアンサンブル内のツリーの総数
# - ｢GBM｣は、過去のツリーの誤りを修正するために各ツリーが順番に成長するため過剰適合しやすい
#   --- 許可する限りGBMは残差に対して追跡を続ける
#   --- - ｢バギング｣や｢ランダムフォレスト｣は、多くのツリーを生成することで過剰適合を抑制する
#   ｢GBM｣は、他のハイパーパラメータの関係で多くのツリーを要する場合もある
#   --- 交差検証で損失関数を最小化するなどして、最適なツリー数を見つける必要がある


# ＜Learning rate＞
# - 最終的な結果に対する各ツリーの貢献度を決定し、アルゴリズムが勾配降下を進む速度を制御する
#   --- 学習率の範囲は0〜1で、一般的な値は0.001〜0.3
#   --- 値が小さいほど、モデルは個々のツリーの特定の特性に対してロバストになる（過剰適合のリスクを下げる）
# - ツリー数を固定すると最適に達しないリスクを高まる（ハイパーパラメータ収縮）
#   --- ｢ツリー数｣と｢学習率｣の両方を同時に動かす必要がある
#   --- 一般的に学習率を下げるとツリー数は多く必要になる


# ＜Tree depth＞
# - 個々の木の深さを制御
#   --- 典型的な値の範囲は深さ3〜8（整数値）
#   --- 木の深さが1になるのは珍しいことではない
# - 深度の小さいツリーは計算が効率的です（ただし、より多くのツリーが必要）
# - 深度の大きいツリーは相互作用をキャプチャできるものの、過剰適合リスクが高まる


# ＜min_n＞
# - 各ツリーの複雑さを制御
#   --- 典型的な値の範囲は5〜15です
#   --- 値が大きいほど過剰適合を防ぐことができる
#   --- 値が小さいほど分類の問題でターゲットクラスの不均衡を助けることができます。



# 12.3.2 実装 ----------------------------------------

# ＜ポイント＞
# - CRANには多数のGBMの実装が見られるが、{gbm}が最も人気が高い
#   --- gbm::gbm()とgbm::gbm.fit()という2つのトレーニング関数を持つ
#   --- gbm::gbm.fit()は上級者向け（行列で指定、計算効率性が高い）

# ＜参考＞
# - https://www.rdocumentation.org/packages/gbm/versions/1.1-1/topics/gbm


# 単純なGBMの実行
# --- 実行時間：約2分
set.seed(123)
ames_gbm1 <-
  gbm(formula = Sale_Price ~ .,
      data = ames_train,
      distribution = "gaussian",  # SSE loss function
      n.trees = 5000,
      shrinkage = 0.1,
      interaction.depth = 3,
      n.minobsinnode = 10,
      cv.folds = 10
  )


# 確認
ames_gbm1 %>% print()



# CVエラーが最小のRMSE
# ---最初にCVエラーが最小なツリーを探す
best <- ames_gbm1$cv.error %>% which.min()
ames_gbm1$cv.error[best] %>% sqrt()


# プロット
# --- get MSE and compute RMSE
ames_gbm1 %>% gbm.perf(method = "cv")




# 12.3.3 チューニング戦略 ----------------------------------------

# ＜ポイント＞
# - ｢GBM｣はランダムフォレストと異なり、ハイパーパラメータの設定に応じて精度にバラツキが出やすい
#  --- 綿密なチューニング戦略が必要となる


# ＜綿密なチューニング戦略＞
# 1. はじめに比較的高い学習率を選択
#    - デフォルト値の0.1でよい（0.05〜0.2くらいで幅広い問題に対応可能）
# 2. 1で指定した学習率に最適なツリーの数を決定する
# 3. ツリーのハイパーパラメーターを修正し、学習率を調整して、速度とパフォーマンスを比較します。
# 4. ツリー固有のパラメーターを調整(tree_depth / min_n)
# 5. 4でツリー固有のパラメーターが見つかったら、学習率を下げて精度の向上を評価する
# 6. 最終的なハイパーパラメータ設定を使用して交差検証を実施



## ステップ3 : 学習率の調整

# グリッドの設定
hyper_grid <- expand.grid(
  learning_rate = c(0.3, 0.1, 0.05, 0.01, 0.005),
  RMSE = NA,
  trees = NA,
  time = NA
)

# グリッドサーチの実行
# --- 実行時間：約10分
# --- GBM1回あたり2分 * 5回
for(i in seq_len(nrow(hyper_grid))) {

  # fit gbm
  set.seed(123)  # for reproducibility
  train_time <- system.time({
    m <- gbm(
      formula = Sale_Price ~ .,
      data = ames_train,
      distribution = "gaussian",
      n.trees = 5000,
      shrinkage = hyper_grid$learning_rate[i],
      interaction.depth = 3,
      n.minobsinnode = 10,
      cv.folds = 10
   )
  })

  # add SSE, trees, and training time to results
  hyper_grid$RMSE[i]  <- sqrt(min(m$cv.error))
  hyper_grid$trees[i] <- which.min(m$cv.error)
  hyper_grid$Time[i]  <- train_time[["elapsed"]]

}

# results
arrange(hyper_grid, RMSE)



## ステップ4 ： ツリー固有のパラメーターを調整

# グリッドの設定
hyper_grid <- expand.grid(
  n.trees = 6000,
  shrinkage = 0.01,
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 10, 15)
)


# グリッドサーチの実行
# --- 実行時間：約10分
# --- GBM1回あたり2分 * 5回
model_fit <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode) {
  set.seed(123)
  m <- gbm(
    formula = Sale_Price ~ .,
    data = ames_train,
    distribution = "gaussian",
    n.trees = n.trees,
    shrinkage = shrinkage,
    interaction.depth = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    cv.folds = 10
  )
  # compute RMSE
  sqrt(min(m$cv.error))
}

# perform search grid with functional programming
hyper_grid$rmse <- purrr::pmap_dbl(
  hyper_grid,
  ~ model_fit(
    n.trees = ..1,
    shrinkage = ..2,
    interaction.depth = ..3,
    n.minobsinnode = ..4
    )
)

# results
arrange(hyper_grid, rmse)




# 12.4 確率的GBM -----------------------------

# ＜各ライブラリの実装＞
# - 各ツリーを作成する前に行をサブサンプル
#   --- （gbm、h2o、およびxgboostで使用可能）
# - 各ツリーを作成する前に列をサブサンプル
#   --- （h2o＆xgboost）
# - 各ツリーの各分割を検討する前に列をサブサンプルします
#   --- （h2o＆xgboost）


# ＜ポイント＞
# - 行の積極的なサブサンプリングは有益であることが示されている
#   --- 一般的な値は0.5〜0.8の範囲



# ＜ポイント＞
# - トレーニングデータセットのランダムサブサンプルでアルゴリズムをトレーニングすることで予測精度が向上する
#   --- ツリーの相関関係がさらに減少
#   --- ｢確率的勾配ブースティング(GBM)｣として実装されている

# グリッドの設定
hyper_grid <- list(
  sample_rate = c(0.5, 0.75, 1),              # row subsampling
  col_sample_rate = c(0.5, 0.75, 1),          # col subsampling for each split
  col_sample_rate_per_tree = c(0.5, 0.75, 1)  # col subsampling for each tree
)

# H2Oの設定
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,
  stopping_rounds = 10,
  max_runtime_secs = 60*60
)

# グリッドサーチの実行
# --- 実行時間：約60分
grid <-
  h2o.grid(algorithm = "gbm",
           grid_id = "gbm_grid",
           x = predictors,
           y = response,
           training_frame = train_h2o,
           hyper_params = hyper_grid,
           ntrees = 6000,
           learn_rate = 0.01,
           max_depth = 7,
           min_rows = 5,
           nfolds = 10,
           stopping_rounds = 10,
           stopping_tolerance = 0,
           search_criteria = search_criteria,
           seed = 123
  )

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "gbm_grid",
  sort_by = "mse",
  decreasing = FALSE
)

grid_perf


# Grab the model_id for the top model, chosen by cross validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s get performance metrics on the best model
h2o.performance(model = best_model, xval = TRUE)





# 12.5 XGBoost -----------------------------

# ＜ポイント＞
# - XGBoostは、従来のブーストに比べていくつかの利点を提供持っている
#   - 正則化  ：XGBoostは、追加の正則化ハイパーパラメーターを提供
#   - 並列処理：勾配ブースティングは本質的に並列不可だが、GPUを使い可能にしている
#   - 損失関数：ーザーはカスタムの目的と評価基準を使用して勾配ブースティングモデルを定義および最適化できる
#   - ベース学習器：一般化線形モデルにも対応（あまり使わない）


# ＜参考＞
# - https://xgboost.readthedocs.io/en/latest/


# ＜正則化＞
# - モデルの複雑さを軽減し、過剰適合を防ぐのに役立つ複数の正則化パラメーターを提供する
#   --- 正則化パラメータには｢gamma｣｢alpha/lambda｣がある
#   --- モデルの複雑さを抑制し、過剰適合を減らすように機能する
# - gammaは特定のツリーの複雑さを制御する
#   --- ツリーのリーフノードにさらにパーティションを作成するために必要な最小損失削減を指定
#   --- 最大深度までツリーを成長させた後、指定されたガンマを満たさない分割を見つけて削除してツリーを整理
#   --- トレインとテストのCVエラーの間に有意差が見られる場合に使用する（0～無限大、一般的に1-20）
# - alpha/lambdaは従来の正則化を行う
#   --- alphaはL1正則化、lambdaはL2正則化


# ＜ドロップアウト＞
# - ディープニューラルネットワークの過剰適合を防ぐためにディープラーニングで広く採用されている
#   ---GBMの過剰適合に対処するためにも使用できます
# - ブースティングでは最初の数本のツリーがモデルのパフォーマンスを支配する傾向になる
#   --- 後で追加されたツリーは、特徴空間の小さなサブセットのみの予測を改善するだけ
#   --- ドロップアウトは、ブーストシーケンスでランダムにツリーをドロップすることで過学習を抑制する
#   --- ｢DART｣ブースターは、ドロップアウト加法回帰ツリーのことを指す


library(recipes)

xgb_prep <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_integer(all_nominal()) %>%
    prep(training = ames_train, retain = TRUE) %>%
    juice()

X <- as.matrix(xgb_prep[setdiff(names(xgb_prep), "Sale_Price")])
Y <- xgb_prep$Sale_Price


set.seed(123)
ames_xgb <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "reg:linear",
  early_stopping_rounds = 50,
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)

# minimum test CV RMSE
min(ames_xgb$evaluation_log$test_rmse_mean)

# hyperparameter grid
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  rmse = 0,          # a place to dump RMSE results
  trees = 0          # a place to dump required number of trees
)

# grid search
for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "reg:linear",
    early_stopping_rounds = 50,
    nfold = 10,
    verbose = 0,
    params = list(
      eta = hyper_grid$eta[i],
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i],
      lambda = hyper_grid$lambda[i],
      alpha = hyper_grid$alpha[i]
    )
  )
  hyper_grid$rmse[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

# results
hyper_grid %>%
  filter(rmse > 0) %>%
  arrange(rmse) %>%
  glimpse()


# 12.6 特徴量の解釈 -----------------------------

# variable importance plot
vip::vip(xgb.fit.final)



