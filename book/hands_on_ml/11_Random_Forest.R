# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 11 Random Forests
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/2
# URL       : https://bradleyboehmke.github.io/HOML/random-forest.html
#           : https://koalaverse.github.io/homlr/notebooks/11-random-forests.nb.html
# **********************************************************************************


# ＜ポイント＞
# - ランダムフォレストはバギング決定木を修正したアルゴリズム
#   --- 相関関係のないツリーを大規模に作成してパフォーマンス改善を目指している
#   --- 比較的少ないパラメータチューニングで良好なパフォーマンスを実現

# -ランダムフォレストはデータセットと特徴量をそれぞれブートストラップサンプリングして多様なツリーを作成
#  --- 特徴量のサンプリングでツリー間の不安定性解消や相関低下を実現（バギングはすべての変数を用いる）
#  --- バギングを超えたツリー間の相関係数の低下を実現（予測力が大幅に改善する可能性）

# - 特徴量のサンプリングで計算速度が改善するが、サーチスペースを増やすと計算量が増加
#   --- {ranger}や{h2o}では並列化を実装



# ＜目次＞
# 11.1 準備
# 11.2 バギングの拡張
# 11.3 すぐに使えるパフォーマンス
# 11.4 ハイパーパラメータ
# 11.5 チューニング戦略
# 11.6 特徴量の重要度分析




# 11.1 準備 ----------------------------------------------

library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(ranger)
library(h2o)
library(vip)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)
library(ggRandomForests)



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



# 11.2 バギングの拡張 ----------------------------------------------

# ＜ポイント＞
# - ツリー成長プロセスにより多くのランダム性を注入することによりツリー相関を減らす
#   --- 特徴量のサンプリング
#   --- ハイパーパラメータ(mtry)としてインプット



# ＜処理プロセス＞
# 1.トレーニングデータを与える
# 2.構築するツリーの数を選択する（n_trees）
# 3.  | for i = 1 to n_trees
# 4.  |  - トレーニングデータのブートストラップサンプルを生成
# 5.  |  - ブートストラップされたデータに回帰/分類ツリーを成長させる
# 6.  |  | for each split do
# 7.  |  |  - すべての変数(p個)からmtryの変数をランダムに選択
# 8.  |  |  - mtryの変数の中から最適な変数/分割点を選択する
# 9.  |  |  - ノードを2つの子ノードに分割します
# 10. |  | end
# 11. | - 典型的なツリーモデルの停止基準を使用して、
#     | - ツリーは完全です（ただし、剪定しないでください）
# 12. | 終了
# 13.ツリーのアンサンブルを出力する




# 11.3 すぐに使えるパフォーマンス ----------------------------------------------

# 特徴量の一覧
ames_train %>% names()


# 特徴量の数
n_features <- ames_train %>% names() %>% setdiff("Sale_Price") %>% length()
n_features %>% print()


# モデル構築
# --- mtry = floor(n_features / 3)
# --- respect.unordered.factors = "order" （順序付けられていない因子変数の処理方法を指定）
ames_rf1 <-
  ranger(Sale_Price ~ .,
         data = ames_train,
         mtry = floor(n_features / 3),
         respect.unordered.factors = "order",
         seed = 123
         )




# 確認
ames_rf1 %>% print()


# RMSE
default_rmse <- ames_rf1$prediction.error %>% sqrt()
default_rmse %>% print()



# 11.4 ハイパーパラメータ ----------------------------------------------

# ＜ハイパーパラメータ＞
# 1.木の数（num.trees / trees）
# 2.特定の分割で検討する機能の数（mtry）
# 3.各ツリーの複雑さ（min.node.size / max.depth）
# 4.サンプリング方式
# 5.ツリー構築中に使用する分割ルール



# 11.4.1 木の数 -------------------------

# - エラー率を安定させるのに十分な数である必要がある
#   --- 特徴量の数の10倍から開始してみることが推奨されている
#   --- 木の数を増やすことで、よりロバストで安定した結果が期待できる
#   --- 計算時間への影響は、ツリーの数に比例して増加


# - 変数重要度測定にも影響を及ぼす
# - 厳密にはハイパーパラメータではない、


# 特徴量の数
n_features <- ncol(ames_train) - 1


# チューニング・グリッド
#  --- treesを変化させる
tuning_grid <- expand.grid(
  trees = seq(10, 1000, by = 20),
  rmse  = NA
)


# チューニング
for(i in seq_len(nrow(tuning_grid))) {

  # ランダムフォレストの実行
  fit <- ranger(
    formula = Sale_Price ~ .,
    data = ames_train,
    num.trees = tuning_grid$trees[i],
    mtry = floor(n_features / 3),
    respect.unordered.factors = 'order',
    verbose = FALSE,
    seed = 123
  )

  # RMSEの計算
  tuning_grid$rmse[i] <- sqrt(fit$prediction.error)

}


# プロット
# --- ツリーの数は一定水準を超えると予測精度に影響を与えなくなる
tuning_grid %>%
  ggplot(aes(trees, rmse)) +
    geom_line(size = 1) +
    labs(title = "Relation between trees and RMSE",
         y = "OOB Error (RMSE)",
         x = "Number of trees")




# 11.4.2 mtry -------------------------

# - 分割変数のランダム化を制御するハイパーパラメーター
#   --- 低ツリー相関と妥当な予測強度のバランスをとるのに役立つ
#   --- 回帰問題の場合はp/3、分類問題の場合はSQRT(p)、あたりが目安となる
#   --- 最も影響力のあるパラメータ

# - mtryの数は｢バイアス｣と｢バリアンス｣の関係に影響を与える
#   --- 特徴量が少ない場合： mtryの値が高いほどバイアスが小さくなる傾向がある
#   --- 特徴量が多い場合  ： mtryの値が低いほどバリアンスが小さくなる傾向がある（汎化性能の改善）


# p: 特徴量数


# チューニンググリッドの作成
# --- 特徴量2-80(最大)
tuning_grid <- expand.grid(
  trees = seq(10, 1000, by = 20),
  mtry  = floor(c(seq(2, 80, length.out = 5), 26)),
  rmse  = NA
)

# チューニング数
tuning_grid %>% nrow()


# グリッドサーチ
# --- 実行時間：2分程度
for(i in seq_len(nrow(tuning_grid))) {

  # ランダムフォレストの実行
  fit <- ranger(
    formula    = Sale_Price ~ .,
    data       = ames_train,
    num.trees  = tuning_grid$trees[i],
    mtry       = tuning_grid$mtry[i],
    respect.unordered.factors = 'order',
    verbose    = FALSE,
    seed       = 123
  )

  # RMSEの計算
  tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
}


# ラベル作成
labels <-
  tuning_grid %>%
    filter(trees == 990) %>%
    mutate(mtry = as.factor(mtry))


# プロット
tuning_grid %>%
  mutate(mtry = as.factor(mtry)) %>%
  ggplot(aes(trees, rmse, color = mtry)) +
  geom_line(size = 1, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = labels, aes(trees, rmse, label = mtry),
                           nudge_x = 50, show.legend = FALSE) +
    labs(title = "Relation between mtry and RMSE",
         y = "OOB Error (RMSE)",
         x = "Number of trees")




# 11.4.3 ツリーの複雑さ -------------------------

# - ランダムフォレストは決定木に基づいて構成されるので、決定木のハイパーパラメータもコントロールすることができる
#   --- node size / max depth / max number of terminal nodes / the required node size

# - ノードサイズはツリーの複雑さを制御するための最も一般的なハイパーパラメータ
#   --- 分割されたリーフノードに最低限含まれていなければならないレコード数を指定
#   --- ほとんどの実装は分類に1、回帰に5のデフォルト値を使用（min.node.size）
#   --- ノードサイズを大きくすることで実行時間を大幅に短縮できる
#   --- エラーの見積もりにほとんど影響を与えない（アーリーストッピングがかかるため）


# チューニンググリッドの作成
  tuning_grid <- expand.grid(
    min.node.size = 1:20,
    run_time  = NA,
    rmse = NA
  )


# 確認
tuning_grid %>% as_tibble()
tuning_grid %>% nrow()


# グリッドサーチ
# --- ノードサイズ: min.node.size
for(i in seq_len(nrow(tuning_grid))) {
  fit_time <- system.time({
    # モデル構築
    fit <- ranger(
      formula    = Sale_Price ~ .,
      data       = ames_train,
      num.trees  = 1000,
      mtry       = 26,
      min.node.size = tuning_grid$min.node.size[i],
      respect.unordered.factors = 'order',
      verbose    = FALSE,
      seed       = 123
    )
  })

  tuning_grid$run_time[i] <- fit_time[[3]]
  tuning_grid$rmse[i] <- sqrt(fit$prediction.error)

}


# 集計
min_node_size <-
  tuning_grid %>%
    mutate(error_first = first(rmse),
           runtime_first = first(run_time),
           `Error Growth` = (rmse / error_first) - 1,
           `Run Time Reduction` = (run_time / runtime_first) - 1)


# プロット作成
p1 <-
  min_node_size %>%
    ggplot(aes(min.node.size, `Error Growth`)) +
    geom_smooth(size = 1, se = FALSE, color = "black") +
    scale_y_continuous("Percent growth in error estimate", labels = scales::percent) +
    xlab("Minimum node size") +
    ggtitle("A) Impact to error estimate")

p2 <-
  min_node_size %>%
    ggplot(aes(min.node.size, `Run Time Reduction`)) +
    geom_smooth(size = 1, se = FALSE, color = "black") +
    scale_y_continuous("Reduction in run time", labels = scales::percent) +
    xlab("Minimum node size") +
    ggtitle("B) Impact to run time")

gridExtra::grid.arrange(p1, p2, nrow = 1)
  xlab("Number of trees")




# 11.4.4 サンプリング方式 -------------------------

# ＜sample.fraction＞
# - ランダムフォレストの既定のサンプリングスキームはブートストラップ
#   --- 観測値の100％が置換ありのリサンプリングで作成される
#   --- min.node.sizeは観測値の数をコントロールする
#   --- サンプルサイズを小さくすることで、ツリーの多様性が高まってツリー間の相関関係を低下させることができる


  # チューニンググリッドの作成
  tuning_grid <- expand.grid(
    sample.fraction = seq(.05, .95, by = .05),
    replace  = c(TRUE, FALSE),
    rmse = NA
  )


  # 確認
  tuning_grid %>% as_tibble()
  tuning_grid %>% nrow()


  # グリッドサーチ
  # --- sample.fraction
  # ---replace
  for(i in seq_len(nrow(tuning_grid))) {
    fit <- ranger(
      formula    = Sale_Price ~ .,
      data       = ames_train,
      num.trees  = 1000,
      mtry       = 26,
      sample.fraction = tuning_grid$sample.fraction[i],
      replace = tuning_grid$replace[i],
      respect.unordered.factors = 'order',
      verbose    = FALSE,
      seed       = 123
    )

    tuning_grid$rmse[i] <- sqrt(fit$prediction.error)

  }

  # プロット
  tuning_grid %>%
    ggplot(aes(sample.fraction, rmse, color = replace)) +
    geom_line(size = 1) +
    scale_x_continuous("Sample Fraction", breaks = seq(.1, .9, by = .1), labels = scales::percent) +
    ylab("OOB Error (RMSE)") +
    scale_color_discrete("Sample with Replacement") +
    theme(legend.position = c(0.8, 0.85),
          legend.key = element_blank(),
          legend.background = element_blank())


# 11.4.5 分割ルール -------------------------


# - splitrule引数で指定
#   --- 回帰：SSE
#   --- 分類：Gini不純度(Gini impurity)





# 11.5 チューニング戦略 ----------------------------------------------

# ******* ranger *********

# ＜ポイント＞
# - 複数のハイパーパラメータを持つアルゴリズムを使う際には、ハイパーパラメータのチューニングが必要
# --- ranger自体はグリッドサーチの仕組みを持たない
# --- expand.grid()でパターンを作成してループで逐次処理を行う


  # チューニンググリッドの設定
  # --- トライアルのパターンをデータフレームにしておく
  hyper_grid <-
    expand.grid(mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
                min.node.size = c(1, 3, 5, 10),
                replace = c(TRUE, FALSE),
                sample.fraction = c(.5, .63, .8),
                rmse = NA)


  # 確認
  hyper_grid %>% as_tibble()
  hyper_grid %>% dim()
  
  
  # グリッドサーチ
  # --- RMSEを計算
  for(i in seq_len(nrow(hyper_grid))) {
    # モデル構築
    # --- ハイパーパラメターを適用
    fit <- ranger(
      formula         = Sale_Price ~ ., 
      data            = ames_train, 
      num.trees       = n_features * 10,
      mtry            = hyper_grid$mtry[i],
      min.node.size   = hyper_grid$min.node.size[i],
      replace         = hyper_grid$replace[i],
      sample.fraction = hyper_grid$sample.fraction[i],
      verbose         = FALSE,
      seed            = 123,
      respect.unordered.factors = 'order',
    )
    # RMSEの計算
    hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
  }
  
  # assess top 10 models
  hyper_grid %>%
    arrange(rmse) %>%
    mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
    head(10)


  # プロット
  hyper_grid$rmse %>% sort() %>% plot()



# ******* h2o *********

# ＜ポイント＞
#・H2Oはデカルトサーチに加えて｢ランダムグリッドサーチ｣や｢アーリーストッピング｣ができる
#  --- チューニングの計算コストの改善に寄与


# H2O起動
h2o.no_progress()
h2o.init(max_mem_size = "5g")


# H2Oオブジェクトに変換
train_h2o <- ames_train %>% as.h2o()


# YとXの列名を定義
response   <- "Sale_Price"
predictors <- ames_train %>% colnames() %>% setdiff(response)


# モデル構築
# --- n_feature = 80
# --- n_tree = 800
h2o_rf1 <-
  h2o.randomForest(x = predictors,
                   y = response,
                   training_frame = train_h2o,
                   ntrees = n_features * 10,
                   seed = 123
                   )

# 確認
h2o_rf1 %>% print()


# チューニンググリッドの設定
hyper_grid <- list(
  mtries = floor(n_features * c(.05, .15, .25, .333, .4)),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)


# パターン数
# --- 240パターン
hyper_grid %>% expand.grid() %>% dim()


# サーチ条件の設定
# --- アーリーストッピング
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 300
)


# グリッドサーチ
# --- DRF専用の関数ではない
# --- デカルトグリッドとランダムグリッドの両方をサポート
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors,
  y = response,
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = n_features * 10,
  seed = 123,
  stopping_metric = "RMSE",
  stopping_rounds = 10,           # stop if last 10 trees added
  stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)


# グリッドサーチの結果取得
random_grid_perf <- h2o.getGrid(
  grid_id = "rf_random_grid",
  sort_by = "mse",
  decreasing = FALSE
)


# 確認
result <- random_grid_perf@summary_table %>% as.data.frame()
result %>% print()
result %>% nrow()


  
# 11.6 特徴量の重要度分析 -----------------------------------------------
  
# ＜ポイント＞
# - ｢ジニ不純度｣による変数重要度
# --- 特定のフィーチャの損失関数の平均合計削減に基づいてフィーチャの重要度を決定

# - ｢順列ベース｣による変数重要度
#   --- ベースツリーの予測精度に対して、各変数の値がランダムに並べ替えられ精度が再度計算する
#   --- このランダムな特徴値のシャッフルの結果としての精度の低下は、各予測子のすべてのツリーで平均化されます。
#   --- 精度の平均低下が最大の変数が最も重要であると見なされます。



# モデル構築
# --- 重要度を出力するように設定
# --- importance = "impurity"（ジニ不純度）
rf_impurity <- ranger(
  formula = Sale_Price ~ .,
  data = ames_train,
  num.trees = 2000,
  mtry = 32,
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)

# モデル構築
# --- 重要度を出力するように設定
# --- importance = "permutation"（順列法）
rf_permutation <- ranger(
  formula = Sale_Price ~ .,
  data = ames_train,
  num.trees = 2000,
  mtry = 32,
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "permutation",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)


# 変数重要度の取得
p1 <- rf_impurity %>% vip::vip(num_features = 25, bar = FALSE)
p2 <- rf_permutation %>% vip::vip(num_features = 25, bar = FALSE)

# プロット
gridExtra::grid.arrange(p1, p2, nrow = 1)
