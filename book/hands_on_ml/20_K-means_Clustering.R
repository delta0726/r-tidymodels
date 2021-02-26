# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 3 Feature & Target Engineering
# Title     : Chapter 20 K-means Clustering
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/7
# URL       : https://bradleyboehmke.github.io/HOML/kmeans.html
#           : https://koalaverse.github.io/homlr/notebooks/20-kmeans.nb.html
# **********************************************************************************


# ＜ポイント＞
# - クラスタリングは、非類似度を計測したうえで類似したものをクラス分類する手法の総称
#   --- クラスタリングは観測空間(n)を圧縮している
#   --- PCAは特徴量空間(p)を圧縮している
# - K-meansは、観測値をkグループのセットに分割するために最も一般的に使用されるアルゴリズムの1つ
#   --- 観測値を相互に排他的なグループに分類しようとする
#   --- 各クラスターは割り当てられた観測値の平均に対応する中心(重心)で表される
# - クラスタリングは大規模データになると以下のような問題が発生する
#   --- カテゴリカルデータの処理： cluster::pam()
#   --- 大規模データの処理： cluster::clara()



# ＜目次＞
# 20-1. 準備
# 20-2. 距離尺度
# 20-3. クラスターの定義
# 20-4. k-menasアルゴリズム
# 20-5. MNISTのクラスタリング
# 20-6. クラスターの数
# 20-7. 混合データのクラスタリング
# 20.8. その他の分類メソッド



# 20-1. 準備 ----------------------------------------------------------


library(tidyverse)
library(tidyquant)
library(magrittr)
library(stringr)
library(Hmisc)
library(kableExtra)
library(cluster)
library(factoextra)


# カレントディレクトリ設定
setwd("I:/Project/R/tidymodels/book/ml_hans_on")


# データ取得1
# --- MnistDataset
mnist <- dslabs::read_mnist()
mnist %>% glimpse()


# データ取得2
# --- my_basket
my_basket <- read_csv("data/my_basket.csv")
my_basket %>% glimpse()




# 20-2. 距離尺度 ----------------------------------------------------------

# ＜ポイント＞
# - 観測値をグループ分類するには、距離/非類似度を計算するための何らかの方法が必要
#   --- KNNと同様
# - 距離尺度として以下のようなものが挙げれる
#   --- 古典的な方法はユークリッド距離とマンハッタン距離である
#   --- 遺伝子発現データに広く使用されている相関ベースの距離
#   --- Gower距離測定など代替の距離測度
#   --- テキストマイニングの分野ではコサイン距離
# - ユークリッド距離(直線距離)は外れ値に非常に敏感
#   --- 特徴量がガウス分布に従う場合はユークリッド距離を使用するのが妥当
#   --- 外れ値に対してより堅牢にしたい場合は、マンハッタン距離/ミンコフスキー距離
# - 相関ベースの距離がは特徴量がスケーリングされていない場合に推奨される



# 20-3. クラスターの定義 ----------------------------------------------------------

# ＜ポイント＞
# - k-meansの背後にある基本的な考え方は、クラスター内の変動全体が最小化されるようにクラスターを構築すること
# - 標準アルゴリズムはHartigan-Wongであり、クラスター内の変動の合計を観測間のユークリッド距離の合計として定義



# 関数定義
# --- データ作成
create_data <- function(sd) {
  tibble(
    x1 = c(rnorm(100, sd = sd), rnorm(100, sd = sd) + 3),
    x2 = c(rnorm(100, sd = sd), rnorm(100, sd = sd) - 2)
  ) %>%
    mutate(`W(Ck)` = case_when(
      sd == 0.5  ~ "Best",
      sd == 0.75 ~ "Better",
      sd == 1   ~ "Good"
    ))
}


# データ作成
# --- 3つのリストにデータフレームを作成
# --- 標準偏差の大きさ
# --- Good=1  Better=0.75  Best=0.5
df <- c(0.5, 0.75, 1) %>% map(create_data)



# k-meansの実行
# --- リストごとに適用
# --- 2つのクラスターを作成
k2 <- df %>% map(~ kmeans(.x[, 1:2], 2, nstart = 20))
k2 %>% glimpse()


# プロット用データ
# --- クラスターとセンターを追加
df <-
  df %>%
    map2(k2, ~ mutate(.x, cluster = .y$cluster)) %>%
    map2_dfr(k2, ~ inner_join(.x, .y$centers %>%
                          as.data.frame() %>%
                          mutate(cluster = row_number()), by = "cluster")) %>%
  rename(x1 = x1.x, x2 = x2.x, x_center = x1.y, y_center = x2.y) %>%
  mutate(`W(Ck)` = factor(`W(Ck)`, levels = c("Good", "Better", "Best")))

# プロット作成
df %>%
  ggplot(aes(colour = factor(cluster))) +
  facet_wrap(~ `W(Ck)`) +
  geom_segment(aes(x = x1, xend = x_center, y = x2, yend = y_center), lty = "dashed", alpha = .5) +
  geom_point(aes(x_center, y_center), size = 4) +
  geom_point(aes(x1, x2), show.legend = FALSE, alpha = .5) +
  scale_x_continuous(bquote(X[1]), breaks = NULL, labels = NULL) +
  scale_y_continuous(bquote(X[2]), breaks = NULL, labels = NULL) +
  theme(legend.position = "none")




# ＜ポイント＞
# - k-meansは凸状の境界を必要とするため、クラスターの形状が複雑な場合、この仮定は効果がない可能性がある
# - 標準アルゴリズムはHartigan-Wongであり、クラスター内の変動の合計を観測間のユークリッド距離の合計として定義
# - 以下の例ではスパイラル型のデータセットを用いてクラスタリングを適用
#   --- k-meansの前提が成立しないためではうまく分類できない
#   --- スペクトラムクラスタリングはカーネルトリックを用いて非凸型のデータを分類する
#   --- 22章のモデルベースクラスタリングでも分類可能


# データ作成1
# --- Original spiral Data
set.seed(111)
obj <- mlbench::mlbench.spirals(200, 1, 0.025)
df <- data.frame(
  x = obj$x[, 1],
  y = obj$x[, 2],
  class = obj$classes
)

# データ確認
df %>% tibble()

# プロット作成
p1 <-
  df %>%
    ggplot(aes(x, y)) +
    geom_point() +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle('(A) Original spiral data')


# データ作成
# --- k-meansの実行
kmeans_on_spiral <- df[, 1:2] %>% kmeans(centers = 2)
df_kmeans <- df %>% mutate(kmeans_clusters = kmeans_on_spiral$cluster)


# プロット作成
p2 <-
  df_kmeans %>%
    ggplot(aes(x, y, color = kmeans_clusters)) +
    geom_point(show.legend = FALSE) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle('(B) k-means clusters')


# データ作成
# --- カーネルスペクトラムを実行
sc <- df[, 1:2] %>% as.matrix() %>% kernlab::specc(centers = 2)
df_specc <- df %>% mutate(spec_clusters = sc@.Data)

# プロット作成
p3 <-
  df %>%
    ggplot(aes(x, y, color = spec_clusters)) +
    geom_point(show.legend = FALSE) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle('(C) Spectral clusters')

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)




# 20-4. k-menasアルゴリズム ----------------------------------------------------------

# ＜ポイント＞
# k-meansアルゴリズムは、次のように要約できる
# 1. 作成するクラスターの数(k)を指定する（アナリスト）
# 2. データセットからランダムにk個の観測値を選択して、初期クラスター重心として使用して割り当てます
# 3. 選択した距離測定に基づいて、最も近い重心まで観測します.
# 4. k個のクラスターのそれぞれについて、クラスター内の新しい平均値を計算することによりクラスターの重心を更新
# 5. i番目のクラスターの重心は、クラスターiの観測値のすべてのp個の特徴の平均を含む長さpのベクトルです。
# 6. 繰り返し、SSwithinを最小化します。
# 7. クラスター割り当ての変更が停止または最大反復回数に達するまで、ステップ3〜4を繰り返します。
#     経験則として、10〜20回の反復を実行することです。



# データ作成
df <- tibble(
    x1 = c(rnorm(100), rnorm(100) + 3),
    x2 = c(rnorm(100), rnorm(100) - 2)
)


# データ確認
df %>% print()


# クラスタリング作成
# --- 同じデータセットにk-meansを6パターン適用
df_keans <-
  map(1:6, ~ kmeans(df, 3)) %>%
    map2_dfr(1:6, ~ df %>% mutate(
      cluster = .x$cluster,
      name = paste0("Iteration: ", .y, ";  W(Ck): ", round(.x$tot.withinss, 2))
      ))


# プロット作成
# --- 開始点が異なると結果が異なる
# --- 回数を重ねるごとに集約されていく
df_keans %>%
  ggplot(aes(x1, x2, colour = cluster)) +
  geom_point(show.legend = FALSE, size = 1) +
  facet_wrap(~ name, nrow = 2)



# 20-5. MNISTのクラスタリング ------------------------------------------------------------

# ＜ポイント＞
# - MNISTピクセルフィーチャでk-meansクラスタリングを実行して例とした分析
#   --- 応答変数を使用せずに数字の一意のクラスターを識別できるかどうかを確認する
# - データに10個の一意の数字が含まれていることがすでにわかっているためk=10とする


# データ準備
features <- mnist$train$images


# k-meansの適用
# --- 実行時間：4-5分程度
mnist_clustering <- features %>% kmeans(centers = 10, nstart = 10)
mnist_clustering %>% glimpse()


# クラスタリングのセンターを抽出
mnist_centers <- mnist_clustering$centers
mnist_centers %>% dim()
mnist_centers %>% head()


# プロット作成
par(mfrow = c(2, 5), mar = c(0.5, 0.5, 0.5, 0.5))
layout(matrix(seq_len(nrow(mnist_centers)), 2, 5, byrow = FALSE))
for (i in seq_len(nrow(mnist_centers))) {
  image(matrix(mnist_centers[i, ], 28, 28)[, 28:1],
        col = gray.colors(12, rev = TRUE), xaxt = "n", yaxt = "n")
}


# Create mode function
mode_fun <- function(x){
  which.max(tabulate(x))
}

mnist_comparison <-
  data.frame(cluster = mnist_clustering$cluster,
             actual = mnist$train$labels) %>%
  group_by(cluster) %>%
  mutate(mode = mode_fun(actual)) %>%
  ungroup() %>%
  mutate_all(factor, levels = 0:9)


# 混合行列の作成
yardstick::conf_mat(
  mnist_comparison,
  truth = actual,
  estimate = mode
) %>%
  autoplot(type = 'heatmap')




# 20-6. クラスターの数 ------------------------------------------------------------

# ＜課題＞
# - k-meansのクラスター数は事前知識がない場合にはどのように決定するのか
#   --- クラスターの数を選択するには、微妙なバランスが必要
#   --- kの値を大きくすると、クラスターの均一性が向上する一方、過剰適合のリスクがあります。


# ＜ポイント＞
# - 経験則として一般的に使用される値はk=Sqrt(n/2)です
#   --- この方法だと、巨大データセットの場合にはクラスター数が多くなってしまう
#   --- MNISTデータセットだとk=173と大きくなってしまう（次元の呪い）
# - {nbclust}はクラスターパフォーマンスの測定値の多くを実装してkを探すサポートをしてくれる
#   --- {factoextra}は{nbclust}をラップして美しい各種チャートを作成してくれる
# - 最も一般的な方法の1つはエルボー法がある
#   --- クラスター内の変動合計が最小になるようにクラスターを定義する
#   --- kを1-20などの間で総平方和(WSS)を計算してプロットする
#   --- 大規模データセットの場合forループなどを併用して調べる必要がある
#   --- 仮に、正確なクラスタ数が分からなくても、適度に近いクラスタ数であれば十分要件を満たすタスクが多い



# プロット作成
# --- method = 'wss' : エルボー法
# --- 下落がなだらかになる直前の箇所を選択する(k=5)
my_basket %>%
  fviz_nbclust(kmeans,
               k.max = 25,
               method = "wss",
               diss = get_dist(my_basket, method = "spearman"))



# 20-7. 混合データのクラスタリング ------------------------------------------------------------

# *** カテゴリカルデータのクラスタリング ********************************************

# ＜ポイント＞
# - クラスタリングの教科書の例の多くは数値データのみとなっている
# - 現実のデータには｢カテゴリカルデータ｣や｢順序データ｣も含まれる
#   --- 方法1：自力で全て数値に直す
#   --- 方法2：Gower距離を使用


# ＜数値データへの変換手順＞
# - 順序カテゴリ変数を数値に変換
# - カテゴリ変数をワンホットエンコード


# データ準備
ames_full <-
  AmesHousing::make_ames() %>%
    mutate_if(str_detect(names(.), 'Qual|Cond|QC|Qu'), as.numeric)


# データ確認
# --- カテゴリカルデータ/順序データも含まれる
ames_full %>% glimpse()


# One-hot encode
# --- ｢sale price｣のみを残す
full_rank <- caret::dummyVars(Sale_Price ~ ., data = ames_full, fullRank = TRUE)
ames_1hot <- full_rank %>% predict(ames_full)


# 関数定義
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)


# 正規化
ames_1hot_scaled <-
  ames_1hot %>%
    as_tibble() %>%
    mutate(across(where(is.numeric), ~ scale2(.x, na.rm = TRUE)))


# データ確認
ames_1hot_scaled %>% as_tibble() %>% glimpse()
ames_1hot_scaled %>% dim()


# 最適クラスター数の推定
# --- 使用するクラスタ数が明確でない
# --- エラー発生
set.seed(123)
ames_1hot_scaled %>%
  select(-Neighborhood.Hayden_Lake) %>%
  fviz_nbclust(kmeans,
               method = "wss",
               k.max = 25,
               verbose = FALSE)



# *** gower距離の導入 ********************************************

# ＜課題＞
# - 特徴量の数が増えると、k-meansのパフォーマンスが低下する傾向がある
#   --- スクリープロットでクラスターごとのWSSの乖離が小さくなる（分類効率の低下）


# ＜Gower距離＞
# 数値データ：範囲正規化されたマンハッタン距離
# 順序データ：変数が最初にランク付けされ、次にマンハッタン距離がタイの特別な調整とともに使用
# 名目データ：ワンホットエンコード変換され、次にダイス係数が使用されます。 2つの観測値（X、Y）のダイスメトリックを計算するために、アルゴリズムはすべてのワンホットエンコードされたカテゴリ変数を調べ、それらを次のようにスコアリングします。

# a —両方の観測値のダミーの数1
# b —ダミーの数Xの場合は1、Yの場合は0
# c —ダミーの数Xの場合は0、Yの場合は1
# d —両方のダミーの数0


# ＜参考＞
# データサイエンスのための統計学入門 P294-298



# データ準備
# --- Sales Priceのみを除く
ames_full <- AmesHousing::make_ames() %>% select(-Sale_Price)


# 距離尺度の算出
# --- Gower距離
gower_dst <- ames_full %>% daisy(metric = "gower")
gower_dst %>% class()
gower_dst %>% glimpse()


# You can supply the Gower distance matrix to several clustering algos
pam_gower <- gower_dst %>% pam(k = 8, diss = TRUE)
diana_gower <- gower_dst %>% diana(diss = TRUE)
agnes_gower <- gower_dst %>% agnes(diss = TRUE)




# 20.8. その他の分類メソッド -----------------------------------------------

# *** 大規模データセット ********************************************

# ＜ポイント＞
# - データの次元が大きくなるにつれて、より多くの外れ値が介入する可能性がある
#   --- k-meansは平均を使用するため、外れ値に対してロバストではない
#   --- スクリープロットでクラスターごとのWSSの乖離が小さくなる（分類効率の低下）
# - cluster::pam()は中央値を用いて重心を決定するためロバストな結果が得られる


ames_1hot_scaled %>%
  fviz_nbclust(pam,
               method = "wss",
               k.max = 25,
               verbose = FALSE)



# *** 大規模データセット ********************************************

# ＜ポイント＞
# - 大規模データセットでは、k-means/階層クラスタリング/PAMクラスタリングのそれぞれで計算コストが高まる
#   --- CLARAはPAMと同じアルゴリズムを実行する大規模データセットで行う手法
# - 代替方法として、中央値の周りの分割(PAM)を使用することが挙げられる
#   --- cluster::clara()
#   --- cluster::pam()
#   --- 類似した結果だが、clara()は1/5程度の計算時間で実行可能

# ＜アルゴリズム＞
# 1. データセットを固定サイズの複数のサブセットにランダムに分割
# 2. 各サブセットでPAMアルゴリズムを計算し、対応するkメドイドを選択
#    --- データセット全体の各観測値を最も近いメドイドに割り当てます。
# 3. 最も近いメドイドに対する観測値の非類似度の平均（または合計）を計算します。
#    --- これは、クラスタリングの適合度の尺度として使用されます。
# 4. 平均（または合計）が最小であるサブデータセットを保持します。



# データ準備
features <- mnist$train$images


# k-means
system.time(kmeans(features, centers = 10))


# CLARA
system.time(clara(features, k = 10))


