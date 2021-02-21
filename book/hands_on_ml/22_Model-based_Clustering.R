# Title     : Chapter 22 Model-based Clustering
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/7
# URL       : https://bradleyboehmke.github.io/HOML/model-clustering.html
#           : https://koalaverse.github.io/homlr/notebooks/22-model-based-clustering.nb.html




# ＜ポイント＞
# - k-meansや階層クラスタリングはパラメータを与えないヒューリスティックベースのアルゴリズム
#   --- モデルベースのクラスタリングは、パラメータとして事前情報を与えることができる
# - モデルベースクラスタリングには実行速度の面で課題がある
#   --- モデルベースのクラスタリング手法が劇的に過剰にパラメータ化されているため
#   --- クラスタリングの前に次元削減を実行することである程度解消（目的が損なわれないように次元削減）
#   --- {HDclassif}が代替ソリューションとして挙げれあれる



# ＜目次＞
# 22-1. 準備
# 22-2. 確率と不確実性の測定
# 22-3. 共分散のタイプ
# 22-4. モデル選択
# 22-5. 分析事例： My basket




# 22-1. 準備 ----------------------------------------------------------


library(tidyverse)
library(mclust)


# カレントディレクトリ設定
setwd("I:/Project/R/tidymodels/book/ml_hans_on")


# データ取得
# --- geyser
data(geyser, package = 'MASS')
geyser %>% glimpse()


# データ取得
# --- my_basket
my_basket <- read_csv("data/my_basket.csv")
my_basket %>% glimpse()



# 22-2. 確率と不確実性の測定 -----------------------------------------------

# ＜ポイント＞
# - モデルベースのクラスタリングは、データが基礎となる確率分布の混合から形成される見なされる
#   --- ガウス混合モデル(GMM)
#   --- 各観測値はk個の多変量正規分布の1つとして分布すると想定
# - 確率的クラスター割当は、不確実性が高いクラスターまたは観測値を識別することができる
#   --- 代替ソリューションを検討するのに非常に役立つ

# 間欠泉データ
# --- 確率分布のイメージ
# --- 以降ではモデルベース・クラスタリングで3つのクラスタを再現
geyser %>%
  ggplot(aes(waiting, duration)) +
    geom_point(size = 0.75, alpha = 0.5) +
    geom_density2d(aes(alpha = ..level..), show.legend = FALSE) +
    scale_x_continuous("X") +
    scale_y_continuous("Y") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )


# Apply GMM model with 3 components
geyser_mc <- geyser %>% Mclust(G = 3, verbose = FALSE)
geyser_mc %>% summary()


# プロット
# --- 密度
geyser_mc %>% plot(what = "density")


# プロット
# --- 不確実性
geyser_mc %>% plot(what = "uncertainty")


# 不確実性の高いサンプル
# --- 上位のみ表示
geyser_mc$uncertainty %>%
  sort(decreasing = TRUE) %>%
  head()




# 22-3. 共分散のタイプ ---------------------------------------

par(
  mfrow = c(2, 5), mar = c(0.5, 0.5, 2.5, 0.5),
  cex = .4,
  pch = 19,
  col = alpha('black', 0.25),
  xaxt = 'n', yaxt = 'n', ann = FALSE, cex.main = 3
  )

# EII
set.seed(111)
obj <- mlbench::mlbench.hypercube(n = 400)
df <- data.frame(
    x = obj$x[, 1],
    y = obj$x[, 2]
)
m <- Mclust(df, verbose = FALSE)
plot(m, what = "density", type = "hdr")
points(df)


title(main = m$modelName)

# VII
df <- data_frame(
    x1 = c(rnorm(100, sd = .75), rnorm(100, sd = .25) + 5),
    x2 = c(rnorm(100, sd = .75), rnorm(100, sd = .25))
)
m <- Mclust(df, G = 2, verbose = FALSE)
plot(m, what = "density", type = "hdr")


points(df)
title(main = m$modelName)




# EEI
df <- data_frame(
    x1 = c(rnorm(100, sd = .75), rnorm(100, sd = .75)),
    x2 = c(rnorm(100, sd = .75), rnorm(100, sd = .75) - 5)
)
m <- Mclust(df, G = 2, modelNames = "EEI", verbose = FALSE)
plot(m, what = "density", type = "hdr")
points(df)


title(main = m$modelName)

# VVI
set.seed(111)
obj <- mlbench::mlbench.cuboids(300)
df <- data.frame(
    x = obj$x[, 1],
    y = obj$x[, 2]
)
m <- Mclust(df, verbose = FALSE)
plot(m, what = "density", type = "hdr")

points(df)
title(main = m$modelName)

# VVE
m <- Mclust(faithful, G = 2, verbose = FALSE)
plot(m, what = "density", type = "hdr")
points(faithful)



title(main = m$modelName)

# EEE
set.seed(111)
obj <- mlbench::mlbench.cassini(200)
df <- data.frame(
    x = obj$x[, 1],
    y = obj$x[, 2]
)
m <- Mclust(df, G = 20, verbose = FALSE)
plot(m, what = "density", type = "hdr")


points(df)
title(main = m$modelName)

# EEV
set.seed(111)
obj <- mlbench::mlbench.spirals(200, 1, 0.025)
df <- data.frame(
    x = obj$x[, 1],
    y = obj$x[, 2]
)
m <- Mclust(df, G = 20, verbose = FALSE)
plot(m, what = "density", type = "hdr")
points(df)


title(main = m$modelName)

# VEV
m <- Mclust(mtcars[, c('mpg', 'wt')], G = 2, verbose = FALSE)
plot(m, what = "density", type = "hdr")

points(mtcars[, c('mpg', 'wt')])
title(main = m$modelName)



# EEV
set.seed(111)
obj <- mlbench::mlbench.smiley()
df <- data.frame(
    x = obj$x[, 1],
    y = obj$x[, 2]
)
m <- Mclust(df, G = 20, verbose = FALSE)
plot(m, what = "density", type = "hdr")
points(df)



title(main = m$modelName)

# EVE
set.seed(111)
obj <- mlbench::mlbench.1spiral(300, sd = .15)
df <- data.frame(
    x = obj[, 1],
    y = obj[, 2]
)
m <- Mclust(df, verbose = FALSE)
plot(m, what = "density", type = "hdr")

points(df)
title(main = m$modelName)






# 22-4. モデル選択 --------------------------------------------------

# ＜課題＞
# - 表22.3の14のモデルすべてを適用し、データを最もよく特徴付けるモデルを特定
#   --- ベイズ情報量基準(BIC)がよくワークする
#   --- BICを基準にハイパーパラメータのチューニングを行う


# サマリー
# --- Mclust()がEEIモデルを適用していることを確認
geyser_mc %>% summary()
geyser_mc %>% summary(parameters = TRUE)


# チューニング
geyser_optimal_mc <- geyser %>% Mclust(verbose = FALSE)
geyser_optimal_mc %>% summary()


legend_args <- list(x = "bottomright", ncol = 5)
plot(geyser_optimal_mc, what = 'BIC', legendArgs = legend_args)
plot(geyser_optimal_mc, what = 'classification')

plot(geyser_optimal_mc, what = 'uncertainty')





# 22-5. 分析事例： My basket---------------------------------------


my_basket_mc <- my_basket %>% Mclust(1:20, verbose = FALSE)
my_basket_mc %>% summary()


my_basket_mc %>%
  plot(what = 'BIC',
       legendArgs = list(x = "bottomright", ncol = 5))

probabilities <- my_basket_mc$z
colnames(probabilities) <- paste0('C', 1:6)

probabilities <-
  probabilities %>%
    as.data.frame() %>%
    mutate(id = row_number()) %>%
    tidyr::gather(cluster, probability, -id)

probabilities %>%
  ggplot(aes(probability)) +
    geom_histogram() +
    facet_wrap(~ cluster, nrow = 2)

uncertainty <- data.frame(
  id = 1:nrow(my_basket),
  cluster = my_basket_mc$classification,
  uncertainty = my_basket_mc$uncertainty
)

uncertainty %>%
  group_by(cluster) %>%
  filter(uncertainty > 0.25) %>%
  ggplot(aes(uncertainty, reorder(id, uncertainty))) +
  geom_point() +
  facet_wrap(~ cluster, scales = 'free_y', nrow = 1)



cluster2 <- my_basket %>%
  scale() %>%
  as.data.frame() %>%
  mutate(cluster = my_basket_mc$classification) %>%
  filter(cluster == 2) %>%
  select(-cluster)

cluster2 %>%
  tidyr::gather(product, std_count) %>%
  group_by(product) %>%
  summarize(avg = mean(std_count)) %>%
  ggplot(aes(avg, reorder(product, avg))) +
  geom_point() +
  labs(x = "Average standardized consumption", y = NULL)


