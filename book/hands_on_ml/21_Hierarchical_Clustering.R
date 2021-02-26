# **********************************************************************************
# Title     : Hands-on Machine Learning with R
# Chapter   : 21 Hierarchical Clustering
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/7
# URL       : https://bradleyboehmke.github.io/HOML/hierarchical.html
#           : https://koalaverse.github.io/homlr/notebooks/21-hierarchical-clustering.nb.html
# **********************************************************************************


# ＜ポイント＞
# - 階層クラスタリングは、k-meansクラスタリングの代替アプローチ
#   --- クラスターの数を事前に指定する必要がない
#   --- デンドログラムを作成するため可視化に優れている
# - ｢ボトムアップ｣と｢トップダウン｣の２つの方法がある


# ＜目次＞
# 21-1. 準備
# 21-2. アルゴリズム
# 21-3. 階層クラスタリング
# 21-4. 最適クラスターの決定
# 21-5. デンドログラムの活用



# 21-1. 準備 ----------------------------------------------------------

# Helper packages
library(tidyverse)
library(cluster)
library(factoextra)


# データ準備
# --- クラスタリングでは基準化が必須
ames_scale <-
  AmesHousing::make_ames() %>%
    select_if(is.numeric) %>%
    select(-Sale_Price) %>%
    mutate_all(as.double) %>%
    scale()



# 21-2. アルゴリズム ----------------------------------------------------------

# ＜ポイント＞
# - 階層クラスタリングのアルゴリズムは2つに分類される
#   --- 凝集的クラスタリング(ANGES)
#   --- 分割階層的クラスタリング(DIANA)
# - 凝集的クラスタリング（AGNES：AGglomerative NESting）
#   --- ボトムアップ方式で個々の要素を結合させていく
#   --- 最終的に1つのクラスタになるまで繰り返される
# - 分割階層的クラスタリング（DIANA：DIvise ANAlysis）
#   --- トップダウンで非類似度に基づいて分割していく


# ＜凝集アルゴリズム＞


# ＜非類似度の尺度＞
# - 最小分散(Ward法)がよく用いられる
#   --- クラスタ内の分散を最小化
#   --- コンパクトなクラスターを作成
#   --- 凝集的クラスタリングでよく用いられる




# 21-3. 階層クラスタリング(ANGES) ----------------------------------------------------------

# ＜ポイント＞
# - Rでは以下の関数がよく用いられる
#   --- ANGES： stats::hclust() / cluster::agnes()
#   --- DIANA： cluster::diana()


# *** 凝集的クラスタリング **************************

# stats::hclust()
# --- 書籍でよく紹介される方法
set.seed(123)
d <- ames_scale %>% dist(method = "euclidean")
hc1 <- d %>% hclust(method = "complete" )


# 簡便プロット
# --- 観測数が多く判別不能
hc1 %>% plot()


# cluster::agnes()
# --- stats::hclust()と同様に動作する
# --- 検出されたクラスタリング構造の量を測定する凝集係数(AC)の取得が可能
set.seed(123)
hc2 <- agnes(ames_scale, method = "complete")
hc2$ac


# *** アルゴリズムの比較 **************************

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# 関数定義
# --- 凝集係数(AC)の取得
ac <- function(x) {
  agnes(ames_scale, method = x)$ac
}

# 各アルゴリズムのACを比較
purrr::map_dbl(m, ac)



# *** 分割階層的クラスタリング **************************

# 分割階層的クラスタリング(DIANA)
# --- 1に近い分割係数（DC）は、より強いグループの区別を示唆
hc4 <- ames_scale %>% diana()
hc4$dc



# 21-4. 最適クラスターの決定 ---------------------------------------

# ＜ポイント＞
# - 階層的クラスタリングは、クラスターの関係を表す完全に接続された樹状図を提供する
#   --- その中で、抽出するクラスターの優先数を選択する必要がある場合がある
# - クラスタ数の決定方法として以下の手法が挙げられる
#   --- エルボー分析
#   --- シルエット分析
#   --- ギャップ統計量



# 最適クラスタ数
# --- 視覚的に判断
p1 <-
  ames_scale %>%
    fviz_nbclust(FUN = hcut, method = "wss", k.max = 10) +
    ggtitle("(A) Elbow method")

p2 <-
  ames_scale %>%
    fviz_nbclust(FUN = hcut, method = "silhouette", k.max = 10) +
    ggtitle("(B) Silhouette method")

p3 <-
  ames_scale %>%
    fviz_nbclust(FUN = hcut, method = "gap_stat", k.max = 10) +
    ggtitle("(C) Gap statistic")


# プロット作成
# --- 比較
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)



# 21-5. デンドログラムの活用 -------------------------------------------------

# ＜ポイント＞
# - 階層的クラスタリングの良いところは、データ内のクラスター間の関係を示す完全な樹状図を提供すること
# - ただし、解釈が誤解されやすいので注意
#   --- 高さは最下層の隣接した要素の距離を示すのではない
#   --- 高さは下層を凝集したポイントとの距離を示している


# 階層クラスタリング
# --- ウォード法
hc5 <- d %>% hclust(method = "ward.D2" )

# ツリーのカット
sub_grp <- hc5 %>% cutree(k = 8)

# Number of members in each cluster
sub_grp %>% table()


# Construct dendorgram for the Ames housing example
hc5 <- hclust(d, method = "ward.D2" )
dend_plot <- fviz_dend(hc5)
dend_data <- attr(dend_plot, "dendrogram")
dend_cuts <- cut(dend_data, h = 8)
fviz_dend(dend_cuts$lower[[2]])



df <- data.frame(
  x1 = c(-1.5, -1.3, -.9, -.6, .1, .1, .6, 1.2, 1.4),
  x2 = c(-.4, -1.5, -1.2, -1, -1.1, .6, -.2, -.5, -.3),
  label = c(3, 4, 6, 1, 2, 9, 8, 5, 7),
  row.names = c(3, 4, 6, 1, 2, 9, 8, 5, 7)
)
highlight <- filter(df, label %in% c(2 ,9))
p1 <- ggplot(df, aes(x1, x2, label = label)) +
  geom_label() +
  geom_label(data = highlight, fill = 'yellow')
df <- data.frame(
  x1 = c(-1.5, -1.3, -.9, -.6, .1, .1, .6, 1.2, 1.4),
  x2 = c(-.4, -1.5, -1.2, -1, -1.1, .6, -.2, -.5, -.3),
  row.names = c(3, 4, 6, 1, 2, 9, 8, 5, 7)
)
p2 <- factoextra::fviz_dend(hclust(dist(df)))
gridExtra::grid.arrange(p1, p2, nrow = 1)



# Ward's method
hc5 <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 8)
# Number of members in each cluster
table(sub_grp)



# Plot full dendogram
fviz_dend(
  hc5,
  k = 8,
  horiz = TRUE,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.1
)


dend_plot <- fviz_dend(hc5)                # create full dendogram
dend_data <- attr(dend_plot, "dendrogram") # extract plot info
dend_cuts <- cut(dend_data, h = 70.5)      # cut the dendogram at
                                           # designated height
# Create sub dendrogram plots
p1 <- fviz_dend(dend_cuts$lower[[1]])
p2 <- fviz_dend(dend_cuts$lower[[1]], type = 'circular')
# Side by side plots
gridExtra::grid.arrange(p1, p2, nrow = 1)
