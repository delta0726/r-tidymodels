# Title     : Chapter 17 Principal Components Analysis
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/7
# URL       : https://bradleyboehmke.github.io/HOML/pca.html
#           : https://koalaverse.github.io/homlr/notebooks/17-pca.nb.html
#           : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/pca.html




# ＜ポイント＞
# - 主成分分析(PCA)は、元の変動を可能な限り保持するデータセットの低次元表現を見つけるための方法
#   --- n個の観測値のそれぞれがp次元空間に存在するという考え方
#   --- PCAは特徴量空間(p)を圧縮している
#   --- クラスタリングは観測空間(n)を圧縮している
# - PCAで見つかった新しい次元のそれぞれは、元のp特徴の線形結合


# ＜注意点＞
# - PCAは外れ値に大きな影響を受ける
#   --- 一般化低ランクモデルは外れ値に対して考慮されている
# - 直線的な関係性しか表現できない
#   --- カーネルPCAは非線形投影を行うことができる


# ＜目次＞
# 17.1 準備
# 17.2 PCAのアイデア
# 17.3 主成分を見つける
# 17.4.H2OによるPCA
# 17.5 主成分のプロット
# 17.6.最適な主成分数



# 17.1 準備 ---------------------------------------

# ＜ポイント＞
# - 次元削減ではデータ内の欠損値はすべて削除または補完してNAがない状態にしておく
# - データはすべて数値（カテゴリデータはダミー変換で数値化）
# - データはすべて基準化しておく（各アルゴリズムが実装済のことが多い）



library(magrittr)
library(tidyverse)
library(tidyquant)
library(corrr)
library(h2o)
library(pca3d)
library(kableExtra)


# カレントディレクトリ設定
setwd("I:/Project/R/tidymodels/book/ml_hans_on")


# データ取得
# --- 食料品店から2,000トランザクションで購入したアイテムと数量を識別
my_basket <- read_csv("data/my_basket.csv")


# データ確認
my_basket %>% print()
my_basket %>% glimpse()



# 17.2. PCAのアイデア ----------------------------------------------

# ＜ポイント＞
# - 主成分分析は、元のバリエーションをできるだけ多く保持するデータセットの低次元表現を見つける手法
# - データの次元数は特徴量の列数だけ存在するが、全て等しく興味があるわけではない
#   --- データの情報量は｢分散量｣が持っていると考える
#   --- 相関が高い特徴量はまとめることができる



# 元データの次元数
# --- 42次元
# --- 次元数が多いと関係性が理解しにくい
my_basket %>% ncol()


# 相関分析
my_basket %>% correlate()


# 相関順位
X_Plot <-
  my_basket %>%
    correlate() %>%
    stretch(remove.dups = TRUE, na.rm = TRUE) %>%
    arrange(desc(r))


# プロット
# --- 42個の変数のうち23個の変数の組み合わせは互いに中程度の相関(0.25以上)を持っている
# --- これらのような一般的な属性を、元のデータよりも低次元で説明したいことがよくあります
#     例えば、コーラ/ペプシ/7UPを｢ソーダ｣のように潜在変数でまとめたい
X_Plot %>%
  mutate(name = str_c(x, y, sep = "_"), ) %>%
  mutate(name = fct_reorder(name, r)) %>%
  arrange(desc(r)) %>%
  head(30) %>%
  ggplot(aes(x = name, y = r)) +
  geom_bar(stat = "identity") +
  coord_flip()




# 17.3 主成分を見つける ------------------------------------------------------

# ***** 2次元のPCA ******************************

# データ準備
df <-
  AmesHousing::make_ames() %>%
    select(var1 = First_Flr_SF, var2 = Gr_Liv_Area) %>%
    filter(var1 != var2) %>%
    mutate_all(log) %>%
    scale() %>%
    data.frame() %>%
    filter(var1 < 4)


# データ確認
# --- 2系列のデータセット
# --- 高い相関を持つ
df %>% head()
df %>% correlate()

# プロット
# --- 本当はPC1とPC2は直交になっているべき
df %>%
    ggplot(aes(x = var1, y = var2)) +
    geom_jitter(alpha = .2, size = 1, color = "dodgerblue") +
    geom_segment(
      aes(x = 0, xend = 1.5 , y = 0, yend = 1.5),
      arrow = arrow(length = unit(0.25,"cm")), size = 0.75, color = "black"
    ) +
    annotate("text", x = 1, y = .2, label = "First principal component", size = 2.5, hjust = 0) +
    annotate("text", x = -3, y = .8, label = "Second principal component", size = 2.5, hjust = 0) +
    geom_segment(
      aes(x = 0, xend = -0.27 , y = 0, yend = .65),
      arrow = arrow(length = unit(0.25,"cm")), size = 0.75, color = "black"
    ) +
    xlab("Feature 2") +
    ylab("Feature 1") +
    theme_bw()



# ***** 3次元のPCA ******************************

# ＜ポイント＞
# - PCAの空間表現は3次元までが限界


# データ作成
# --- 対数化
df <-
  AmesHousing::make_ames() %>%
    select(var1 = First_Flr_SF,
           var2 = Gr_Liv_Area,
           var3 = TotRms_AbvGrd) %>%
    filter(var1 != var2) %>%
    mutate_at(vars(var1, var2), log)


# 主成分分析
pca <- df %>% prcomp(scale = FALSE)

# プロット
# --- 3次元
pca %>% pca3d()





# 17.4.H2OによるPCA ----------------------------------------------

# ＜ポイント＞
# - H2Oは、元削減方法全体で一貫性を提供し、データ準備手順の多くを自動化してくれる
#   --- 数値特徴、欠落値の代入、およびカテゴリー特徴のエンコード


# H2Oの起動
h2o.no_progress()
h2o.init(max_mem_size = "5g")


# H2Oフレームに変換
my_basket.h2o <- my_basket %>% as.h2o()


# PCAの実行
# --- 基準化や欠損値処理の方法を指定することができる
# --- データが数値データの場合は、pca_method =“GramSVD”を使用するのが最適
# --- データがカテゴリ変数の場合は、pca_method =“GLRM”を使用するのが最適
my_pca <-
  my_basket.h2o %>%
    h2o.prcomp(pca_method = "GramSVD",
               k = ncol(my_basket.h2o),
               transform = "STANDARDIZE",
               impute_missing = TRUE,
               max_runtime_secs = 1000
               )


# 格納データ
my_pca %>% glimpse()
my_pca@model %>% names()




# 17.5 主成分のプロット------------------------------------------------------

# PC1の因子負荷量
my_pca@model$eigenvectors %>%
  as.data.frame() %>%
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, reorder(feature, pc1))) +
  geom_point() +
  theme_tq()


# マッピング
# --- PC1とPC2
my_pca@model$eigenvectors %>%
  as.data.frame() %>%
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, pc2, label = feature)) +
  geom_text() +
  theme_tq()




# 17.6.最適な主成分数 -------------------------------------------------------------

#＜最適成分数のアプローチ＞
#・固有値基準
#・分散の割合の説明基準
#・スクリープロット基準



## 17.6.1.固有値 -------------------------------------------------------------

# 固有値の計算
eigen <-
  my_pca@model$importance["Standard deviation", ] %>%
  as.vector() %>%
  .^2

# 合計
# --- 固有値の合計は特徴量の数と等しくなる
eigen %>% sum()


# 固有値が1以上なら重要度が高い
which(eigen >= 1)


# プロット
data.frame(
  PC = seq_along(eigen),
  Eigenvalue = unlist(eigen)
) %>%
  ggplot(aes(PC, Eigenvalue)) +
  geom_point() +
  geom_hline(yintercept = 1, lty = "dashed", color = "red") +
  scale_y_continuous(breaks = 0:6) +
  xlab("PC") +
  annotate("text", x = 15, y = 1, label = "eigenvalue criteria cutoff", color = "red", size = 5, hjust = 0, vjust = -1)




## 17.6.2.分散比率 -------------------------------------------------------------

# Extract and plot PVE and CVE
data.frame(
  PC  = my_pca@model$importance %>% seq_along(),
  PVE = my_pca@model$importance %>% .[2,] %>% unlist(),
  CVE = my_pca@model$importance %>% .[3,] %>% unlist()
) %>%
  tidyr::gather(metric, variance_explained, -PC) %>%
  ggplot(aes(PC, variance_explained)) +
  geom_point() +
  facet_wrap(~ metric, ncol = 1, scales = "free")




ve <- data.frame(
  PC  = my_pca@model$importance %>% names(),
  PVE = my_pca@model$importance %>% .[2,] %>% unlist(),
  CVE = my_pca@model$importance %>% .[3,] %>% unlist()
)

min(which(ve$CVE >= 0.75))


## 17.6.3.スクリープロット -------------------------------------------------------------

# プロット作成
# --- スクリープロットはY軸に固有値(PVE)をとる
# --- PVEが大きく落ち込んだ直後が最適な成分数
# --- PC8あたりまでが妥当か？
data.frame(
  PC  = my_pca@model$importance %>% seq_along(),
  PVE = my_pca@model$importance %>% .[2,] %>% unlist()
  ) %>%
  ggplot(aes(PC, PVE, group = 1, label = PC)) +
  geom_point() +
  geom_line() +
  geom_text(nudge_y = -.002)





