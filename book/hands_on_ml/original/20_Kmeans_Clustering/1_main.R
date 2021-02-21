# ********************************************************************************
# Title   : k平均法クラスタリング
# Theme   : 
# Chapter : 20
# URL     : https://bradleyboehmke.github.io/HOML/kmeans.html
# Support : https://koalaverse.github.io/homlr/notebooks/20-kmeans.nb.html
# H2O　   : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/k-means.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(magrittr)
library(stringr)
library(Hmisc)
library(kableExtra)
library(cluster)
library(factoextra)



# 0. ポイント整理 ----------------------------------------------

# ＜前提＞
#・次元削減ではデータ内の欠損値はすべて削除または補完する
#・データはすべて数値（カテゴリデータはダミー変数化）
#・データはすべて基準か（各アルゴリズムが実装済のことが多い）


# ＜注意点＞
#・PCAは外れ値に大きな影響を受ける
#  --- 一般化低ランクモデルは外れ値に対して考慮されている
#・直線的な関係性しか表現できない
#  --- カーネルPCAは非線形投影が行える 


#＜最適成分数のアプローチ＞
#・固有値基準
#・分散の割合の説明基準
#・スクリープロット基準



# 1. データ準備 ----------------------------------------------

# データ取得1
mnist <- dslabs::read_mnist()

# データ取得2
my_basket <- read_csv("data/my_basket.csv")

# データ確認
my_basket %>% dim()



# 2. 相関分析 ----------------------------------------------

# 相関分析
my_basket %>% 
  correlate()


# 相関順位
my_basket %>% 
  correlate() %>% 
  stretch(remove.dups = TRUE, na.rm = TRUE) %>% 
  arrange(desc(r)) %>% 
  mutate(r = round(r, 3)) %>% 
  kable(caption = "Various items in our my basket data that are correlated.") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)


# 次元数
# --- 42次元
# --- 次元数が多いと関係性が理解しにくい
my_basket %>% ncol()




# 3.H2OによるPCA ----------------------------------------------

# H2Oの起動
h2o.no_progress()
h2o.init(max_mem_size = "5g")


# H2Oフレームに変換
my_basket.h2o <- my_basket %>% as.h2o()

# PCAの実行
# --- 今回は特徴量と同じ数のkを指定
my_pca <- 
  my_basket.h2o %>% 
    h2o.prcomp(pca_method = "GramSVD",
               k = ncol(my_basket.h2o), 
               transform = "STANDARDIZE", 
               impute_missing = TRUE,
               max_runtime_secs = 1000
               )


# 格納データ
my_pca %>% list.tree(depth = 1)
my_pca@model %>% names()



# 4.プロット -------------------------------------------------------------

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




# 5.最適な主成分数 -------------------------------------------------------------

## 5-1.固有値 -------------------------------------------------------------

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




## 5-2.分散比率 -------------------------------------------------------------

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


## 5-3スクリープロット -------------------------------------------------------------

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





