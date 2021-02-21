# ********************************************************************************
# Title   : Gradient Boosting
# Chapter : 12
# URL     : https://bradleyboehmke.github.io/HOML/gbm.html
# Support : https://koalaverse.github.io/homlr/notebooks/10-bagging.nb.html
# ********************************************************************************



library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(doParallel)
library(foreach)
library(rsample)
library(rpart)
library(gbm)
library(xgboost)
library(vip)
library(h2o)
library(ggfortify)
library(DataExplorer)
library(gridExtra)
library(DALEX)



# ＜ポイント＞
#・バギングは低バイアスで高分散のモデルにより効果的
#・ブースティングは高バイアスで低分散のモデルにより効果的
#・ブースティングの主なアイデアは、新しいモデルをアンサンブルに順番に追加すること
#・モデルを更新していくことで、直前のモデルの誤りを修正していく


# ＜ベース学習器＞
# ・一応様々な学習器をプラグインすることできる
# ・現実ではほぼ常に決定木を使用する（高バイアス・低分散）


# ＜弱学習器＞
#・エラー率がランダム推測よりもわずかに優れているような学習器を用いる
#・決定木でいうと、分割の少ない浅い木が適している



# 0.データ作成 ----------------------------------------------------------

# Sinカーブのデータを作成
set.seed(1112)  
df <- tibble::tibble(
  x = seq(from = 0, to = 2 * pi, length = 1000),
  y = sin(x) + rnorm(length(x), sd = 0.5),
  truth = sin(x)
)

# 確認
df %>% print()



# 1.関数定義 ----------------------------------------------------------

# Function to boost `rpart::rpart()` trees
rpartBoost <- function(x, y, data, num_trees = 100, learn_rate = 0.1, tree_depth = 6) {
  x <- data[[deparse(substitute(x))]]
  y <- data[[deparse(substitute(y))]]
  G_b_hat <- matrix(0, nrow = length(y), ncol = num_trees + 1)
  r <- y
  for(tree in seq_len(num_trees)) {
    g_b_tilde <- rpart(r ~ x, control = list(cp = 0, maxdepth = tree_depth))
    g_b_hat <- learn_rate * predict(g_b_tilde)
    G_b_hat[, tree + 1] <- G_b_hat[, tree] + matrix(g_b_hat)
    r <- r - g_b_hat
    colnames(G_b_hat) <- paste0("tree_", c(0, seq_len(num_trees)))
  }
  cbind(df, as.data.frame(G_b_hat)) %>%
    gather(tree, prediction, starts_with("tree")) %>%
    mutate(tree = stringr::str_extract(tree, "\\d+") %>% as.numeric())
}

# Plot boosted tree sequence
rpartBoost(x, y, data = df, num_trees = 2^10, learn_rate = 0.05, tree_depth = 1) %>%
  filter(tree %in% c(0, 2^c(0:10))) %>%
  ggplot(aes(x, prediction)) +
  ylab("y") +
  geom_point(data = df, aes(x, y), alpha = .1) +
  geom_line(data = df, aes(x, truth), color = "blue") +
  geom_line(colour = "red", size = 1) +
  facet_wrap(~ tree, nrow = 3)

