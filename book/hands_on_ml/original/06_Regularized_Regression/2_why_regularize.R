# ********************************************************************************
# Title   : 正則化回帰
# Chapter : 6
# Memo    : 6.2 Wny Regulizedの箇所
# URL     : https://bradleyboehmke.github.io/HOML/regularized-regression.html
# Support : https://koalaverse.github.io/homlr/notebooks/06-regularized-regression.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(magrittr)
library(glmnet)
library(glmnetUtils)
library(ggfortify)




# 0. メモ ----------------------------------------------

# ＜OLS回帰の目的＞
#・観測された応答値と予測された応答値の間の平方誤差(SSE)の合計を最小化する超平面を見つけること


# ＜課題＞
#・説明変数が増えてくるとOLS回帰が前提とするエラーの等分散性や正規性が担保できなくなる
#・ステップワイズ法による探索は非効率的


# ＜ソリューション＞
#・正則化回帰によりSSEにP(ペナルティ)をつける
#・ペナルティが減らせる場合にのみ係数を増やすことができる



# 1. 準備 ----------------------------------------------

## データ

# ボストンデータ
boston_df <- pdp::boston


# 確認
boston_df %>% glimpse()


# モデル用データ
boston_train_x <- model.matrix(cmedv ~ ., boston_df)[, -1]
boston_train_y <- boston_df$cmedv



# 2. リッジ回帰 ----------------------------------------------

#・変数選択は実施されず、最終モデルはすべての回帰係数を保持する
#・重要度の低い係数にはペナルティがかかり、ノイズは減らされる
#・すべての変数を使いたい場合に適している


# リッジ回帰
boston_ridge <- 
  glmnet(x = boston_train_x, 
         y = boston_train_y, 
         alpha = 0)

# リッジ回帰
# --- {glmnetUtils}の記法
# --- 従来の回帰の記法で記述できる
boston_ridge <- 
  glmnetUtils::glmnet(cmedv ~ . - lon, data = boston_df)



# 格納データ
boston_ridge %>% glimpse()


# ラムダとペナルティ
lam <- 
  boston_ridge$lambda %>% 
    as.data.frame() %>%
    mutate(penalty = boston_ridge$a0 %>% names()) %>%
    set_colnames(c("lambda", "penalty")) %>% 
    as_tibble()


# プロット用データ
results <- 
  boston_ridge$beta %>% 
    as.matrix() %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    gather(penalty, coefficients, -rowname) %>%
    left_join(lam) %>% 
    as_tibble()


# プロット用ラベル
result_labels <- 
  results %>%
  group_by(rowname) %>%
  filter(lambda == min(lambda)) %>%
  ungroup() %>%
  top_n(5, wt = abs(coefficients)) %>%
  mutate(var = paste0("x", 1:5))

# プロット
ggplot() +
  geom_line(data = results, 
            aes(lambda, coefficients, group = rowname, color = rowname), 
            show.legend = FALSE) +
  scale_x_log10() +
  geom_text(data = result_labels, 
            aes(lambda, coefficients, label = var, color = rowname),
            nudge_x = -.06, show.legend = FALSE)


# プロット
# --- お手軽
boston_ridge %>% autoplot(xvar = c("lambda"))




## ラッソ回帰 ----

#・重要度の低い変数の係数をゼロとする変数選択が行われる
#・モデル検証やメリハリのあるモデル構築に有用

# リッジ回帰
boston_lasso <- glmnet(x = boston_train_x, 
                       y = boston_train_y, 
                       alpha = 1)


# ラムダとペナルティ
lam <- 
  boston_lasso$lambda %>% 
  as.data.frame() %>%
  mutate(penalty = boston_lasso$a0 %>% names()) %>%
  set_colnames(c("lambda", "penalty")) %>% 
  as_tibble()

# プロット用データ
results <- 
  boston_lasso$beta %>% 
  as.matrix() %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  gather(penalty, coefficients, -rowname) %>%
  left_join(lam) %>% 
  as_tibble()

# プロット用ラベル
result_labels <- 
  results %>%
  group_by(rowname) %>%
  filter(lambda == min(lambda)) %>%
  ungroup() %>%
  top_n(5, wt = abs(coefficients)) %>%
  mutate(var = paste0("x", 1:5))

# プロット
ggplot() +
  geom_line(data = results, 
            aes(lambda, coefficients, group = rowname, color = rowname), 
            show.legend = FALSE) +
  scale_x_log10() +
  geom_text(data = result_labels, 
            aes(lambda, coefficients, label = var, color = rowname),
            nudge_x = -.06, show.legend = FALSE)


# プロット
# --- お手軽
boston_lasso %>% autoplot(xvar = c("lambda"))




## Elasti Net(弾性ネット) ----

#・LASSOとRIDGEを組み合わせたもの
#・リッジ回帰のメリハリをつけて強化するという側面を持つ


# 弾性ネット回帰
boston_elastic <- glmnet(x = boston_train_x, 
                         y = boston_train_y, 
                         alpha = 0.2)


# ラムダとペナルティ
lam <- 
  boston_elastic$lambda %>% 
  as.data.frame() %>%
  mutate(penalty = boston_elastic$a0 %>% names()) %>%
  set_colnames(c("lambda", "penalty")) %>% 
  as_tibble()

# プロット用データ
results <- 
  boston_elastic$beta %>% 
  as.matrix() %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  gather(penalty, coefficients, -rowname) %>%
  left_join(lam) %>% 
  as_tibble()

# プロット用ラベル
result_labels <- 
  results %>%
  group_by(rowname) %>%
  filter(lambda == min(lambda)) %>%
  ungroup() %>%
  top_n(5, wt = abs(coefficients)) %>%
  mutate(var = paste0("x", 1:5))

# プロット
ggplot() +
  geom_line(data = results, 
            aes(lambda, coefficients, group = rowname, color = rowname), 
            show.legend = FALSE) +
  scale_x_log10() +
  geom_text(data = result_labels, 
            aes(lambda, coefficients, label = var, color = rowname),
            nudge_x = -.06, show.legend = FALSE)


# プロット
# --- お手軽
boston_elastic %>% autoplot(xvar = c("lambda"))
