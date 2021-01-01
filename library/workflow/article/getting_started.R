# Title     : Getting Started
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/10
# URL       : https://workflows.tidymodels.org/articles/extras/getting-started.html#a-different-model



library(tidyverse)
library(tidymodels)
library(DataExplorer)
library(discrim)


# データロード
data("bivariate")


# データ概要
# --- 2ファクターによる分類問題
bivariate_train %>% print()
bivariate_train %>% glimpse()

# レコード数
bivariate_train %>% dim()
bivariate_val %>% dim()


# 相関分析
# --- AとBは相関が高い
bivariate_train %>% plot_correlation()



# データ確認 -----------------------------------------------------------

# プロット作成（散布図）
# --- AとBともに外れ値がある
bivariate_train %>%
  ggplot(aes(x = A, y = B, col = Class)) +
    geom_point(alpha = .3) +
    coord_equal(ratio = 20)


# プロット作成（ボックスプロット）
# --- Y軸に対数軸を適用
bivariate_train %>%
  pivot_longer(cols = c(A, B), names_to = "predictor") %>%
  ggplot(aes(x = Class, y = value)) +
  geom_boxplot() +
  facet_wrap(~predictor, scales = "free_y") +
  scale_y_log10()


logit_mod <-
  logistic_reg() %>%
  set_engine("glm")




# 共通モデル(ロジスティック回帰) -------------------------------------------

# モデリング
logit_mod <-
  logistic_reg() %>%
  set_engine("glm")


# ワークフロー設定
# --- モデルのみ定義
# --- フォーミュラは後ほど設定
glm_workflow <-
  workflow() %>%
  add_model(logit_mod)



# モデル1：線形モデル-------------------------------------------------------

# モデル定義＆学習
# --- 単純な線形モデル
simple_glm <-
  glm_workflow %>%
  add_formula(Class ~ .) %>%
  fit(data = bivariate_train)


# 予測データの作成
# --- 検証用データで予測
# --- 予測結果を元データに追加
simple_glm_probs <-
  simple_glm %>%
    predict(bivariate_val, type = "prob") %>%
    bind_cols(bivariate_val)


# パフォーマンス評価
# --- ROCカーブ
simple_glm_roc <-
  simple_glm_probs %>%
  roc_curve(Class, .pred_One)


# パフォーマンス評価
# --- AUC
simple_glm_probs %>%
  roc_auc(Class, .pred_One)


# プロット作成
simple_glm_roc %>%
  autoplot()



# モデル2：分数モデル------------------------------------------------

# モデル定義＆学習
ratio_glm <-
  glm_workflow %>%
  add_formula(Class ~ I(A/B)) %>%
  fit(data = bivariate_train)


# 予測データの作成
# --- 検証用データで予測
# --- 予測結果を元データに追加
ratio_glm_probs <-
  ratio_glm %>%
    predict(bivariate_val, type = "prob") %>%
    bind_cols(bivariate_val)

# パフォーマンス評価
# --- ROCカーブ
ratio_glm_roc <-
  ratio_glm_probs %>%
  roc_curve(Class, .pred_One)


# パフォーマンス評価
# --- AUC
ratio_glm_probs %>%
  roc_auc(Class, .pred_One)


# プロット作成
simple_glm_roc %>%
  autoplot() +
    geom_path(
      data = ratio_glm_roc,
      aes(x = 1 - specificity, y = sensitivity),
      col = "#FDE725FF"
    )




# 線形モデル + Box-Cox変換 -------------------------------------------

# レシピの導入
# --- 単純な線形モデル
# --- Box-Cox変換で分布の歪度を正規化
trans_recipe <-
  recipe(Class ~ ., data = bivariate_train) %>%
    step_BoxCox(all_predictors()) %>%
    prep()


# モデル定義＆学習
# --- レシピの適用
trans_glm <-
  glm_workflow %>%
  add_recipe(trans_recipe) %>%
  fit(data = bivariate_train)


# 予測データの作成
# --- 検証用データで予測
# --- 予測結果を元データに追加
trans_glm_probs <-
  trans_glm %>%
    predict(bivariate_val, type = "prob") %>%
    bind_cols(bivariate_val)


# パフォーマンス評価
# --- ROCカーブ
trans_glm_roc <-
  trans_glm_probs %>%
  roc_curve(Class, .pred_One)


# パフォーマンス評価
# --- AUC
trans_glm_probs %>% roc_auc(Class, .pred_One)


# プロット作成
autoplot(simple_glm_roc) +
  geom_path(
    data = ratio_glm_roc,
    aes(x = 1 - specificity, y = sensitivity),
    col = "#FDE725FF"
  ) +
  geom_path(
    data = trans_glm_roc,
    aes(x = 1 - specificity, y = sensitivity),
    col = "#21908CFF"
  )






# 線形モデル + PCA -------------------------------------------


# プロット再確認
# --- AとBを逆変換（相関が高いことが改めて確認される）
# --- 単純な変換では限界がある
bivariate_train %>%
  ggplot(aes(x = 1/A, y = 1/B, col = Class)) +
    geom_point(alpha = .3) +
    coord_equal(ratio = 1/12)


# レシピの追加
# --- 単純な線形モデル
# --- Box-Cox変換で分布の歪度を正規化
pca_recipe <-
  recipe(Class ~ ., data = bivariate_train) %>%
    step_BoxCox(all_predictors()) %>%
    step_normalize(A, B) %>%
    step_pca(A, B, num_comp = 2) %>%
    prep()


# モデル定義＆学習
# --- レシピの適用
pca_glm <-
  glm_workflow %>%
  add_recipe(pca_recipe) %>%
  fit(data = bivariate_train)


# 予測データの作成
# --- 検証用データで予測
# --- 予測結果を元データに追加
pca_glm_probs <-
  pca_glm %>%
    predict(bivariate_val, type = "prob") %>%
    bind_cols(bivariate_val)


# パフォーマンス評価
# --- ROCカーブ
pca_glm_roc <-
  pca_glm_probs %>%
  roc_curve(Class, .pred_One)


# パフォーマンス評価
# --- AUC
pca_glm_probs %>% roc_auc(Class, .pred_One)



# プロット作成
autoplot(simple_glm_roc) +
  geom_path(
    data = ratio_glm_roc,
    aes(x = 1 - specificity, y = sensitivity),
    col = "#FDE725FF"
  ) +
  geom_path(
    data = trans_glm_roc,
    aes(x = 1 - specificity, y = sensitivity),
    col = "#21908CFF"
  ) +
  geom_path(
    data = pca_glm_roc,
    aes(x = 1 - specificity, y = sensitivity),
    col = "Blue"
  )




# 線形判別分析 -------------------------------------------


# モデル定義
discrim_mod <-
  discrim_flexible() %>%
  set_engine("earth") %>%
  set_mode("classification")


# レシピ定義
# --- 単純な線形モデル
# --- Box-Cox変換で分布の歪度を正規化
trans_recipe <-
  recipe(Class ~ ., data = bivariate_train) %>%
    step_BoxCox(all_predictors()) %>%
    prep()


# ワークフロー設定＆学習
discrim_wflow <-
  workflow() %>%
  add_recipe(trans_recipe) %>%
  add_model(discrim_mod) %>%
  fit(data = bivariate_train)


# 予測データの作成
# --- 検証用データで予測
# --- 予測結果を元データに追加
discrim_probs <-
  discrim_wflow %>%
    predict(bivariate_val, type = "prob") %>%
    bind_cols(bivariate_val)


# パフォーマンス評価
# --- ROCカーブ
discrim_roc <-
  discrim_probs %>%
  roc_curve(Class, .pred_One)


# パフォーマンス評価
# --- AUC
discrim_probs %>% roc_auc(Class, .pred_One)




