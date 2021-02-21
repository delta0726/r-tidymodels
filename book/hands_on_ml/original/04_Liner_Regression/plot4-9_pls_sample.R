# ********************************************************************************
# Title   : PCA回帰 (PCR)
# Chapter : 4
# Memo    : 図4-9のPCRとPLSの結果比較のプロットを作成する
# URL     : https://bradleyboehmke.github.io/HOML/linear-regression.html
# Support : https://koalaverse.github.io/homlr/notebooks/06-regularized-regression.nb.html
# ********************************************************************************


library(AppliedPredictiveModeling)
library(recipes)
library(tidyr)


# ポイント整理 ----------------------------------------------

# ＜ポイント＞
#・PCRとPLSの違いを理解する
#・{recipe}でPCRとPLSの前処理が適用できることを確認する



# プロット作成 ----------------------------------------------


# データ準備
data(solubility)


# モデル用データ
df <- cbind(solTrainX, solTrainY)
df %>% dim()
df %>% glimpse()


# PCRを適用したデータセット
# --- step_pca()でPCRを実行
# --- PC1とPC2を比較のため抽出
pca_df <- 
  recipe(solTrainY ~ ., data = df) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_pca(all_predictors(), num_comp = 5) %>%
    prep(training = df, retain = TRUE) %>%
    juice() %>%
    select(PC1, PC2, solTrainY) %>%
    rename(`PCR Component 1` = "PC1", `PCR Component 2` = "PC2") %>%  
    gather(component, value, -solTrainY)


# PLSを適用したデータセット
# --- step_pls()でPLSを実行
# --- PLS1とPLS2を比較のため抽出
pls_df <- 
  recipe(solTrainY ~ ., data = df) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_pls(all_predictors(), outcome = "solTrainY", num_comp = 2) %>%
    prep(training = df, retain = TRUE) %>%
    juice() %>%
    rename(`PLS Component 1` = "PLS1", `PLS Component 2` = "PLS2") %>%
    gather(component, value, -solTrainY)


# プロット比較
# --- PC1/PLS1 PC2/PLS2 は全く違う要素を示していることがわかる
pca_df %>% 
  bind_rows(pls_df) %>%
  ggplot(aes(value, solTrainY)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "lm", se = FALSE,) +
  facet_wrap(~ component, scales = "free") +
  labs(x = "PC Eigenvalues", y = "Response")


