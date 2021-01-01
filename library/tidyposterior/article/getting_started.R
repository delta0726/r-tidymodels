# Title     : Getting Started
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/11
# URL       : https://tidyposterior.tidymodels.org/articles/Getting_Started.html
#           : https://cran.r-project.org/web/packages/tidyposterior/vignettes/Getting_Started.html


# ＜ポイント＞
#
#


# ＜目次＞
# 1 データ準備
#





# 1 データ準備 -------------------------------------

# ＜ポイント＞
# - クロスバリデーションで出力されたFoldごとのメトリックを使用する
# - モデル間のメトリックの変動は大きい
# - モデル内のメトリックの変動はかなり小さい
#


library(tidyverse)
library(tidyposterior)


# データ準備
data(precise_example)


# データ確認
# --- Foldごとに各種モデルのメトリックが格納されている
precise_example %>% print()
precise_example %>% glimpse()


# ROCのみを抽出
rocs <-
  precise_example %>%
   select(id, contains("ROC")) %>%
   setNames(tolower(gsub("_ROC$", "", names(.))))


# ロング型に変換`
rocs_stacked <-
  rocs %>%
    gather( "model", "statistic", -id)


# 確認
rocs %>% print()
rocs_stacked %>% print()


# プロット
# --- ROCによる精度の違いがモデル間で大きいことが確認できる
# --- ラインは概ね平行であるため、強いリサンプル間リサンプル効果がある可能性がある
# --- パフォーマンスの違いにもかかわらず、モデルごとにほぼ等しい変動がある(変動は大きくない)
rocs_stacked %>%
  ggplot(aes(x = model, y = statistic, group = id, col = id)) +
    geom_line(alpha = .75) +
    theme(legend.position = "none")




# ベイズ事後分布 -------------------------------------

# ＜ポイント＞
# - Foldごとの結果から見てモデル間の差は有意なものかを判定


# ベイズ分析
# --- リサンプリングによる結果の違いが本質的なものかを確認
roc_model <- rocs %>% perf_mod(seed = 2824)
roc_model %>% print()


# 確認
roc_model %>% glimpse()
roc_model$stan



# 事後分布の取得
# --- 事後分析のメトリック値
roc_post <- roc_model %>% tidy()
roc_post %>% head()


# プロット
roc_post %>%
  mutate(model = factor(model, levels = c("glm", "knn", "nnet"))) %>%
  ggplot() +
    geom_point(data = rocs_stacked, aes(x = model, y = statistic), alpha = .5, col = "blue")




#%% モデル比較(結果比較) -------------------------------------

# RMSEの差の事後を計算
glm_v_nnet <- roc_model %>% contrast_models( "nnet", "glm")
glm_v_nnet %>% head()


# サマリー
glm_v_nnet %>% summary(size = 0.02)


# プロット
glm_v_nnet %>% ggplot(size = 0.02)
