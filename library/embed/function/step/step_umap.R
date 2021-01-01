# ******************************************************************************
# Title     : step_umap
# Objective : TODO
# Created by: Owner
# Created on: 2020/11/26
# URL       : https://embed.tidymodels.org/reference/step_umap.html
# ******************************************************************************


# ＜ポイント＞
# - 特徴量をより小さなスペースに投影する非線形の次元削減手法の一種
#   --- UMAPは、Uniform Manifold Approximation and Projectionの略
#   --- データの局所的な低次元表現を見つける教師ありの非線形次元削減手法


# ＜構文＞
# step_umap(
#  recipe,
#  ...,
#  role = "predictor",
#  iris_trained = FALSE,
#  outcome = NULL,
#  neighbors = 15,
#  num_comp = 2,
#  min_dist = 0.01,
#  learn_rate = 1,
#  epochs = NULL,
#  options = list(verbose = FALSE, n_threads = 1),
#  seed = sample(10^5, 2),
#  retain = FALSE,
#  object = NULL,
#  skip = FALSE,
#  id = rand_id("umap")
#)



# ＜参考＞
# t-SNEより強いUMAPを（工学的に）理解したい
# https://qiita.com/odanny/items/06ab88353bcee7bf6aa7


# 1 準備
# 2 レシピ作成



# 1 準備 -------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(embed)
library(modeldata)


# データ分割
split <- seq.int(1, 150, by = 9)
iris_tr <- iris[-split, ]
iris_te <- iris[ split, ]



# 2 レシピ作成 -------------------------------------------------------------

# 乱数シード
set.seed(11)


# レシピ作成
rec <-
  recipe(Species ~ ., data = iris_tr) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_umap(all_predictors(), outcome = vars(Species), num_comp = 2) %>%
    prep()


# レシピ適用
iris_baked <-
  rec %>%
    bake(new_data = iris_te, Species, starts_with("umap"))


# 2 レシピ作成 -------------------------------------------------------------

# データ確認
iris_baked %>% print()
iris_te %>% print()


# プロット作成
iris_baked %>%
  ggplot(aes(x = umap_1, y = umap_2, col = Species)) +
  geom_point(alpha = .75, size = 1.5) +
  theme_bw()
