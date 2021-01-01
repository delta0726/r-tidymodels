# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/03


# ＜ポイント＞
# - 相関が高い系列の多いデータセットで回帰をする場合はPLS回帰が理想的
# - PLSはPCAと同様の手法で潜在変数を抽出して回帰を行う
# - PLSは、PCAとは異なり、PLSコンポーネントを作成するときに結果データも組み込みます
# - PCAと同様にpredictorの分散を最大化することに加え、Outcomeとの間の相関を同時に最大化する



library(caret)
library(rsample)
library(purrr)
library(recipes)
library(pls)
library(ggplot2)
library(DataExplorer)


#%% 準備 -------------------------------------------

# データロード
data(tecator)

# データ準備
colnames(endpoints) <- c("water", "fat", "protein")
colnames(absorp) <- names0(ncol(absorp))
tecator <- cbind(endpoints, absorp) %>% as_tibble()


# データ概要
tecator %>% glimpse()


# 相関分析
# --- ほとんどの変数が完全相関
# --- このままだと回帰に使いにくい
tecator %>% plot_correlation()




#%% 前処理 -------------------------------------------

# レシピ作成
# --- リスト型データフレームで使う場合はprep行わない（prepperで一括）
norm_rec <-
  recipe(water + fat + protein ~ ., data = tecator) %>%
    step_center(everything()) %>%
    step_scale(everything())


# レシピ確認
norm_rec %>% print()


# 相関分析
# --- この時点では相関関係自体は変わらない
norm_rec %>% prep() %>% juice() %>% plot_correlation()


# バリデーションデータの作成
set.seed(57343)
folds <- tecator %>% vfold_cv(repeats = 10)
folds %>% print()


# データ分割
# ---
folds_rec <-
  folds %>%
    mutate(recipes = map(splits, prepper, recipe = norm_rec))



#%% メイン処理 -------------------------------------------


get_var_explained <- function(recipe, ...) {

  # Extract the predictors and outcomes into their own matrices
  y_mat <- juice(recipe, composition = "matrix", all_outcomes())
  x_mat <- juice(recipe, composition = "matrix", all_predictors())

  # The pls package prefers the data in a data frame where the outcome
  # and predictors are in _matrices_. To make sure this is formatted
  # properly, use the `I` function to inhibit `data.frame` from making
  # all the individual columns. `pls_format` should have two columns.
  pls_format <- data.frame(
    endpoints = I(y_mat),
    measurements = I(x_mat)
  )
  # Fit the model
  mod <- plsr(endpoints ~ measurements, data = pls_format)

  # Get the proportion of the predictor variance that is explained
  # by the model for different number of components.
  xve <- explvar(mod)/100

  # To do the same for the outcome, it is more complex. This code
  # was extracted from pls:::summary.mvr.
  explained <- drop(pls::R2(mod, estimate = "train", intercept = FALSE)$val) %>%
    # transpose so that components are in rows
    t() %>%
    as.data.frame() %>%
    # Add the predictor proportions
    mutate(predictors = cumsum(xve) %>% as.vector(),
           components = seq_along(xve)) %>%
    # Put into a tidy format that is tall
    gather(source, proportion, -components)
}



# 予測結果の取得
folds_explain <-
  folds_rec %>%
    mutate(var = map(recipes, get_var_explained))


variance_data <-
  bind_rows(folds_explain[["var"]]) %>%
    filter(components <= 15) %>%
    group_by(components, source) %>%
    summarize(proportion = mean(proportion))


# プロット
variance_data %>%
  ggplot(aes(x = components, y = proportion, col = source)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(legend.position = "top")