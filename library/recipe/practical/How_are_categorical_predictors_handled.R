# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/03


# ＜課題＞
# - 頻度がまれな分類問題はオーバーフィッティングしやすい
# - 多くの場合、ほとんどのモデルは過半数のクラスに適合しすぎる
# - 出現頻度の高いクラスを含むクラスの非常に優れた統計を生成
# - 少数派のクラスはパフォーマンスが低下します。


# ＜ポイント＞
# - サブサンプリングは、まれにしか発生しない分類データを処理するのに役立つアプローチ
# - 少数派クラスと同じ頻度で発生するまで、多数派クラスのデータをサンプリングする
# - 直感に反するが、データの大部分を破棄することが結果を生成するのに効果的
# - レシピではstep_downsampleが用意されている


# ＜注意点＞
# - ダウンサンプリングは、トレーニングセットのみで実行することを目的としています
# - このため、デフォルトはskip = TRUE
# - prep(recipe, retain = TRUE)レシピを準備するときに使用することを推奨
# - juice()を適用して、データのダウンサンプリングされたバージョンを取得





library(magrittr)
library(tidyverse)
library(tidymodels)
library(tidyposterior)
library(caret)
library(MASS)

set.seed(244)


# データロード
imbal_data <- twoClassSim(1000, intercept = 10)


# データ概要
# --- Y: Class
imbal_data %>% glimpse()


# データ頻度の確認
# --- Class1が圧倒的に少ない（出現率5％程度）
imbal_data$Class %>% table()


#%% 前処理 -----------------------------------------------

# レシピ作成
# --- この時点でprep()はしない
imbal_rec <-
  recipe(Class ~ ., data = imbal_data) %>%
  step_downsample(Class)


# データのリサンプリング
set.seed(5732)
cv_folds <- imbal_data %>% vfold_cv(strata = "Class", v = 10, repeats = 5)


# レシピ適用
# ---prepper()でリストごとのレシピを一括prep
cv_folds <-
  cv_folds %>%
  mutate(recipes = map(splits, prepper, recipe = imbal_rec))


# 確認
cv_folds$recipes[[1]]



#%% モデル構築＆学習＆評価 -----------------------------------------------


# 処理プロセス
# --- qdaでモデリング
# --- レシピを適用してデータ作成
# --- 学習＆予測
# --- 結果抽出
assess_res <- function(split, rec = NULL, ...) {
  if (!is.null(rec))
    mod_data <- juice(rec)
  else
    mod_data <- analysis(split)

  mod_fit <- qda(Class ~ ., data = mod_data)

  if (!is.null(rec))
    eval_data <- bake(rec, assessment(split))
  else
    eval_data <- assessment(split)

  eval_data <- eval_data
  predictions <- predict(mod_fit, eval_data)
  eval_data %>%
    mutate(
      pred = predictions$class,
      prob = predictions$posterior[,1]
    ) %>%
    dplyr::select(Class, pred, prob)
}



# 結果：レシピ適用なし
# --- ほとんどがゼロ
cv_folds$splits[[1]] %>%
  assess_res() %>%
  mutate(prob = round(prob, 3)) %>%
  head()

# 結果：レシピ適用あり
# --- 判定できている
cv_folds$splits[[1]] %>%
  assess_res(cv_folds$recipes[[1]]) %>%
  mutate(prob = round(prob, 3)) %>%
  head()


# 上記の処理をtidyデータフレームに実装
cv_folds <-
  cv_folds %>%
    mutate(sampled_pred = map2(splits, recipes, assess_res),
           normal_pred  =  map(splits, assess_res))


# 確認
cv_folds %>% print()



#%% モデル評価 -----------------------------------------------

# 統計値の追加
# --- sampled_pred: サンプリングあり
# --- normal_pred : サンプリングなし
cv_folds <-
  cv_folds %>%
    mutate(sampled_roc =
             map_dfr(sampled_pred, roc_auc, Class, prob) %>%
               pull(".estimate"),
           normal_roc =
             map_dfr(normal_pred,  roc_auc, Class, prob) %>%
               pull(".estimate"),
           sampled_J =
             map_dfr(sampled_pred, j_index, Class, pred) %>%
               pull(".estimate"),
           normal_J =
             map_dfr(normal_pred,  j_index, Class, pred) %>%
               pull(".estimate"))


# プロット
cv_folds %>%
  ggplot(aes(x = (sampled_roc + normal_roc)/2,
             y = sampled_roc - normal_roc)) +
    geom_point() +
    geom_hline(yintercept = 0, col = "green")


cv_folds %>%
  ggplot(aes(x = (sampled_J + normal_J)/2,
             y =  sampled_J - normal_J)) +
    geom_point() +
    geom_hline(yintercept = 0, col = "green")



#%% 事後確率の分析 -----------------------------------------------

# Remove all columns except the resample info and the J indices,
# then fit the Bayesian model
j_mod <-
  cv_folds %>%
  dplyr::select(-recipes, -matches("pred$"), -matches("roc$")) %>%
  perf_mod(seed = 62378, iter = 5000)




j_mod %>%
  tidy(seed = 234) %>%
  ggplot()



