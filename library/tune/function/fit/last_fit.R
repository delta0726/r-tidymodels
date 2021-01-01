# Title     : last_fit
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://tune.tidymodels.org/reference/last_fit.html


# ＜ポイント＞
# - 以下の一連の動作をtibble形式で行う
# - 1.rsplitデータの訓練データで学習して、テストデータで評価してモデルを構築する
# - 2.｢.metrics｣には、モデル評価のメトリックが表示される
# - 3.｢.prediction｣には、テストデータの予測値が格納される
# - 4.workflowオブジェクトも同時に出力される


# ＜その他＞
# - ハイパーパラメータのチューニングとは関係なさそう



# ＜構文＞
# - last_fit(object, split, ..., metrics = NULL)
# - object : workflow



library(tidyverse)
library(recipes)
library(rsample)
library(parsnip)
library(workflows)


# データ確認
mtcars %>% as_tibble()
mtcars %>% glimpse()


# データ分割
set.seed(6735)
tr_te_split <- mtcars %>% initial_split()
tr_te_split %>% print()


# レシピ作成
spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
    step_ns(disp)


# モデリング
lin_mod <-
  linear_reg() %>%
    set_engine("lm")



#%% ワークフローなし -------------------------------------------

# モデル学習
# --- データにはrsplitオブジェクトを与える
# --- モデルのフィッティングと予測が同時に行われる
spline_res <- lin_mod %>% last_fit(spline_rec, split = tr_te_split)
spline_res %>% print()


# 要素の確認
spline_res %>% names()


# 要素へのアクセス
spline_res$splits
spline_res$id
spline_res$.metrics
spline_res$.predictions
spline_res$.workflow


# 要素へのアクセス
# --- よく使う以下の２項目はショートカットも用意されている
spline_res %>% collect_metrics()
spline_res %>% collect_predictions()



#%% ワークフローあり -------------------------------------------

# ワークフローの設定
spline_wfl <-
 workflow() %>%
   add_recipe(spline_rec) %>%
   add_model(lin_mod)


# モデル学習
spline_res2 <- spline_wfl %>% last_fit(split = tr_te_split)
spline_res2 %>% print()


# 要素の確認
spline_res2 %>% names()


# 要素へのアクセス
spline_res$splits
spline_res$id
spline_res$.metrics
spline_res$.predictions
spline_res$.workflow

