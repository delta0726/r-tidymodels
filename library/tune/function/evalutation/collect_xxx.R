# Title     : collect_metrics / collect_prediction
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/16
# URL       : https://tune.tidymodels.org/reference/collect_predictions.html


# ＜ポイント＞
# - チューニング結果からメトリックや予測値をデータフレーム形式で取得する
# - summarise引数で集計値と元データを選択することができる



# ＜構文＞
# collect_metrics(x, summarize = TRUE)
# collect_predictions(x, summarize = FALSE, parameters = NULL)





library(tidyverse)
library(tidymodels)


# データセット
# --- ames_grid_search（tune_grid）
# --- ames_iter_search（tune_bayes）
# --- ames_wflow
data("example_ames_knn")


# 確認
ames_grid_search %>% print()
ames_iter_search %>% print()
ames_wflow %>% print()



#%% collect_metrics() -----------------------------------------------

# 確認
# --- k近傍法
ames_wflow %>% print()


# パラメータ確認
ames_wflow %>% parameters()
ames_wflow %>% parameters() %>% as_tibble()




# リサンプリング結果
# --- 10回のリサンプリング
# --- 20パターンのグリッドサーチ
ames_grid_search %>% print()
ames_grid_search$.metrics[[1]]


# メトリックの抽出
# --- サマリー出力
ames_grid_search %>% collect_metrics(summarize = TRUE)


# メトリックの抽出
# --- フル出力(200レコード)
ames_grid_search %>% collect_metrics(summarize = FALSE)




#%% collect_predictions() -----------------------------------------------


# ＜ポイント＞
# - チューニングでsave_pred引数をTRUEにすると各foldで予測値を取得することができる
# - collect_predictions()を使うとfoldを結合したデータフレーム形式で取得できる


# データ確認
mtcars %>% as_tibble()
mtcars %>% glimpse()


# レシピ定義
spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = tune("df"))


# モデル定義
lm_mod <-
  linear_reg() %>%
    set_engine("lm")


# リサンプリング
set.seed(93599150)
car_folds <- mtcars %>% vfold_cv(v = 2, repeats = 3)
car_folds %>% print()


# コントロール設定
ctrl <- control_resamples(save_pred = TRUE)


# グリッド設定
grid <- tibble(df = 3:6)
grid %>% print()


# チューニング
# --- 予測値を結果に保存しておく必要がある
resampled <-
  lm_mod %>%
    tune_grid(preprocessor = spline_rec,
              resamples    = car_folds,
              grid         = grid,
              control      = control_resamples(save_pred = TRUE))


# 確認
resampled %>% print()


# 予測値の抽出
# --- collect_predictions()
resampled %>%
  collect_predictions() %>%
  arrange(.row)


# 予測値の抽出
# --- 同様の操作
resampled %>%
  select(id, id2, .predictions) %>%
  unnest() %>%
  arrange(.row)