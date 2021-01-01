# Title     : tune_grid / control_grid
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/16
# URL       : https://tune.tidymodels.org/reference/tune_grid.html
#           : https://tune.tidymodels.org/reference/control_grid.html




# ＜ポイント＞
# - リサンプルデータに対してワークフローの定義に従ってチューニングパラメータのメトリックを計算する
# - チューニングは｢モデル｣｢レシピ｣の両方に対して行うことができる
# - yardstick::metric_set()を使うことで複数メトリックでチューニングすることが可能



# ＜参考＞
# http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
# blog_Rebecca_20200414.R



# ＜構文＞
# tune_grid(
#   workflow,
#   resamples,
#   ...,
#   param_info = NULL,
#   grid = 10,
#   metrics = NULL,
#   control = control_grid()
# )


# ＜コントロール変数＞
# control_grid(
#   verbose = FALSE,
#   allow_par = TRUE,
#   extract = NULL,
#   save_pred = FALSE,
#   pkgs = NULL
# )




library(tidyverse)
library(tidymodels)


# データ確認
mtcars %>% as_tibble()


# バリデーションデータの作成
set.seed(6735)
folds <- mtcars %>% vfold_cv(v = 5)
folds %>% print()



#%% レシピのチューニング -------------------------------------------------

# レシピ定義
# --- スプラインの自由度をチューニング
spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = tune("disp")) %>%
  step_ns(wt, deg_free = tune("wt"))


# モデル定義
lin_mod <-
  linear_reg() %>%
  set_engine("lm")


# グリッド設定
# --- 手動
spline_grid <- expand.grid(disp = 2:5, wt = 2:5)
spline_grid %>% print()


# チューニング
spline_res <-
  lin_mod %>%
    tune_grid(preprocessor = spline_rec,
              resamples    = folds,
              grid         = spline_grid)

# 確認
spline_res %>% print()
spline_res %>% names()


# メトリックの取得
spline_res %>% collect_metrics()



# 上位の組み合わせ
spline_res %>% show_best(metric = "rmse")




#%% モデルのチューニング -------------------------------------------------

# レシピ定義
car_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())


# モデル定義
# --- ハイパーパラメータのチューニング
svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")


# Use a space-filling design with 7 points
set.seed(3254)
svm_res <-
  svm_mod %>%
    tune_grid(preprocessor = car_rec,
              resamples    = folds,
              grid         = 7)



# 確認
svm_res %>% print()
svm_res %>% collect_metrics()



# 上位の組み合わせ
svm_res %>% show_best(metric = "rmse")


# プロット
svm_res %>%
  autoplot(metric = "rmse") +
  scale_x_log10()

