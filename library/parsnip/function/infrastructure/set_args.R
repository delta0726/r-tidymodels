# Title     : set_args
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/set_args.html



# ＜ポイント＞
# - モデルの引数を設定する
# - 引数には｢ハイパーパラメータ｣と｢モデル独自の引数｣が含まれる


# ＜構文＞
# set_args(object, ...)




# 通常の指定方法
# --- メイン関数でハイパーパラメータとモードを指定する
# --- set_engine()でライブラリ、他の引数を設定する
rand_forest(mtry = 3, mode = "regression") %>%
  set_engine("ranger", importance = TRUE)


# 事後的に指定
# --- set_args()でハイパーパラメータとモデル独自の引数を設定
rand_forest() %>%
  set_args(mtry = 3, importance = TRUE) %>%
  set_mode("regression")



# 参考：set_*()を使った設定
rf_model <-
  rand_forest() %>%
    set_args(mtry = 5, trees = 100) %>%
    set_engine("ranger") %>%
    set_mode("classification")