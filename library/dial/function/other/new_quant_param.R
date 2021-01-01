# Title     : new_quant_param
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/23
# URL       : https://dials.tidymodels.org/reference/new-param.html



# ＜ポイント＞
# - 新しいハイパーパラメータオブジェクトを定義する際に使用する
# - 既存のハイパーパラメータ関数の内部関数として使用されている



# 関数定義の確認
# --- 内部関数として使用
mtry



# 定義方法
# --- new_quant_param()を内部で使用して、ラッパー関数を定義する
num_subgroups <- function(range = c(1L, 20L), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_subgroups = "# Subgroups"),
    finalize = NULL
  )
}



# 確認
num_subgroups()


# レンジの設定
num_subgroups() %>% range_get()
num_subgroups(range = c(3L, 5L))


# シーケンスの取得
value_seq(num_subgroups(), 5)

