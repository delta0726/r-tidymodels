# Title     : group_vfold_cv
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/6
# URL       : https://rsample.tidymodels.org/reference/group_vfold_cv.html



# ＜概要＞
# - 指定したグループのみがテストデータとなるようにリサンプリングを行う
# - グループ数の数だけFoldが作成される
# - グループ単位で予測したいタスクに有効
# - Strataの概念は存在しない（グループ指定で分け方がすでに決まっている）


# ＜構文＞
# group_vfold_cv(data, group = NULL, v = NULL, ...)
#
# group    : 中立化したい列名
# v        : foldの数



library(tidyverse)
library(tidymodels)


# データ作成
set.seed(3527)
test_data <- data.frame(id = sort(sample(1:20, size = 80, replace = TRUE)))
test_data$dat <- test_data %>% nrow() %>% runif()


# データ確認
test_data %>% as_tibble()
test_data %>% group_by(id) %>% tally()


# idごとにクロスバリデーション
# --- Foldごとのテストデータに該当するグループのみが割り当てられる
set.seed(5144)
split_by_id <- test_data %>% group_vfold_cv(group = "id")
split_by_id %>% print()
split_by_id$splits[[1]] %>% training() %>% distinct(id)
split_by_id$splits[[1]] %>% testing() %>% distinct(id)


# 関数定義
get_id_left_out <- function(x)
  unique(assessment(x)$id)


# リサンプリングごとのid
# --- 元の順番に並んでいるわけではない
# --- リサンプリングごとに1つのid
split_by_id$splits %>% map_int(get_id_left_out)
split_by_id$splits %>% map_int(get_id_left_out) %>% table()



#
set.seed(5144)
split_by_some_id <- test_data %>% group_vfold_cv(group = "id", v = 7)
held_out <- split_by_some_id$splits %>% map(get_id_left_out)
table(unlist(held_out)
map_int(held_out, length)