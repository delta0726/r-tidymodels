# Title     : populate
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/13
# URL       : https://rsample.tidymodels.org/reference/populate.html


# ＜ポイント＞
# - rsplitオブジェクトにout_idを追加する
# - complement()でも類似操作が可能



library(tidyverse)
library(tidymodels)



# データ確認
mtcars %>% print()
mtcars %>% glimpse()


# バリデーションデータの作成
set.seed(28432)
fold_rs <- mtcars %>% vfold_cv()
fold_rs %>% print()


# foldの中身
# --- out_idはNA
fold_rs$splits[[1]]
fold_rs$splits[[1]] %>% names()
fold_rs$splits[[1]]$id
fold_rs$splits[[1]]$in_id
fold_rs$splits[[1]]$out_id
fold_rs$splits[[1]]$data


# populate()を適用
# --- 個別のfold
fold_rs$splits[[1]]$out_id
fold_rs$splits[[1]] %>% populate() %>% .$out_id

# populate()を適用
# --- 全体
fold_rs_all <- fold_rs %>% populate()
fold_rs_all$splits[[1]]$out_id