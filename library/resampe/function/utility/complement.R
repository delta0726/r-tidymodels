# Title     : complement
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/13
# URL       : https://rsample.tidymodels.org/reference/complement.html



# ＜ポイント＞
# - rsplitオブジェクトにout_idを追加して出力する
# - populate()でも類似操作が可能



# ＜構文＞
# complement(x, ...)




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


# out_idを抽出
# --- populate()でも同様の操作が可能
fold_rs$splits[[1]] %>% complement()
fold_rs$splits[[1]] %>% populate() %>% use_series(out_id)