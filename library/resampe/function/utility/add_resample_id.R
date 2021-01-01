# Title     : add_resample_id
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/13
# URL       : https://rsample.tidymodels.org/reference/add_resample_id.html


# ＜ポイント＞
# - リサンプリングIDの列をデータセットに追加する
#   --- リサンプリングデータは元のデータフレームのみで、IDは付いてはない




library(magrittr)
library(rsample)


# データ確認
mtcars %>% print()
mtcars %>% glimpse()



#%% クロスバリデーション -------------------------------------------

# バリデーションデータの作成
set.seed(363)
car_folds <- mtcars %>% vfold_cv(repeats = 3)
car_folds %>% print()


# 個別のデータセットにリサンプリングのIDを追加する
car_folds$splits[[1]] %>%
  analysis() %>%
  add_resample_id(car_folds$splits[[1]]) %>%
  head()



#%% ブートストラップデータ -------------------------------------------

# ブートストラップデータの作成
car_bt <- mtcars %>% bootstraps()
car_bt %>% print()


# 個別のデータセットにリサンプリングのIDを追加する
car_bt$splits[[1]] %>%
  analysis() %>%
  add_resample_id(car_bt$splits[[1]]) %>%
  head()
