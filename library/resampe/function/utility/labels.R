# Title     : labels
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/13
# URL       : https://rsample.tidymodels.org/reference/labels.rset.html



# ＜ポイント＞
# - rsplitオブジェクトからid列をベクトルで抽出



# ＜構文＞
# labels(object, make_factor = FALSE, ...)



library(tidyverse)
library(tidymodels)



# データ確認
mtcars %>% print()
mtcars %>% glimpse()



#%% vfold_cvオブジェクトのケース --------------------------------------

# バリデーションデータの作成
set.seed(28432)
fold_rs <- mtcars %>% vfold_cv()
fold_rs %>% print()


# ラベル抽出
fold_rs %>% labels()


# id列をそのまま抽出しても同じ
fold_rs %>%
  mutate(label = map(splits, labels)) %>%
  select(id, label) %>%
  unnest()



#%% vfold_cvオブジェクトのケース --------------------------------------

# 単独のデータ分割
splits <- mtcars %>% initial_split()
splits %>% print()

# ラベル抽出
splits %>% labels()


