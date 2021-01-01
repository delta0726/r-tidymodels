# Title     : Basic Usage
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/03
# URL       : https://rsample.tidymodels.org/articles/Basics.html


# ＜ポイント＞
# - rsamplesは｢分析データ｣と｢評価データ｣を分割した｢rsplit｣オブジェクトを基本とする
# - リサンプリングはlisted Dataframeで｢rsplit｣オブジェクトの形で提供される
# - tidymodelsはlisted Dataframeで処理が展開される


library(tidyverse)
library(rsample)



# データセット
mtcars %>% rownames_to_column() %>% print()


#%% rsplitオブジェクト ----------------------------------------------------

# データ分割
split_data <- mtcars %>% initial_split()
split_data %>% print()
split_data %>% class()


# データ確認
split_data %>% analysis() %>% as_tibble()
split_data %>% assessment() %>% as_tibble()


# 以下も同様にワークする
split_data %>% training() %>% as_tibble()
split_data %>% testing() %>% as_tibble()



#%% リサンプリング ----------------------------------------------------

# ブートストラップ法
set.seed(8584)
bt_resamples <- mtcars %>% bootstraps(times = 3)
bt_resamples %>% print()
bt_resamples %>% class()


# 要素抽出
# --- rsplitオブジェクト
first_resample <- bt_resamples$splits[[1]]
first_resample %>% print()
first_resample %>% class()


# レコード数の確認
mtcars %>% nrow()
first_resample %>% analysis() %>% nrow()
first_resample %>% assessment() %>% nrow()


# 訓練データの抽出
first_resample %>%
  analysis() %>%
  rownames_to_column() %>%
  arrange(rowname)


# 評価用データの抽出
first_resample %>%
  assessment() %>%
  rownames_to_column() %>%
  arrange(rowname)


