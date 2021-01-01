# Title     : initial_split
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/initial_split.html



# ＜ポイント＞
# - 断面データを訓練データとテストデータに分割する（rsplitオブジェクト）
# - {rsample}で行われるリサンプルは全てrsplitオブジェクトで展開される



library(rsample)


# データ分割
# --- classの割合が再現されるようにサンプリングする
set.seed(123)
iris_split_1 <- iris %>% initial_split(prop = 3/4)
iris_split_2 <- iris %>% initial_split(prop = 3/4, strata = Species)


# 確認
iris_split_1
iris_split_2


# 分割データの格納
iris_train_1 <- iris_split_1 %>% training()
iris_train_2 <- iris_split_2 %>% training()
iris_test_1  <- iris_split_1 %>% testing()
iris_test_2  <- iris_split_2 %>% testing()


# レコード数
iris_train_1 %>% nrow()
iris_train_2 %>% nrow()
iris_test_1 %>% nrow()
iris_test_2 %>% nrow()


# Speciesの割合
# --- 元の割合は保存されない
round(table(iris_train_1$Species) / nrow(iris), 3)
round(table(iris_test_1$Species) / nrow(iris), 3)


# Speciesの割合
# --- 元の割合が保存されている
round(table(iris_train_2$Species) / nrow(iris), 3)
round(table(iris_test_2$Species) / nrow(iris), 3)

