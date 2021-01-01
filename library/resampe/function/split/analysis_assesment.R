# Objective : analysis / Assesment
# Title     : TODO
# Created by: Owner
# Created on: 2020/06/24
# URL       : https://rsample.tidymodels.org/reference/as.data.frame.rsplit.html




# ＜ポイント＞
# - rsplitオブジェクトからAnalysisデータとAssessmentデータを取得する
# - analysis()とassessment()はas.data.frame(data = "")のラッパー関数



library(magrittr)
library(rsample)


# データ確認
mtcars %>% print()
mtcars %>% glimpse()


# データ分割
# --- Analysis / Assesment という名称で分割されている
set.seed(104)
folds <- mtcars %>% vfold_cv()
folds %>% print()
folds %>% class()


# データ確認
folds$splits[[1]] %>% print()
folds$splits[[1]] %>% class()


# データ分割
folds$splits[[1]] %>% analysis() %>% as_tibble()
folds$splits[[1]] %>% assessment() %>% as_tibble()


# データ分割
folds$splits[[1]] %>% as.data.frame(data = "analysis") %>% as_tibble()
folds$splits[[1]] %>% as.data.frame(data = "assessment") %>% as_tibble()

