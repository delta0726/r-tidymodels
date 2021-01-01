# Title     : workflow
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/07
# URL       : https://workflows.tidymodels.org/reference/workflow.html


# ＜ポイント＞
# - ｢モデル定義｣と｢レシピ｣を格納するコンテナオブジェクトを提供する


# ＜構文＞
# - workflow()





# 1. 準備 -------------------------------------------

library(tidyverse)
library(recipes)
library(workflows)




# 2. ワークフロー定義 ------------------------------------

# レシピの作成
# --- ワークフローに導入する場合はprep()はしなくてもよい
# --- prep()はjuice()で結果を取得するために行う処理
rec <-
  recipe(mpg ~ cyl, mtcars) %>%
    step_log(rec, cyl)


# ワークフローの設定
wrk <-
  workflow() %>%
    add_recipe(rec)



# 確認
wrk %>% print()