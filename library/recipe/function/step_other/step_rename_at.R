# Title     : step_rename_at
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/26
# URL       : https://recipes.tidymodels.org/reference/step_rename.html



# ＜ポイント＞
# - dplyr::rename_at()を使って列名変更を行う
# - 一括で名前を変更する場合に便利



# ＜構文＞
# step_rename_at(
#   recipe,
#   ...,
#   fn,
#   role = "predictor",
#   trained = FALSE,
#   inputs = NULL,
#   skip = FALSE,
#   id = rand_id("rename_at")
# )




# 1. 列名を一括修正 -------------------------------------------

# レシピ作成
rec <-
  recipe(~ ., data = iris) %>%
    step_rename_at(everything(), fn = ~ gsub(".", "_", ., fixed = TRUE))


# 列名確認
iris %>% names()
rec %>% prep() %>% juice() %>% names()

