# Title     : add_rowiondex
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/add_rowindex.html



# ＜ポイント＞
# - データフレームに行番号を追加する




# データフレームに行番号を追加
# --- 列名は｢.row｣
iris %>%
  add_rowindex() %>%
  as_tibble()




