# ******************************************************************************
# Title     : add_woe
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://embed.tidymodels.org/reference/add_woe.html
# ******************************************************************************

# ＜ポイント＞
# - 特定のバイナリ結果に対して予測変数のセットのWoEバージョンをプラグインするための整然とした方法


# ＜構文＞
# add_woe(.data, outcome, ..., dictionary = NULL, prefix = "woe")





# 1.準備 --------------------------------------------------

library(tidyverse)
library(tidymodels)
library(embed)


# データ確認
mtcars %>% as_tibble()
mtcars %>% glimpse()



# 2.


#
mtcars %>% add_woe(outcome = "am", dictionary = , gear:carb)



#
mtcars %>%
  select(am, cyl, gear:carb) %>%
  add_woe("am", cyl, gear:carb)
