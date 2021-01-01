# ******************************************************************************
# Title     : dictionary
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/30
# URL       : https://embed.tidymodels.org/reference/dictionary.html
# ******************************************************************************


# ＜ポイント＞
# -


# ＜構文＞
# dictionary(.data, outcome, ..., Laplace = 1e-06)




# 1.準備 --------------------------------------------------

library(tidyverse)
library(tidymodels)
library(embed)


# データ確認
mtcars %>% as_tibble()
mtcars %>% glimpse()



# 2.


#
mtcars %>% dictionary(outcome = "am",  gear:carb)


#
mtcars %>%
  select(am, gear:carb) %>%
  add_woe(outcome = "am", gear:carb)
