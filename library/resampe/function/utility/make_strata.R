# Title     : make_strata
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/make_strata.html



#
#
#



# ＜構文＞
# make_strata(x, breaks = 4, nunique = 5, pool = 0.1, depth = 20)



library(magrittr)
library(rsample)


# データ作成
# --- ポアソン分布の乱数
set.seed(61)
x1 <- rpois(100, lambda = 5)
x1 %>% print()


# 集計
x1 %>% table()


# 階層の作成
x1_starata <- x1 %>% make_strata()
x1_starata %>% print()
x1_starata %>% table()

