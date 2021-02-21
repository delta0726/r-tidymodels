# ********************************************************************************
# Title   : 一般化低ランクモデル(GLRM)
# Theme   : Idea
# Chapter : 18
# URL     : https://bradleyboehmke.github.io/HOML/GLRM.html
# Support : https://koalaverse.github.io/homlr/notebooks/18-glrm.nb.html
# H2O　   : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glrm.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(h2o)
library(Hmisc)



# 使用データ
mtcars %>% as_tibble()


mtcars %>% glimpse()
