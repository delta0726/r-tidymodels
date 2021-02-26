# *****************************************************************************
# Title     : linear_reg
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/linear_reg.html
# *****************************************************************************


linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  translate()



linear_reg() %>%
  set_engine("glmnet") %>%
  set_mode("regression") %>%
  translate()