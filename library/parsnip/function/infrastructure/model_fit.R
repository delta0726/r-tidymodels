# Title     : model_fit
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/model_fit.html



# ＜ポイント＞
# -



# Keep the `x` matrix if the data are not too big.
spec_obj <-
  linear_reg() %>%
  set_engine("lm", x = ifelse(.obs() < 500, TRUE, FALSE))


spec_obj
spec_obj %>% class()



fit_obj <- spec_obj %>% fit(mpg ~ ., data = mtcars)
fit_obj



nrow(fit_obj$fit$x)#> [1] 32