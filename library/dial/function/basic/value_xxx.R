# Title     : value_xxx
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/23
# URL       : https://dials.tidymodels.org/reference/value_validate.html



# value_validate(object, values)
#
# value_seq(object, n, original = TRUE)
#
# value_sample(object, n, original = TRUE)
#
# value_transform(object, values)
#
# value_inverse(object, values)
#
# value_set(object, values)




library(dplyr)

penalty()


penalty() %>% value_set(-4:-1)#> Amount of Regularization  (quantitative)



penalty() %>% range_get()


penalty()#> Amount of Regularization  (quantitative)
#> Transformer:  log-10
#> Range (transformed scale): [-10, 0]penalty() %>% range_get()#> $lower
#> [1] 1e-10
#>
#> $upper
#> [1] 1
#> value_validate(penalty(), 17)#> [1] FALSE
# get a sequence of values
cost_complexity()#> Cost-Complexity Parameter  (quantitative)
#> Transformer:  log-10
#> Range (transformed scale): [-10, -1]cost_complexity() %>% value_seq(4)#> [1] 1e-10 1e-07 1e-04 1e-01cost_complexity() %>% value_seq(4, original = FALSE)#> [1] -10  -7  -4  -1
on_log_scale <- cost_complexity() %>% value_seq(4, original = FALSE)
nat_units <- value_inverse(cost_complexity(), on_log_scale)
nat_units#> [1] 1e-10 1e-07 1e-04 1e-01value_transform(cost_complexity(), nat_units)#> [1] -10  -7  -4  -1
# random values in the range
set.seed(3666)
cost_complexity() %>% value_sample(2)#> [1] 5.533485e-04 1.480162e-05
