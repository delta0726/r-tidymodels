# Title     : tune_bayes
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/17
# URL       : https://tune.tidymodels.org/reference/tune_bayes.html
#           : https://tune.tidymodels.org/reference/control_grid.html



# ＜構文＞
# tune_bayes(
#   workflow,
#   resamples,
#   ...,
#   iter = 10,
#   param_info = NULL,
#   metrics = NULL,
#   objective = exp_improve(),
#   initial = 5,
#   control = control_bayes()
# )


# ＜コントロール変数＞
#control_bayes(
#  verbose = FALSE,
#  no_improve = 10L,
#  uncertain = Inf,
#  seed = sample.int(10^5, 1),
#  extract = NULL,
#  save_pred = FALSE,
#  time_limit = NA,
#  pkgs = NULL
#)