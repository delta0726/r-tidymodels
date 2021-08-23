# **********************************************************************************
# Library   : workflowsets
# Function  : workflow_map
# Created by: Owner
# Created on: 2021/08/24
# URL       : https://workflowsets.tidymodels.org/reference/workflow_map.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセット内のワークフロー全体で同じ関数を適用する
#   --- 通常のmap()と同じ考え方



# ＜構文＞
# workflow_map(
#   object,
#   fn = "tune_grid",
#   verbose = FALSE,
#   seed = sample.int(10^4, 1),
#   ...
# )


# ＜使用可能な関数＞
# - tune::tune_grid()
# - tune::tune_bayes()
# - tune::fit_resamples()
# - finetune::tune_race_anova()
# - finetune::tune_race_win_loss()
# - finetune::tune_sim_anneal().