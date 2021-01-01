# Title     : 5 visualize
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/visualize.html
#           : https://infer.netlify.app/articles/infer.html



# ＜ポイント＞
# - calculate()で計算した検定統計量のヒストグラムを作成する
# -


# ＜構文＞
# visualize(
#   data,
#   bins = 15,
#   method = "simulation",
#   dens_color = "black",
#   obs_stat = NULL,
#   obs_stat_color = "red2",
#   pvalue_fill = "pink",
#   direction = NULL,
#   endpoints = NULL,
#   endpoints_color = "mediumaquamarine",
#   ci_fill = "turquoise",
#   ...
# )

# data            : calculate()で計算した検定統計量のデータフレーム
# bins            : ヒストグラムのビンの数
# method          : "simulation", "theoretical", or "both"
# dens_color      :
# obs_stat        :
# obs_stat_color  :
# pvalue_fill     :
# direction       :
# endpoints       :
# endpoints_color :
# ci_fill         :


library(tidyverse)
library(infer)


# データロード
data(gss)


# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()



#%% 帰無仮説の分布 -----------------------------------------

null_dist <-
  gss %>%
    specify(response = hours) %>%
    hypothesize(null = "point", mu = 40) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "t")


# プロット
null_dist %>% visualize()



#%% 点推定 -----------------------------------------

point_estimate <-
  gss %>%
    specify(response = hours) %>%
    hypothesize(null = "point", mu = 40) %>%
    calculate(stat = "t")

# find a confidence interval around the point estimate
ci <-
  null_dist %>%
    get_confidence_interval(point_estimate = point_estimate,
                            level = .95,
                            type = "se")

# display a shading of the area beyond the p-value on the plot
null_dist %>%
  visualize() +
  shade_p_value(obs_stat = point_estimate, direction = "two-sided")


null_dist %>%
  visualize() +
  shade_confidence_interval(ci)



#%% 点推定 -----------------------------------------

null_dist_theoretical <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")


visualize(null_dist_theoretical, method = "theoretical")


visualize(null_dist, method = "both")


