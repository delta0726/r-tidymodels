# ****************************************************************************
# Title     : summary
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://tidyposterior.tidymodels.org/reference/summary.posterior.html
# ****************************************************************************


# ＜ポイント＞
# - モデル統計の事後分布の信頼区間を算出する


# ＜構文＞
# summary(object, prob = 0.9, seed = sample.int(10000, 1), ...)




# データロード
data("ex_objects")


# データ確認
posterior_samples %>% print()
posterior_samples %>% group_by(model) %>% tally()


# 信頼区間の算出
posterior_samples %>% summary(prob = 0.9)




#%% 計算証明 ? ------------------------------------

# qt(): 自由度dfのｔ分布上で、確率pに対応するtの値

# 信頼区間(下限)
lower_ci <- function(mean, se, n, conf_level = 0.9){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# 信頼区間(上限)
upper_ci <- function(mean, se, n, conf_level = 0.9){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}


# 検証
posterior_samples %>%
  group_by(model) %>%
  summarise(mean = mean(posterior),
            ssd = sd(posterior),
            count = n()) %>%
  mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(mean, se, count),
         upper_ci = upper_ci(mean, se, count))



posterior_samples %>%
  group_by(model) %>%
  summarise(ci = list(mean_cl_normal(posterior) %>%
                        rename(mean=y, lwr=ymin, upr=ymax))) %>%
  unnest()