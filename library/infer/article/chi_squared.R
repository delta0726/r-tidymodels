# Title     : Tidy Chi-Squared Tests（カイ二乗検定：独立性検定）
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/14
# URL       : https://infer.netlify.app/articles/chi_squared.html






library(tidyverse)
library(tidyquant)
library(infer)


# データロード
data(gss)



# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()



# プロット作成
gss %>%
  ggplot(aes(x = finrela, fill = college)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(type = "qual") +
  theme_tq() +
  labs(x = "finrela: Self-Identification of Income Class",
       y = "Proportion")



# calculate the observed statistic
observed_indep_statistic <-
  gss %>%
    specify(college ~ finrela) %>%
    calculate(stat = "Chisq")


# generate the null distribution using randomization
null_distribution_simulated <-
  gss %>%
    specify(college ~ finrela) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "Chisq")


# generate the null distribution by theoretical approximation
null_distribution_theoretical <-
  gss %>%
    specify(college ~ finrela) %>%
    hypothesize(null = "independence") %>%
    # note that we skip the generation step here!
    calculate(stat = "Chisq")


# visualize the null distribution and test statistic!
null_distribution_simulated %>%
  visualize() +
  shade_p_value(observed_indep_statistic,
                direction = "greater")

# visualize the theoretical null distribution and test statistic!
gss %>%
  specify(college ~ finrela) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") +
  shade_p_value(observed_indep_statistic,
                direction = "greater")



# visualize both null distributions and the test statistic!
null_distribution_simulated %>%
  visualize(method = "both") +
  shade_p_value(observed_indep_statistic,
                direction = "greater")



# calculate the p value from the observed statistic and null distribution
p_value_independence <- null_distribution_simulated %>%
  get_p_value(obs_stat = observed_indep_statistic,
              direction = "greater")

p_value_independence



chisq_test(gss, college ~ finrela)




#%% カイ二乗の適合度検定 -----------------------------------------


# calculating the null distribution
observed_gof_statistic <- gss %>%
  specify(response = finrela) %>%
  hypothesize(null = "point",
              p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6)) %>%
  calculate(stat = "Chisq")



# generating a null distribution, assuming each income class is equally likely
null_distribution_gof <- gss %>%
  specify(response = finrela) %>%
  hypothesize(null = "point",
              p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6)) %>%
  generate(reps = 1000, type = "simulate") %>%
  calculate(stat = "Chisq")


# visualize the null distribution and test statistic!
null_distribution_gof %>%
  visualize() +
  shade_p_value(observed_gof_statistic,
                direction = "greater")



# calculate the p-value
p_value_gof <- null_distribution_gof %>%
  get_p_value(observed_gof_statistic,
              direction = "greater")

p_value_gof



chisq_test(gss,
           response = finrela,
           p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6))


