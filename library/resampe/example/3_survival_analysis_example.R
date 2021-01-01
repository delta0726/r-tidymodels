# Title     : Survival Analysis Example
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/13
# URL       : https://rsample.tidymodels.org/articles/Applications/Survival_Analysis.html


# ＜ポイント＞
# - モンテカルロ・リサンプリングを使う
# - 複数のモデルパターンを学習するノウハウを得る



library(tidyverse)
library(tidymodels)
library(survival)


# データ確認
# --- 肺がんに関するデータ
lung %>% as_tibble()
lung %>% glimpse()



#%% モデリング ----------------------------------------------------------

# モデル構築
lung_mod <- survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), data = lung)
lung_mod %>% summary()


# 係数の取得
lung_mod %>% coef()


#%% データ作成 ----------------------------------------------------------

# リサンプリング
# --- モンテカルロ・リサンプリング
set.seed(9666)
mc_samp <- lung %>% mc_cv(strata = "status", times = 100)
mc_samp %>% print()



#%% モデリング ----------------------------------------------------------

# 関数定義
# --- ステータス1の割合を計算（感染率）
cens_rate <- function(x) mean(analysis(x)$status == 1)


# リストごとに感染率を計算
mc_samp$splits %>%
  map_dbl(cens_rate) %>%
  summary()


# モデルパターンの設定
three_fact <- as.formula(Surv(time, status) ~ ph.ecog + age + strata(sex))
rm_ph.ecog <- as.formula(Surv(time, status) ~           age + strata(sex))
rm_age     <- as.formula(Surv(time, status) ~ ph.ecog +       strata(sex))
rm_sex     <- as.formula(Surv(time, status) ~ ph.ecog + age              )



#%% 学習 ----------------------------------------------------------

# 関数定義
# --- モデル学習
mod_fit <- function(x, form, ...)
  survreg(form, data = analysis(x), ...)


# モデルパターンごとに学習
mc_samp$mod_full    <- mc_samp$splits %>% map(mod_fit, form = three_fact)
mc_samp$mod_ph.ecog <- mc_samp$splits %>% map(mod_fit, form = rm_ph.ecog)
mc_samp$mod_age     <- mc_samp$splits %>% map(mod_fit, form = rm_age)
mc_samp$mod_sex     <- mc_samp$splits %>% map(mod_fit, form = rm_sex)


#%% 学習 ----------------------------------------------------------

get_concord <- function(split, mod, ...) {
  pred_dat <- assessment(split)
  pred_dat$pred <- predict(mod, newdata = pred_dat)
  concordance(Surv(time, status) ~ pred, pred_dat, ...)$concordance
}


#
mc_samp$full    <- mc_samp$splits %>% map2_dbl(mc_samp$mod_full, get_concord)
mc_samp$ph.ecog <- mc_samp$splits %>% map2_dbl(mc_samp$mod_ph.ecog, get_concord)
mc_samp$age     <- mc_samp$splits %>% map2_dbl(mc_samp$mod_age, get_concord)
mc_samp$sex     <- mc_samp$splits %>% map2_dbl(mc_samp$mod_sex, get_concord)


concord_est <- mc_samp %>%
  dplyr::select(-matches("^mod"))


concord_est %>%
  gather() %>%
  ggplot(aes(x = statistic, col = model)) +
  geom_line(stat = "density") +
  theme_bw() +
  theme(legend.position = "top")




concord_est <- perf_mod(concord_est, seed = 6507, iter = 5000)

ggplot(tidy(concord_est)) +
  theme_bw()


comparisons <- contrast_models(
  concord_est,
  list_1 = rep("full", 3),
  list_2 = c("ph.ecog", "age", "sex"),
  seed = 4654
  )



ggplot(comparisons, size = 0.05) +
  theme_bw()


summary(comparisons, size = 0.05) %>%
  dplyr::select(contrast, starts_with("pract"))