# Title     : Bootstrap Confidence Intervals
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/13
# URL       : https://rsample.tidymodels.org/articles/Applications/Intervals.html



# ＜ポイント＞
# - リサンプリングデータを{purrr}で操作する
# - purrrでレシピやモデルをtidyに適用する
# - tidymodelsでは{workflow}でリサンプリングデータを一括して扱えるが{purrr}の操作も重要



library(tidymodels)
library(nlstools)
library(GGally)


# データロード
data(O2K)


# データ確認
O2K %>% as_tibble()
O2K %>% names()
O2K %>% glimpse()


# プロット
O2K %>%
  ggplot(aes(x = t, y = VO2)) +
    geom_point()



nonlin_form <-
  as.formula(
    VO2 ~ (t <= 5.883) * VO2rest +
      (t > 5.883) *
      (VO2rest + (VO2peak - VO2rest) * (1 - exp(-(t - 5.883) / mu)))
    )

# Starting values from visual inspection
start_vals <- list(VO2rest = 400, VO2peak = 1600, mu = 1)

res <- nls(nonlin_form, start = start_vals, data = O2K)

tidy(res)

# Will be used to fit the models to different bootstrap data sets:
fit_fun <- function(split, ...) {
  # We could check for convergence, make new parameters, etc.
  nls(nonlin_form, data = analysis(split), ...) %>%
    tidy()
}

set.seed(462)
nlin_bt <-
  bootstraps(O2K, times = 2000, apparent = TRUE) %>%
  mutate(models = map(splits, ~ fit_fun(.x, start = start_vals)))
nlin_bt

nlin_bt$models[[1]]


nls_coef <-
  nlin_bt %>%
  dplyr::select(-splits) %>%
  # Turn it into a tibble by stacking the `models` col
  unnest() %>%
  # Get rid of unneeded columns
  dplyr::select(id, term, estimate)


head(nls_coef)



nls_coef %>%
  # Put different parameters in columns
  tidyr::spread(term, estimate) %>%
  # Keep only numeric columns
  dplyr::select(-id) %>%
  ggscatmat(alpha = .25)


nls_coef %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 20, col = "white") +
  facet_wrap(~ term, scales = "free_x")


p_ints <- int_pctl(nlin_bt, models)
p_ints


nls_coef %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 20, col = "white") +
  facet_wrap(~ term, scales = "free_x") +
  geom_vline(data = p_ints, aes(xintercept = .lower), col = "red") +
  geom_vline(data = p_ints, aes(xintercept = .upper), col = "red")


parametric <-
  tidy(res, conf.int = TRUE) %>%
  dplyr::select(
    term,
    .lower = conf.low,
    .estimate = estimate,
    .upper = conf.high
  ) %>%
  mutate(
    .alpha = 0.05,
    .method = "parametric"
  )

intervals <-
  bind_rows(parametric, p_ints) %>%
  arrange(term, .method)
intervals %>% split(intervals$term)



nls_coef %>%
  ggplot(aes(sample = estimate)) +
  stat_qq() +
  stat_qq_line(alpha = .25) +
  facet_wrap(~ term, scales = "free")



t_stats <- int_t(nlin_bt, models)
intervals <-
  bind_rows(intervals, t_stats) %>%
  arrange(term, .method)
intervals %>% split(intervals$term)



bias_corr <- int_bca(nlin_bt, models, .fn = fit_fun, start = start_vals)
intervals <-
  bind_rows(intervals, bias_corr) %>%
  arrange(term, .method)
intervals %>% split(intervals$term)


fold_incr <- function(split, ...) {
  dat <- analysis(split)
  quants <- quantile(dat$VO2, probs = c(.1, .9))
  tibble(
    term = "fold increase",
    estimate = unname(quants[2]/quants[1]),
    # We don't know the analytical formula for this
    std.error = NA_real_
  )
}


nlin_bt <-
  nlin_bt %>%
  mutate(folds = map(splits, fold_incr))

int_pctl(nlin_bt, folds)


int_bca(nlin_bt, folds, .fn = fold_incr)