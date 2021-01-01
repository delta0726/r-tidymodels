# Title     : Different Bayesian Models
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/11
# URL       : https://tidyposterior.tidymodels.org/articles/Different_Bayesian_Models.html
#           : https://cran.r-project.org/web/packages/tidyposterior/vignettes/Different_Bayesian_Models.html






library(tidyverse)
library(tidyposterior)


# データロード
data("noisy_example")


#%% 準備 -------------------------------------


# データ準備
# --- ばらつきがある回帰モデルの結果のデータ
# --- 二乗平均平方根誤差(RMSE)メトリックを使用してモデルを比較
rmses <-
  noisy_example %>%
   select(id, id2, contains("RMSE")) %>%
   setNames(tolower(gsub("_RMSE$", "", names(.))))


# ロング型に変換
stacked_rmse <-
  rmses %>%
    gather(key = model, value = statistic, -id, -id2)


# 確認
rmses %>% print()
stacked_rmse %>% print()


# モデルごとに平均
mean_rmse <-
  stacked_rmse %>%
    group_by(model) %>%
    summarise(statistic = mean(statistic))


# 確認
mean_rmse %>% print()
stacked_rmse %>% nrow()


# プロット作成
# --- モデルごとに統計値の水準プロット
# --- 線の多くが交差しているため、リサンプル間の変動はモデル間の変動よりも大きい可能性がある
stacked_rmse %>%
  ggplot(aes(x = model, y = statistic,
             group = paste(id, id2), col = paste(id, id2))) +
  geom_line(alpha = .75) +
  theme(legend.position = "none")


# プロット作成
# --- モデルごとに統計量の密度プロット
# --- 変動性を考慮して、ゼロの漸近線に近づいている右に歪んだ分布を示している
stacked_rmse %>%
  ggplot(aes(col = model, x = statistic)) +
    geom_line(stat = "density", trim = FALSE) +
    theme(legend.position = "top")



#%% 最初のモデル -------------------------------------

# ベイズ分析
gamma_model <-
  rmses %>%
    perf_mod(family = Gamma(), seed = 74)


# 信頼区間の算出
gamma_post <- gamma_model %>% tidy(seed = 3750)
gamma_mean <- gamma_post %>% summary()
gamma_mean %>% print()


# プロット
gamma_post %>%
  ggplot() +
  geom_point(data = gamma_mean, aes(y = mean), alpha = .5) +
  geom_point(data = mean_rmse, aes(y = statistic),
             col = "red", pch = 4, cex= 3)



#%% データの変換 -------------------------------------

# ベイズ分析
log_linear_model <-
  rmses %>%
    perf_mod(transform = ln_trans, seed = 74)


# 信頼区間の算出
log_linear_post <- log_linear_model %>% tidy(seed = 3750)
log_linear_mean <- log_linear_post %>% summary()
log_linear_mean %>% print()


# プロット
ggplot(log_linear_post) +
  geom_point(data = log_linear_mean, aes(y = mean), alpha = .5) +
  geom_point(data = mean_rmse, aes(y = statistic),
             col = "red", pch = 4, cex= 3)




#%% 単純なガウスモデル -------------------------------------

# ベイズ分析
linear_model <-
  rmses %>%
    perf_mod(seed = 74)


# 信頼区間の算出
linear_post <- linear_model %>% tidy(seed = 3750)
linear_mean <- linear_post %>% summary()
linear_mean %>% print()

# プロット

linear_post %>%
  ggplot() +
  geom_point(data = linear_mean, aes(y = mean), alpha = .5) +
  geom_point(data = mean_rmse, aes(y = statistic),
             col = "red", pch = 4, cex= 3)



all_contrasts <-
  linear_model %>%
    contrast_models(seed = 8967)


# プロット
all_contrasts %>%
  ggplot(size = 1)


