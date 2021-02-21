#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 11 Comparing models with resampling
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/18
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜概要＞
# - リサンプリングによるモデル精度の評価を行う
# - 統計的検定やベイズ分析の観点からモデル優位性があるかを検証する


# ＜目次＞
# 0 準備
# 1 モデル精度比較（レシピ設定）
# 2 モデル精度比較（モデル構築）
# 3 モデル精度比較（結果）
# 4 モデル精度比較（相関分析）
# 5 単純な仮説検定による評価
# 6 ベイズ手法による評価


# 0 準備 -------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(corrr)
library(tidyposterior)
library(rstanarm)


# データロード
data(ames)

# データ加工
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# データ分割
set.seed(123)
ames_split <- ames %>% initial_split(prob = 0.80, strata = Sale_Price)
ames_train <- ames_split %>% training()
ames_test  <- ames_split %>% testing()


# 1 モデル精度比較（レシピ設定） ----------------------------------------------

# レシピ作成
# --- スプラインあり
ames_rec_spline <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Latitude, Longitude, deg_free = 20)

# レシピ作成
# --- スプラインなし
ames_rec_no_spline <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )


# 2 モデル精度比較（モデル構築） ----------------------------------------------

# 共通設定 *************************************************

# リサンプリングデータの作成
set.seed(55)
ames_folds <- ames_train %>% vfold_cv(v = 10)

# リサンプリング設定
keep_pred <- control_resamples(save_pred = TRUE)


# モデル1： ランダムフォレスト *********************************

# モデル定義
rf_model <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# ワークフロー設定
# --- レシピなし
rf_wflow <-
  workflow() %>%
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
      Latitude + Longitude) %>%
  add_model(rf_model)

# リサンプリング予測
set.seed(130)
rf_res <-
  rf_wflow %>%
    fit_resamples(resamples = ames_folds, control = keep_pred)


# モデル2： 線形回帰モデル ************************************

# モデル定義
lm_model <-
  linear_reg() %>%
    set_engine("lm")

# ワークフロー設定
lm_wflow_spline <-
  workflow() %>%
  add_model(lm_model) %>%
  add_recipe(ames_rec_spline)

# リサンプリング予測
lm_res_with_splines <-
  lm_wflow_spline %>%
   fit_resamples(resamples = ames_folds, control = keep_pred)


# モデル3： スプラインモデル ************************************

# モデル定義
# --- モデル2(線形回帰モデル)と同様
lm_model <-
  linear_reg() %>%
    set_engine("lm")

# ワークフロー設定
lm_wflow_no_spline <-
  workflow() %>%
    add_model(lm_model) %>%
    add_recipe(ames_rec_no_spline)

# リサンプリング予測
lm_res_no_splines <-
  lm_wflow_no_spline %>%
    fit_resamples(resamples = ames_folds, control = keep_pred)


# 3 モデル精度比較（結果） ----------------------------------------------

# メトリック出力
# --- スプライン項を追加することはモデル精度の改善に寄与していない
rf_res %>% collect_metrics()
lm_res_with_splines %>% collect_metrics()
lm_res_no_splines %>% collect_metrics()


# 4 モデル精度比較（相関分析） ----------------------------------------------

# モデル1
# --- ランダムフォレスト
rf_rsq <-
  rf_res %>%
    collect_metrics(summarize = FALSE) %>%
    filter(.metric == "rsq") %>%
    select(id, `random forest` = .estimate)

# モデル2
# --- 線形回帰モデル（スプライン）
lm_rsq_splines <-
  lm_res_with_splines %>%
    collect_metrics(summarize = FALSE) %>%
    filter(.metric == "rsq") %>%
    select(id, `with splines` = .estimate)

# モデル3
# --- 線形回帰モデル（スプラインなし）
lm_rsq_no_splines <-
  lm_res_no_splines %>%
    collect_metrics(summarize = FALSE) %>%
    filter(.metric == "rsq") %>%
    select(id, `no splines` = .estimate)

# 結合
rsq_estimates <-
  rf_rsq %>%
    inner_join(lm_rsq_splines, by = "id") %>%
    inner_join(lm_rsq_no_splines, by = "id")

# 相関係数行列
# --- モデル間でメトリックの相関は高い
rsq_estimates %>%
  select(-id) %>%
  correlate()

# プロット作成
# --- モデルごとの同一FoldでRSQを比較
rsq_estimates %>%
  pivot_longer(cols = c(-id), names_to = "model", values_to = "rsq") %>%
  mutate(model = reorder(model, rsq)) %>%
  ggplot(aes(x = model, y = rsq, group = id, col = id)) +
  geom_line(alpha = .5, lwd = 1.25) +
  theme(legend.position = "none") +
  labs(x = NULL, y = expression(paste(R^2, "statistics")))

# 相関の統計的検定
rsq_estimates %>%
  with( cor.test(`no splines`, `random forest`) ) %>%
  tidy() %>%
  select(estimate, starts_with("conf"))


# 5 単純な仮説検定による評価 ---------------------------------------------------------

# データ加工
# --- 4で作成したデータを利用
# --- スプラインの｢あり/なし｣で比較するため差分系列を追加
compare_lm <-
  rsq_estimates %>%
    mutate(difference = `with splines` - `no splines`)

# 信頼区間の算出
# --- 線形回帰による方法
lm(difference ~ 1, data = compare_lm) %>%
  tidy(conf.int = TRUE) %>%
  select(estimate, p.value, starts_with("conf"))

# 信頼区間の算出
# --- t検定による方法
rsq_estimates %>%
  with( t.test(`with splines`, `no splines`, paired = TRUE) ) %>%
  tidy() %>%
  select(estimate, p.value, starts_with("conf"))


# 6 ベイズ手法による評価 --------------------------------------------------------------

# データ結合
# --- 元のリサンプルデータ + リサンプリング結果
ames_two_models <-
  ames_folds %>%
    bind_cols(rsq_estimates %>% arrange(id) %>% select(-id))

# 確認
ames_two_models %>% slice(1:4)



# The rstanarm package creates copious amounts of output; those results
# are not shown here but are worth inspecting for potential issues.
rsq_anova <-
  ames_two_models %>%
    perf_mod(prior_intercept = student_t(df = 1),
             chains = 4,
             iter = 5000,
             seed = 2)

model_post <-
  rsq_anova %>%
    tidy(seed = 35) %>%
    as_tibble()


model_post %>% glimpse()


model_post %>%
  mutate(model = fct_inorder(model)) %>%
  ggplot(aes(x = posterior)) +
  geom_histogram(bins = 50, col = "white", fill = "blue", alpha = 0.4) +
  facet_wrap(~ model, ncol = 1) +
  labs(x = expression(paste("Posterior for mean ", R^2)))


rqs_diff <-
  rsq_anova %>%
    contrast_models(list_1 = "with splines",
                    list_2 = "no splines",
                    seed = 36)

rqs_diff %>%
  as_tibble() %>%
  ggplot(aes(x = difference)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_histogram(bins = 50, col = "white", fill = "red", alpha = 0.4) +
  labs(x = expression(paste("Posterior for mean difference in ", R^2,
                            " (splines - no splines)")))

rqs_diff %>%
  summary() %>%
  select(-starts_with("pract"))

rqs_diff %>%
  summary(size = 0.02) %>%
  select(contrast, starts_with("pract"))
