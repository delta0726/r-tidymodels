# Title     : 1. モデルを構築する
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/17
# URL       : https://www.tidymodels.org/start/models/


# ＜ポイント＞
# - 従来のlm()とparsnipのliner_reg()の考え方の違いを学ぶ
# - parsnipはモデル式とモデル設定を分離することで、モデル切替が柔軟にできるように設計されている
# - モデル式は｢fit関数｣｢レシピ｣｢add_formula(ワークフロー)｣などで入力する


# ＜目次＞
# 1: 準備
# 2: モデリング
# 3 予測
# 4 ベイジアン・アプローチ



# 1: 準備 ---------------------------


library(magrittr)
library(tidyverse)
library(tidymodels)
library(readr)
library(rstanarm)


# データ取得
# --- ウニの大きさに関するデータセット
urchins <-
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
    set_colnames(c("food_regime", "initial_volume", "width")) %>%
    mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))


# データ概要
urchins %>% print()
urchins %>% glimpse()


# プロット
# --- 摂食条件でで水準や傾きに差があるように見える
urchins %>%
  ggplot(aes(x = initial_volume,
             y = width,
             group = food_regime,
             col = food_regime)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    scale_color_viridis_d(option = "plasma", end = .7)



# 2: モデリング ---------------------------

# フォーミュラの定義
# Y: width
# X: food_regime(カテゴリ情報を含める)
width ~ initial_volume * food_regime


# モデル構築
# --- モデリングではFormulaを含めない
lm_mod <-
  linear_reg() %>%
  set_engine("lm")


# モデル学習
# --- Formulaは学習の段階で定義（モデルを独立させて使いまわせるようにするため）
# --- Recipeを導入してもFormula定義が必要(workflowを導入するとrecipeの定義を使える)
# --- 相互効果を考慮する
lm_fit <-
  lm_mod %>%
    fit(width ~ initial_volume * food_regime, data = urchins)


# モデルのパラメータを取得
lm_fit %>% tidy()



# 3 予測 ---------------------------

# ＜ポイント＞
# - predict()の出力結果は元データと同じ行数で並び順も同一であることが保証される
#   --- bind_cols()で簡単に結合できる
# - predict()はtype引数により様々な出力が可能
#   --- 回帰問題："numeric", "conf_int", "pred_int", "quantile", or "raw"
#   --- 分類問題："class", "prob"


# 新しいデータポイント
# --- 同じinitial_volumeを使ってfood_regimeだけを変更する
new_points <-
  expand.grid(initial_volume = 20,
              food_regime = c("Initial", "Low", "High"))


# 確認
new_points %>% print()


# 予測
# --- type引数を指定しないとデフォルト出力
# --- モデルが回帰問題なら"numeric"、分類問題なら"class"
mean_pred <- lm_fit %>% predict(new_data = new_points)
mean_pred %>% print()


# Predict
# --- 信頼区間を出力する
conf_int_pred <- lm_fit %>% predict(new_data = new_points, type = "conf_int")
conf_int_pred %>% print()


# データ作成
# ---- プロット用
plot_data <-
  new_points %>%
    bind_cols(mean_pred) %>%
    bind_cols(conf_int_pred) %>%
    mutate(type = "Simple")


# プロット
# --- 予測値と信頼区間
plot_data %>%
  ggplot(aes(x = food_regime)) +
    geom_point(aes(y = .pred)) +
    geom_errorbar(aes(ymin = .pred_lower,
                      ymax = .pred_upper),
                  width = .2) +
    labs(y = "urchin size")



# 4 ベイジアン・アプローチ ---------------------------

# ＜ポイント＞
# - モデルがベイジアンアプローチを使用して推定された場合に結果が異なるかどうかを知りたい
#   --- parsnip::linear_reg()で"stan"をエンジンに設定する
# - パラメーターの可能な値を表すモデルパラメーターごとに事前分布を宣言する必要がある
#   --- グループは事前分布がベル型であるべきであることに同意しますが、値の範囲がどうあるべきか誰もわからない
#   --- コーシー分布を使用して保守的に事前分布を広くします


# 事前分布の設定
prior_dist <- student_t(df = 1)


# 乱数設定
set.seed(123)


# モデル構築
# --- エンジンの引数を変えるだけ
# --- {rstanarm}を用いている
bayes_mod <-
  linear_reg() %>%
  set_engine("stan",
             prior_intercept = prior_dist,
             prior = prior_dist)


# モデル学習
bayes_fit <-
  bayes_mod %>%
  fit(width ~ initial_volume * food_regime, data = urchins)


# モデル予測
bayes_fit %>% print(digits = 5)


# パラメータ取得
bayes_fit %>% tidy(intervals = TRUE)


# データ作成
# --- プロット用
bayes_plot_data <-
  new_points %>%
    bind_cols(predict(bayes_fit, new_data = new_points)) %>%
    bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int")) %>%
    mutate(type = "Bayesian")


# プロット
plot_data %>%
  bind_rows(bayes_plot_data) %>%
  ggplot(aes(x = food_regime, group = type, color = type)) +
    geom_point(aes(y = .pred)) +
    geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) +
    facet_wrap(~ type) +
    labs(y = "urchin size") +
    ggtitle("Bayesian model with t(1) prior distribution")


