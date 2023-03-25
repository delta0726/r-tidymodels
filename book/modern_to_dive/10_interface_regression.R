# **************************************************************************************
# Book      : Modern Dive into R
# Theme     : Ⅲ Statistical Interface with infer
# Title     : Inference for Regression
# Created on: 2020/7/24
# URL       : https://moderndive.com/10-inference-for-regression.html
# **************************************************************************************



# ＜概要＞
# - 線形回帰モデルに統計的推論の考え方を融合する
#   --- 線形回帰の前提となる残差の性質を確認する(独立性/正規性/等分散性)
#   --- {infer}のフレームワークでslopeに対してシミュレーションを行う


# ＜目次＞
# 10-0 準備
# 10-1 線形回帰の実施
# 10-2 回帰分析の解釈
# 10-3 回帰推論の条件


# 10-0 準備 -----------------------------------------------------------


# ライブラリ
library(tidyverse)
library(moderndive)
library(infer)


# データロード
evals %>% print()

# データ抽出
evals_ch5 <- evals %>% select(ID, score, bty_avg, age)

# データ確認
# --- Scoreと2変量(bty_avg / age)
evals_ch5 %>% print()
evals_ch5 %>% glimpse()


# 10-1 線形回帰の実施 ----------------------------------------------------

# プロット作成
# --- 散布図に回帰直線をプロット
# --- 回帰直線は2乗残差の合計を最小化する線として定義
evals_ch5 %>%
  ggplot(aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Relationship between teaching and beauty scores") +
  geom_smooth(method = "lm", se = FALSE)

# 線形回帰の実行
# --- X(bty_avg)が1単位増えるごとにYは0.067だけ増加する
# --- 回帰直線を抽出するのに全サンプル(n=463)で実行している
score_model <- lm(score ~ bty_avg, data = evals_ch5)
score_model %>% get_regression_table()


# 10-2 回帰分析の解釈 -----------------------------------------------------------

# 標準誤差 --------------------------------------------

# - 標準誤差は、サンプルから計算された任意の点推定の標準偏差です。
#
#
#




# 10-3 回帰推論の条件 -----------------------------------------------------------


# ＜回帰推論の前提＞
# LINE原則
# 1. Linearity of relationship between variables（変数間の関係の線形性）
# 2. Independence of the residuals（残差の独立性）
# 3. Normality of the residuals（残差の正規性）
# 4. Equality of variance of the residuals（残差の等分散性）



# 線形回帰の実行
# --- X(bty_avg)が1単位増えるごとにYは0.067だけ増加する
# --- 回帰直線を抽出するのに全サンプル(n=463)で実行している
score_model <- lm(score ~ bty_avg, data = evals_ch5)


# 回帰分析の残差データ
# --- Residual = Actual - Estimate
regression_points <- score_model %>% get_regression_points()
regression_points %>% print()
regression_points %>% mutate(residual2 = score - score_hat)



# 残差の線形性
regression_points %>%
  ggplot(aes(x = bty_avg, y = residual)) +
  geom_point()




# プロット作成（残差の正規性）
# --- 残差は平均ゼロで正規分布の形状である必要がある
# --- 残差のヒストグラムで確認
# --- このデータは残差が負よりも正の方向に傾いている
regression_points %>%
  ggplot(aes(x = residual)) +
    geom_histogram(binwidth = 0.25, color = "white") +
    labs(x = "Residual")



# プロット作成（残差の等分散性）
# --- 残差のばらつきは変数Xの水準によらず一定である必要がある
# --- 説明変数と残差の散布図で確認
regression_points %>%
  ggplot(aes(x = bty_avg, y = residual)) +
    geom_point() +
    labs(x = "Beauty Score", y = "Residual") +
    geom_hline(yintercept = 0, col = "blue", size = 1)




# 10-4 シミュレーションベースの回帰分析 ----------------------------------------


# ＜ポイント＞
# - 回帰分析の結果は、信頼性指標として標準誤差を与えてくれるがシミュレーションは行わなかった（理論ベース）
# - ここではシミュレーションベースの回帰分析を行って、結果の信頼性を解釈する


# ＜概要＞
# - specify    : フォーミュラで以下の関係性を定義 score ~ bty_avg
# - generate   : 463個のサンプルからブートストラップによるリサンプリングを行う
# - calculate  : b1に対するスコアを取得
# - Visualize  : ブートストラップ分布で｢パーセンタイル法｣と｢標準誤差法｣を使用して95％信頼区間を作成


# 回帰係数のシミュレーション
# --- ブートストラップ法で1000回のリサンプリングを実施
# --- リサンプリングごとに回帰係数を計算
bootstrap_distn_slope <-
  evals_ch5 %>%
    specify(formula = score ~ bty_avg) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "slope")


# 確認
bootstrap_distn_slope %>% print()


# プロット
bootstrap_distn_slope %>% visualize()


# 信頼区間の取得
# --- パーセンタイル法
percentile_ci <-
  bootstrap_distn_slope %>%
  get_confidence_interval(type = "percentile", level = 0.95)


# 確認
percentile_ci %>% print()


# 全体の回帰係数
observed_slope <-
  evals %>%
    specify(score ~ bty_avg) %>%
    calculate(stat = "slope")


# 確認
observed_slope %>% print()
lm(score ~ bty_avg, data = evals_ch5)


# 信頼区間の取得
# --- 標準誤差法
se_ci <-
  bootstrap_distn_slope %>%
    get_ci(level = 0.95, type = "se", point_estimate = observed_slope)


# 確認
se_ci %>% print()


# プロット
# --- パーセンタイル法と標準誤差法の区間を表示
bootstrap_distn_slope %>%
  visualize() +
  shade_confidence_interval(endpoints = percentile_ci, fill = NULL,
                            linetype = "solid", color = "grey90") +
  shade_confidence_interval(endpoints = se_ci, fill = NULL,
                            linetype = "dashed", color = "grey60") +
  shade_confidence_interval(endpoints = c(0.035, 0.099), fill = NULL,
                            linetype = "dotted", color = "black")



# 帰無分布の傾き
null_distn_slope <-
  evals %>%
    specify(score ~ bty_avg) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "slope")


# p値の取得
null_distn_slope %>%
  get_p_value(obs_stat = observed_slope, direction = "both")





