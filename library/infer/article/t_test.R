# Title     : t-test
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/articles/t_test.html


# ＜概要＞
# - {infer}はtidyverseのフレームワークで柔軟な統計的推論を行うことを目指す
# - 以下の4つの関数で一連の統計的検定を行う
#   --- specify()     : 関心のある変数または変数間の関係を指定。
#   --- hypothesize() : 帰無仮説を宣言。
#   --- generate()    : 帰無仮説を反映するデータを生成。
#   --- calculate()   : 生成されたデータから統計の分布を計算してNull分布を形成




library(tidyverse)
library(infer)


# データロード
data(gss)



# データ確認
gss %>% print()
gss %>% glimpse()
gss %>% skimr::skim()







#%% 1つのt検定 ------------------------------------------------

# プロット作成
# --- 回答者の多くが40時間と答えている
# --- 実施にはその他の値も多く含まれている
# --- 1週間に勤務する真の平均時間数が40であるという証拠があるかどうかをテストしてみましょう。
gss %>%
  ggplot(aes(x = hours)) +
  geom_histogram()


# 検定統計量の算出
# --- 帰無仮説(mu=40)に対するt検定統計量の算出
# --- 帰無仮説の元で作成した帰無分布と比較して、真の値がどれくらいの範囲にあるかを示す
observed_statistic <-
  gss %>%
    specify(response = hours) %>%
    hypothesize(null = "point", mu = 40) %>%
    calculate(stat = "t")


# 確認
observed_statistic %>% print()


# 帰無分布から統計量を推定
null_distribution_1_sample <-
  gss %>%
    specify(response = hours) %>%
    hypothesize(null = "point", mu = 40) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "t")


# 確認
null_distribution_1_sample %>% print()


# プロット
# --- 帰無分布とt統計量を表示
# --- mu=40という帰無仮説は｢ありそうにない｣数値になっている
# --- 帰無仮説は棄却される（muは40ではない）
null_distribution_1_sample %>%
  visualize() +
  shade_p_value(observed_statistic,
                direction = "two-sided")


# p値の算出
# --- 帰無分布とt値から算出
# --- hoursの平均値が40である場合、検定統計量が2.0852以上である確率を示す。
p_value_1_sample <-
  null_distribution_1_sample %>%
    get_p_value(obs_stat = observed_statistic,
                direction = "two-sided")


# 確認
p_value_1_sample %>% print()



# ラッパー関数による結果
gss %>% t_test(response = hours, mu = 40)
gss %>% t_stat(response = hours, mu = 40)





#%% 対応のあるt検定 ------------------------------------------------

# プロット作成
# --- collegeのカテゴリごとにhoursの分布を表示
gss %>%
  ggplot(aes(x = college, y = hours)) +
  geom_boxplot()



# 検定統計量の算出
# --- t値
# --- 帰無仮説；degree/no degree でhoursの平均値は等しい
observed_statistic <-
  gss %>%
    specify(hours ~ college) %>%
    calculate(stat = "t", order = c("degree", "no degree"))


# 確認
observed_statistic %>% print()



# 帰無分布からt値を計算
# --- 説明変数(college)をdegree/no degree に別ける
null_distribution_2_sample_permute <-
  gss %>%
    specify(hours ~ college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "t", order = c("degree", "no degree"))


# 理論分布からt値を計算
null_distribution_2_sample_theoretical <-
  gss %>%
    specify(hours ~ college) %>%
    hypothesize(null = "independence") %>%
    calculate(stat = "t", order = c("degree", "no degree"))



# プロット
# --- シミュレーションによる帰無分布とt統計量
null_distribution_2_sample_permute %>%
  visualize() +
  shade_p_value(observed_statistic,
                direction = "two-sided")


# プロット
# --- 理論分布
null_distribution_2_sample_theoretical %>%
  visualize(method = "theoretical") +
  shade_p_value(observed_statistic,
                direction = "two-sided")


# プロット
# --- 帰無分布と理論分布を併せて表示
# --- t統計量も表示
null_distribution_2_sample_permute %>%
  visualize(method = "both") +
  shade_p_value(observed_statistic,
                direction = "two-sided")


# p値の計算
p_value_2_sample <-
  null_distribution_2_sample_permute %>%
    get_p_value(obs_stat = observed_statistic,
                direction = "two-sided")

# 確認
p_value_2_sample %>% print()


# ラッパー関数による結果
gss %>%
  t_test(formula = hours ~ college,
         order = c("degree", "no degree"),
         alternative = "two-sided")
