# Title     : Bootstrapping and Confidence Intervals
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/15
# URL       : https://moderndive.com/8-confidence-intervals.html



# ＜概要＞
# - サンプルが1つしか確保できない場合に、サンプリング変動をどのように定量化するか
#   --- ｢ブートストラップ｣によるリサンプリングを導入する
#   --- リサンプリングによるばらつきを信頼区間として示す
# - 信頼区間について｢パーセンタイル法｣と｢標準誤差法｣を学ぶ



# ＜本章の流れ＞
# 8.1 Pennies activity
# 8.2 リンサンプリングのシミュレーション（infer::rep_sample_n関数を用いた場合）
# 8.3 信頼区間を理解する（パーセンタイル法と標準誤差法）
# 8.4 信頼区間を構築する（inferのワークフローを理解する）
# 8.5 信頼区間を解釈する
# 8.7 サンプル分布とブートストラップ分布、理論ベースの信頼区間



library(tidyverse)
library(moderndive)
library(infer)
library(gridExtra)




#%% 8.1 Pennies activity ------------------------------------------


# 8.1.1 問題提起 -------------------------


# ＜問題提起＞
# - 全米で2019年現在で使用されている全てのペニー通貨の平均的な製造年月日に興味がある
# - 全てのペニー通貨を集めることは不可能なので、特定地域の地元銀行の金庫のペニーをサンプルとする
# - サンプルデータは58枚、毎回銀行から新しい通貨を調達するのは手間がかかるので避けたい


# ＜ソリューション＞
# - 復元抽出のブートストラップリサンプリングでシミュレーションを行う
# - 非復元抽出だと、抽出した残りのサンプルが特定されてしまうため



# データ確認
# --- 担当者がシャベルでボール救いをした結果
# --- 担当者ごとにreplicateの番号が付いている
pennies_sample %>% print()
pennies_sample %>% glimpse()
pennies_sample %>% skimr::skim()


# プロット作成
# --- ヒストグラム
# --- ほとんどのペニーは1980年から2010年の間
# --- 1970年より古いペニーは数個しかないため、わずかに左に傾いた分布に注意
pennies_sample %>%
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white")


# 平均値
# --- サンプルデータの58レコード
# --- サンプル平均なのでx_bar
x_bar <- pennies_sample %>% summarize(mean_year = mean(year))
x_bar %>% round(2) %>% as.data.frame()



# 8.1.2 ペニーのリサンプリング -------------------------


# リサンプリング用データ
pennies_resample <- tibble(
  year = c(1976, 1962, 1976, 1983, 2017, 2015, 2015, 1962, 2016, 1976,
           2006, 1997, 1988, 2015, 2015, 1988, 2016, 1978, 1979, 1997,
           1974, 2013, 1978, 2015, 2008, 1982, 1986, 1979, 1981, 2004,
           2000, 1995, 1999, 2006, 1979, 2015, 1979, 1998, 1981, 2015,
           2000, 1999, 1988, 2017, 1992, 1997, 1990, 1988, 2006, 2000)
)


# 確認
pennies_resample %>% print()


# ヒストグラム比較
# --- リサンプリング用 vs オリジナル(サンプル)
# --- 似た分布ではあるものの、一致はしていない
p_resample <-
  pennies_resample %>%
    ggplot(aes(x = year)) +
    geom_histogram(binwidth = 10, color = "white") +
    labs(title = "Resample of 50 pennies")

p_sample <-
  pennies_sample %>%
    ggplot(aes(x = year)) +
    geom_histogram(binwidth = 10, color = "white") +
    labs(title = "Original sample of 50 pennies")

grid.arrange(p_resample, p_sample, nrow = 1)



# 平均値の比較
pennies_resample$year %>% mean()
pennies_sample$year %>% mean()



# 8.1.3 35回のリサンプリング -------------------------

# ＜実験＞
# - 35人の友人でリサンプリングのテストを行う


# データ確認
# --- 50回の抽出を35人が行っている
pennies_resamples %>% print()
pennies_resamples %>% group_by(replicate, name) %>% tally()


# 平均値の算出
# --- リサンプリングのシミュレーションごとの50枚の年表示の平均
resampled_means <-
  pennies_resamples %>%
    group_by(name) %>%
    summarize(mean_year = mean(year))


# 確認
resampled_means %>% print()


# ヒストグラム作成
# --- リサンプルごとの発行年の平均値
resampled_means %>%
  ggplot(aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "Sampled mean year")





#%% 8.2 リンサンプリングのシミュレーション --------------------------------------


# 8.2.1 仮想リサンプリング：1回 ------------------------------


# ＜備考＞
# - rep_sample_n()の検証で｢replace引数｣の動作について確認
# - TRUEなら復元抽出、FALSEなら非復元抽出
# - replace=FALSEの場合、size引数はtblのレコード数より小さく設定する必要がある


# データ確認
# --- リサンプリングデータ
pennies_sample %>% print()


# リサンプリング
# --- 50個のサンプルから50回抽出する
# --- シミュレーション回数は1回
# --- replace = TRUE（復元抽出）
virtual_resample <-
  pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 1)


# 確認
virtual_resample  %>% arrange(ID)


# 平均値の算出
# --- リサンプリングでシミュレーションした50枚の年表示の平均
# --- シミュレーション1回のみ
virtual_resample %>%
  summarize(resample_mean = mean(year))



# 8.2.2 仮想リサンプリング：35回 ------------------------------


# リサンプリング
# --- 35回
virtual_resamples <-
  pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 35)


# 確認
virtual_resamples %>% print()
virtual_resamples %>% group_by(replicate) %>% tally()


# 平均値の算出
# --- リサンプリングでシミュレーションした50枚の年表示の平均
virtual_resampled_means <-
  virtual_resamples %>%
    group_by(replicate) %>%
    summarize(mean_year = mean(year))


# 確認
resampled_means %>% print()


# ヒストグラム作成
# --- サンプル平均の｢ブートストラップ分布｣と呼ぶ
# --- 標本平均の標本分布の近似値
virtual_resampled_means %>%
  ggplot(aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "Resample mean year")



# 8.2.3 仮想リサンプリング：1000回 ------------------------------


# リサンプリング平均の算出
# --- パイプで処理を結合
virtual_resampled_means <-
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>%
    group_by(replicate) %>%
    summarize(mean_year = mean(year))


# プロット
# --- サンプル平均のブートストラップ分布
# --- 標本平均の標本分布の近似値
# --- シミュレーション回数を増やすことでベルの形がよりはっきりし始めている
virtual_resampled_means %>%
  ggplot(aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "sample mean")


# 平均値
# --- シミュレーションごとの平均値の平均
virtual_resampled_means %>%
  summarize(mean_of_means = mean(mean_year))





#%% 8.3 信頼区間を理解する --------------------------------------


# 8.3.1 Percentile method ---------------------------

# ＜ポイント＞
# - 信頼区間を構築する1つの方法は、ブートストラップ分布の中央値に対する95％を使用すること
#   --- これを行うには、2.5％点と97.5％点の値を計算する（パーセンタイル法）
# - ｢1.シミュレーション｣｢2.信頼区間の計算｣｢3.プロット｣の３ステップでアプローチ


# データ確認
# --- 50レコード
pennies_sample %>% print()


# ステップ1：シミュレーション
# --- yearをターゲットとすることを明示
# --- 50回の復元抽出のシミュレーションを1000セット行う（ブートストラップ法）
# --- 1000セットそれぞれの平均値を算出する
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")


# 確認
bootstrap_distribution %>% print()



# ステップ２： 信頼区間の計算
# --- 1000回
percentile_ci <-
  bootstrap_distribution %>%
    get_confidence_interval(level = 0.95, type = "percentile")


# 確認
percentile_ci %>% print()



# ステップ３：プロット
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = percentile_ci)




# 8.3.2 Standard error method ---------------------------

# ＜ポイント＞
# - データの分布から標準誤差を算出して使用する
#   --- 標準誤差法
# - ｢1.シミュレーション｣｢2.信頼区間の計算｣｢3.プロット｣の３ステップでアプローチ
#   --- 以下ではステップ１は8.3.1と共通


# ステップ２：信頼区間の算出
# --- bootstrap_distribution()は式の中が確認できる
standard_error_ci <-
  bootstrap_distribution %>%
  get_confidence_interval(type = "se", point_estimate = x_bar)


# 計算照明
# --- 標準誤差と平均値から算出
# --- 微妙に一致しないが考え方は以下の通り
multiplier <- stats::qnorm(1 - (1 - 0.95)/2)
standard_error_ci_calc <-
  virtual_resampled_means %>%
    summarize(MEAN = mean(mean_year),
              SE = sd(mean_year),
              Lower = MEAN - SE * multiplier,
              Upper = MEAN + SE * multiplier)


# 確認
standard_error_ci %>% as.data.frame()
standard_error_ci_calc %>% as.data.frame()


# ステップ３：プロット
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = standard_error_ci)




#%% 8.4 信頼区間を構築する ----------------------------------------------


# 8.4.1 Original workflow -----------------------

# ＜ポイント＞
# - infer::rep_sample_n()でリサンプリングと統計量の算出は可能
# - ｢複数変数の関係性分析｣や｢統計的推論｣までは行えない
#   --- {infer}ワークフローが必要


# リサンプリング平均の算出
# --- リサンプリング用データの提示
# --- リサンプリング・シミュレーション
# --- replicateごとに平均値を算出
pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>%
  group_by(replicate) %>%
  summarize(mean_year = mean(year))




# 8.4.2 infer package workflow -----------------------

# ＜ポイント＞
# - {infer}のワークフローで｢リサンプリング｣と｢統計的推論｣を融合する
# - infer::rep_sample_n()によるリサンプリングフレームワークと類似したプロセスを踏む
# - 統計的推論のステップと一致した関数が提供されている
# - 複数変数の関係性に注目した推論で高い有意性を持つ


# 平均値の算出（dplyrによる集計）
# --- ペニーの製造年
pennies_sample %>%
  summarize(stat = mean(year)) %>%
  as.data.frame()


# 平均値の算出（inferによる集計）
# --- ペニーの製造年
# --- 統計的推論のタグ情報を含んでいる
pennies_sample %>%
  specify(response = year) %>%
  calculate(stat = "mean")



# ステップ1. specify：統計的推論の対象を特定 --------------------

# ＜ポイント＞
# - specify()では統計的推論を行う対象を設定する（属性情報として追加されている）
# - ｢引数｣と｢フォーミュラ｣の両方で設定することができる


# 応答変数のみ指定
# --- 引数方式
pennies_sample %>%
  specify(response = year)


# 応答変数のみ指定
# --- フォーミュラ方式
pennies_sample %>%
  specify(formula = year ~ NULL)




# ステップ2. generate：リサンプリング・シミュレーション --------------------

# ＜ポイント＞
# - generate()でリサンプリングを行う
# - type引数で｢bootstrap｣｢permute｣｢simulate｣を選択することができる


# ブートストラップ・リサンプリング
sim <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap")


# 確認
sim %>% print()
sim %>% group_by(replicate) %>% tally()



# 同様の操作
# --- specify()と異なり対象を特定せず、レコード全体をリサンプリング対象としている
# --- replaceとTRUEとすることで｢bootstrap｣によるリサンプリングとなる
# --- sizeは元データの列数に併せておく
sim2 <-
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1000)


# 確認
sim %>% print()
sim %>% group_by(replicate) %>% tally()




# ステップ3. calculate：統計量の計算 --------------------

# ＜ポイント＞
# - calculate()で算出する統計量を選択する
# - 統計的検定のタイプに合わせた計算方法が準備されている


# ブートストラップ分布のデータ
# --- リサンプリングデータから統計量を計算
# --- replicateごとの統計量を元に分布を作成する
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000) %>%
    calculate(stat = "mean")


# 確認
bootstrap_distribution %>% print()


# 同様の操作
# --- specify()と異なり対象を特定せず、レコード全体をリサンプリング対象としている
# --- replaceとTRUEとすることでbootstrapリサンプリングとなる
bootstrap_distribution_2 <-
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>%
    group_by(replicate) %>%
    summarize(stat = mean(year)) %>%
    ungroup()


# 確認
bootstrap_distribution_2 %>% print()



# ステップ4. visualize：可視化 --------------------

# ＜ポイント＞
# - calculate()で算出する統計量を選択する
# - 統計的検定のタイプに合わせた計算方法が準備されている


# リサンプル分布
# --- ヒストグラム
pennies_sample %>%
  specify(response = year) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  visualize()




# 8.4.3 Percentile method with infer -----------------------


# ステップ１：シミュレーション
# --- ブートストラップ分布のデータ
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")


# ステップ２：信頼区間の算出
# --- パーセンタイル法
percentile_ci <-
  bootstrap_distribution %>%
    get_confidence_interval(level = 0.95, type = "percentile")


# 確認
percentile_ci %>% as.data.frame()


# ステップ３：プロット
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = percentile_ci)


# ステップ３：プロット
# --- shade_ci()はshade_confidence_interval()のmisc
# --- 色を変更
bootstrap_distribution %>%
  visualize() +
  shade_ci(endpoints = percentile_ci, color = "hotpink", fill = "khaki")



# 8.4.4 Standard error method with infer -----------------------

# ステップ１：シミュレーション
# --- ブートストラップ分布のデータ
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")


# ステップ２：信頼区間の算出
# --- 標準誤差法
standard_error_ci <-
  bootstrap_distribution %>%
  get_confidence_interval(type = "se", point_estimate = x_bar)


# 確認
standard_error_ci %>% as.data.frame()


# ステップ３：プロット
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = standard_error_ci)




# 8.5 信頼区間の解釈 -----------------------

# ＜ポイント＞
# - 信頼区間の有効性は、母集団パラメータの真の値が含まれているかどうかによって判断する
# - 母集団の真の値が分からない場合は推定を行う
#


# ＜はじめに＞
# - 7章で使用した玉のサンプリングを用いる


# データ確認
bowl %>% print()


# 赤玉の割合
# --- 母集団の平均値（通常は不明なことが多いが、この問題では計算可能）
bowl %>%
  summarize(p_red = mean(color == "red"))



# 8.5.1 Did the net capture the fish? ---------------

# データ確認
bowl_sample_1 %>% print()


# ステップ１：統計的推論の対象を特定
# --- 文字列を応答値に設定するとファクターとして扱われる
# --- 赤玉に興味があるのでsuccessに指定
bowl_sample_1 %>%
  specify(response = color, success = "red")


# ステップ２：リサンプリング
# --- ブートストラップ法による1000回リサンプリング
# --- 50000レコード（50個の元データを1000回リサンプリング）
bowl_sample_1 %>%
  specify(response = color, success = "red") %>%
  generate(reps = 1000, type = "bootstrap")


# ステップ３：統計量の算出
# --- 比率
sample_1_bootstrap <-
  bowl_sample_1 %>%
    specify(response = color, success = "red") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "prop")


# 確認
sample_1_bootstrap %>% print()



# ステップ４：.信頼区間の算出
# --- パーセンタイル法
percentile_ci_1 <-
  sample_1_bootstrap %>%
  get_confidence_interval(level = 0.95, type = "percentile")


# 確認
percentile_ci_1 %>% as.data.frame()




# ステップ５：.可視化

# ヒストグラム作成
# --- ブートストラップ分布
# --- 信頼区間を追加
sample_1_bootstrap %>%
  visualize(bins = 15) +
  shade_confidence_interval(endpoints = percentile_ci_1) +
  geom_vline(xintercept = 0.42, linetype = "dashed")



# 50個のボールサンプルの場合 ------------------

# リサンプリング
bowl_sample_2 <- bowl %>% rep_sample_n(size = 50)
bowl_sample_2 %>% print()


# ステップ１ー３
# --- 1: 統計的推論の対象を特定
# --- 2: リサンプリング
# --- 3: 統計量の算出
sample_2_bootstrap <-
  bowl_sample_2 %>%
    specify(response = color, success = "red") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "prop")


# ステップ４：.信頼区間の算出
# --- パーセンタイル法
percentile_ci_2 <-
  sample_2_bootstrap %>%
    get_confidence_interval(level = 0.95, type = "percentile")


# 確認
percentile_ci_2


sample_2_bootstrap %>%
  rename(p_hat = stat) %>%
  mutate(n = nrow(bowl_sample_1),
         SE = sqrt(p_hat * (1 - p_hat) / n),
         MoE = 1.96 * SE,
         lower_ci = p_hat - MoE,
         upper_ci = p_hat + MoE) %>%
  ggplot(aes(x = replicate, y = p_hat, ymin = lower_ci, ymax = upper_ci))+
    geom_pointrange()+
    geom_hline(yintercept = real_red_prop$prop, linetype=2)+
    coord_flip()+
    xlab('Variable')


# 8.6 結論 ----------------------------

mythbusters_yawn


mythbusters_yawn %>%
  group_by(group, yawn) %>%
  summarize(count = n())

mythbusters_yawn %>%
  specify(formula = yawn ~ group)

mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes")


first_six_rows <- head(mythbusters_yawn)
first_six_rows

first_six_rows %>%
  sample_n(size = 6, replace = TRUE)



mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap")



mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props")


bootstrap_distribution_yawning <- mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("seed", "control"))
bootstrap_distribution_yawning




visualize(bootstrap_distribution_yawning) +
  geom_vline(xintercept = 0)




bootstrap_distribution_yawning %>%
  get_confidence_interval(type = "percentile", level = 0.95)


obs_diff_in_props <- mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  # generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("seed", "control"))
obs_diff_in_props


myth_ci_se <- bootstrap_distribution_yawning %>%
  get_confidence_interval(type = "se", point_estimate = obs_diff_in_props)
myth_ci_se






# 8.7 結論 ----------------------------

# 8.7.1 ブートストラップ分布とサンプル分布の違い -----------------------------------------

# ＜サンプル分布＞
# - サンプル分布は真の分布(bowl)をリサンプリングの起点にしている
# - サンプル分布はリサンプリングを非復元抽出で行う
# - 平均値も標準誤差も正しく推定できる


# ＜ブートストラップ分布＞
# - サンプル分布は真の分布(bowl)をリサンプリングの起点にしている
# - サンプル分布はリサンプリングを非復元抽出で行う一方、ブートストラップ分布は復元抽出で行う
# - 平均値はずれる可能性が高いが、標準誤差は正しく推定できる



# サンプル分布の作成 ----------------------------------

# 元データの確認
# --- 母集団である2400個
bowl %>% print()


# ステップ1： リサンプルデータの作成
# --- replace引数がFALSEなので非復元抽出
# --- 非復元抽出はシャベル1回で1シミュレーションで取得することを意味する
# --- ブートストラップリサンプリングの場合は復元抽出なので対照的
virtual_samples <-
  bowl %>%
    rep_sample_n(size = 50, replace = FALSE, reps = 1000)


# ステップ2： 統計値の計算
# --- 赤玉の比率を計算
# --- リサンプリング・シミュレーション
sampling_distribution <-
  virtual_samples %>%
    group_by(replicate) %>%
    summarize(red = sum(color == "red")) %>%
    mutate(prop_red = red / 50)


# 真の赤玉の比率
# --- 真の値
real_red <-
  bowl %>%
    summarize(p_red = mean(color == "red"))


# ステップ3： サンプル分布の作成
# --- 赤玉の比率をヒストグラム化
p1 <-
  sampling_distribution %>%
    ggplot(aes(x = prop_red)) +
      geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
      xlim(0, 0.8) +
      ylim(0, 350) +
      geom_vline(xintercept = real_red$p_red, color = "red", size = 1.5) +
      labs(x = "Proportion of 50 balls that were red",
           title = "Sampling distribution")


# 確認
print(p1)



## ブートストラップ分布 ------------------------------------------------


# 元データの確認
# --- IlyasとYohanの50個のサンプリング
bowl_sample_1 %>% print()


# ステップ１ー３
# --- 1: 統計的推論の対象を特定
# --- 2: リサンプリング（復元抽出のブートストラップ法 ※サンプル分布と異なる点）
# --- 3: 統計量の算出
bootstrap_distribution <-
  bowl_sample_1 %>%
    specify(response = color, success = "red") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "prop")


# 真の赤玉の比率
# --- 真の値
Ilyas_Yohan_red <-
  bowl_sample_1 %>%
    summarize(p_red = mean(color == "red"))


# ステップ4： ブートストラップ分布の作成
# --- 赤玉の比率をヒストグラム化
p2 <-
  bootstrap_distribution %>%
    ggplot(aes(x = stat)) +
      geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
      xlim(0, 0.8) +
      ylim(0, 350) +
      geom_vline(xintercept = Ilyas_Yohan_red$p_red, linetype = "dotted", color = "red", size=2) +
      labs(x = "Proportion of 50 balls that were red",
           title = "Sampling distribution")


# 確認
print(p2)



## 比較 ------------------------------------------------

# ＜ポイント＞
# - サンプル分布は母集団平均のP=0.375を中心に分布する（サンプリングがランダムに行われているため）
# - ブートストラップ分布はIlyasとYohanの50個のサンプリング平均のP=0.42を中心に分布する
#   --- ブートストラップは少数サンプルしか得られないことが前提なので、平均値はずれる可能性が高い


# プロット比較
grid.arrange(p1, p2, nrow = 1)


# ＜ポイント＞
# - 標準誤差は両社でかなり近接している
#   --- ブートストラップは標準誤差に対して適切な推定を提供する。

# 標準誤差の比較
sampling_distribution %>% summarize(se = sd(prop_red))
bootstrap_distribution %>% summarize(se = sd(stat))




# 8.7.2 理論ベースの信頼区間 -----------------------------------------

# ＜ポイント＞
# - シミュレーションベースの方法として｢パーセンタイル法｣や｢標準誤差法｣を用いて信頼区間を計算した
# - 理論ベースの方法では、正規分布を前提として信頼区間を計算する
# - 信頼区間95％として2σ=1.96を利用する


# データ確認
# --- 生徒33人が行ったシミュレーション結果
# --- すでに50回のリサンプリングの平均値を格納している
tactile_prop_red %>% print()


# 信頼区間の計算
# --- n=50はシミュレーション回数に対応
conf_ints <-
  tactile_prop_red %>%
    rename(p_hat = prop_red) %>%
    mutate(n = 50,
           SE = sqrt(p_hat * (1 - p_hat) / n),
           MoE = 1.96 * SE,
           lower_ci = p_hat - MoE,
           upper_ci = p_hat + MoE)


# 確認
conf_ints %>% print()


# 真の赤玉の比率
real_red_prop <-
  bowl %>%
    summarize(p_red = mean(color == "red"))


# プロット
# --- サンプルごとの信頼区間プロット
conf_ints %>%
  ggplot(aes(x = group, y = p_hat, ymin = lower_ci, ymax = upper_ci))+
   geom_pointrange()+
   geom_hline(yintercept = real_red_prop$p_red, linetype=2)+
   coord_flip()+
   xlab('Variable')





