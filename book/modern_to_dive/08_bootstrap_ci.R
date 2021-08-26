# **************************************************************************************
# Book      : Modern Dive into R
# Theme     : Ⅲ Statistical Interface with infer
# Title     : 8 Bootstrapping and Confidence Intervals
# Created on: 2021/08/26
# URL       : https://moderndive.com/8-confidence-intervals.html
# **************************************************************************************


# ＜概要＞
# - サンプルが1つしか確保できない場合に、サンプリング変動をどのように定量化するか
#   --- ｢ブートストラップ｣によるリサンプリングを導入する
#   --- リサンプリングによるばらつきを信頼区間として示す
# - 信頼区間について｢パーセンタイル法｣と｢標準誤差法｣を学ぶ


# ＜目次＞
# 8-0 準備
# 8-1 Pennies activity
# 8-2 リンサンプリングのシミュレーション
# 8-3 信頼区間を理解する
# 8-4 信頼区間の算出
# 8-5 信頼区間の解釈
# 8-6 ケーススタディ
# 8-7 結論


# 8-0 準備 -------------------------------------------------------

# ライブラリ
library(tidyverse)
library(moderndive)
library(infer)
library(gridExtra)



# 8-1 Pennies activity ------------------------------------------

# ＜問題提起＞
# - 全米で2019年現在で使用されている全てのペニー通貨の平均的な製造年に興味がある
# - 全てのペニー通貨を集めることは不可能なので、特定地域の地元銀行の金庫のペニーをサンプルとする
# - サンプルデータは50枚、毎回銀行から新しい通貨を調達するのは手間がかかるので避けたい


# ＜ソリューション＞
# - ブートストラップ・リサンプリング(復元抽出)でシミュレーションを行う
#   --- 非復元抽出だと、抽出した残りのサンプルが特定されてしまうため


# ＜目次＞
# 8-1-1 リサンプリングの元データ
# 8-1-2 ペニーのリサンプリング
# 8-1-3 35回のリサンプリング（ブートストラップ）


# 8-1-1 リサンプリングの元データ ----------------------

# ＜概要＞
# - サンプルデータ(50枚)の分布と平均値を確認しておく


# データ確認
# --- 担当者がシャベルでボール救いをした結果
# --- 担当者ごとにreplicateの番号が付いている
pennies_sample %>% print()
pennies_sample$year %>% table()


# プロット作成
# --- ヒストグラム
# --- ほとんどのペニーは1980年から2010年の間
# --- 1970年より古いペニーは数個しかないため、わずかに左に傾いた分布に注意
pennies_sample %>%
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white")


# 平均値
# --- 50枚のペニー通貨の製造年の平均値
# --- サンプル平均（点推定値）
x_bar <- pennies_sample %>% summarize(mean_year = mean(year))
x_bar %>% round(2) %>% as.data.frame()



# 8-1-2 ペニーのリサンプリング -------------------------

# ＜概要＞
# - ブートストラップ(復元抽出)によるリサンプリングを行う
#   --- 非復元抽出にすると元の50枚のペニーと同じ結果になってしまう


# リサンプリング用データ
# --- 8-1-1と完全一致はしていない
pennies_resample <- tibble(
  year = c(1976, 1962, 1976, 1983, 2017, 2015, 2015, 1962, 2016, 1976,
           2006, 1997, 1988, 2015, 2015, 1988, 2016, 1978, 1979, 1997,
           1974, 2013, 1978, 2015, 2008, 1982, 1986, 1979, 1981, 2004,
           2000, 1995, 1999, 2006, 1979, 2015, 1979, 1998, 1981, 2015,
           2000, 1999, 1988, 2017, 1992, 1997, 1990, 1988, 2006, 2000)
)


# 確認
pennies_resample %>% print()
pennies_resample %>% table()


# ヒストグラム作成
# --- リサンプリング用 
# --- オリジナル(サンプル)
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

# プロット比較
# --- 似た分布ではあるものの一致はしていない
grid.arrange(p_resample, p_sample, nrow = 1)



# 平均値の比較
# --- リサンプリング用 
# --- オリジナル(サンプル)
pennies_resample$year %>% mean()
pennies_sample$year %>% mean()



# 8-1-3 35回のリサンプリング -------------------------

# ＜概要＞
# - 35人の友人で50回のリサンプリングを行うテストを実施する


# データ確認
# --- 50回の抽出を35人が行っている
pennies_resamples %>% print()
pennies_resamples %>% group_by(replicate, name) %>% tally()


# 平均値の算出
# --- リサンプリングのシミュレーションごとの平均値（50枚の製造年の平均）
resampled_means <-
  pennies_resamples %>%
    group_by(name) %>%
    summarize(mean_year = mean(year))


# 確認
resampled_means %>% print()


# ヒストグラム作成
# --- リサンプルごとの発行年の平均値
# --- ブートストラップ分布（標本平均の標本分布の近似値）
resampled_means %>%
  ggplot(aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "Sampled mean year")



# 8-2 リンサンプリングのシミュレーション --------------------------------------

# ＜概要＞
# - {infer}のrep_sample_n()を用いてリサンプリングをパイプラインで処理する


# ＜目次＞
# 8-2-1 仮想リサンプリング：1回
# 8-2-2 仮想リサンプリング：35回
# 8-2-3 仮想リサンプリング：1000回 


# 8-2-1 仮想リサンプリング：1回 ------------------------------

# ＜概要＞
# - infer::rep_sample_n()はデータフレームのレコードをリサンプリングする
#   --- dplyr::sample_n()は非復元抽出のみ対応


# データ確認
# --- リサンプリングデータ
pennies_sample %>% print()
pennies_sample$year %>% table()


# リサンプリング
# --- 50個のサンプルから50回抽出する（シミュレーションは1回のみ）
virtual_resample <-
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1)


# 確認
virtual_resample %>% arrange(ID)


# 平均値の算出
# --- リサンプリングでシミュレーションした50枚の年表示の平均
# --- シミュレーション1回のみ
virtual_resample %>%
  summarize(resample_mean = mean(year))



# 8-2-2 仮想リサンプリング：35回 ------------------------------

# ＜概要＞
# - infer::rep_sample_n()はデータフレームのレコードをリサンプリングする
#   --- dplyr::sample_n()は非復元抽出のみ対応


# リサンプリング
# --- 1750レコード（50回抽出 * 35シミュレーション）
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


# 8-2-3 仮想リサンプリング：1000回 ------------------------------

# リサンプリング平均
# --- シミュレーションを実施（50000レコード）
# --- シミュレーションごとの平均値を算出(1000レコード)
virtual_resampled_means <-
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>%
    group_by(replicate) %>%
    summarize(mean_year = mean(year))


# ヒストグラム作成
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


# 8-3 信頼区間を理解する --------------------------------------

# ＜概要＞
# - ブートストラップに信頼区間を導入して点推定から区間推定に拡張する
# - {infer}のspecify()とgenerate()を用いてパイプラインを構築
#   --- visualize()でggplot2ベースのプロット作成もサポート


# ＜目次＞
# 8-3-1 Percentile method
# 8-3-2 Standard error method


# 8-3-1 Percentile method ---------------------------

# ＜ポイント＞
# - ブートストラップ分布の中央値に対する95％の信頼区間を設定する
#   --- 2.5％点と97.5％点の値を計算する（パーセンタイル法）


# ＜ステップ＞
# ステップ１：シミュレーション
# ステップ２：信頼区間の計算
# ステップ３：プロット

# データ確認
# --- 50レコード
pennies_sample %>% print()
pennies_sample$year %>% table()


# ステップ１：シミュレーション
# --- yearをターゲットとすることを明示
# --- 50回の復元抽出のシミュレーションを1000セット行う（ブートストラップ法）
# --- 1000セットそれぞれの平均値を算出する
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")


# 確認
# --- シミュレーション結果
bootstrap_distribution %>% print()


# 参考：specify()
# --- inferクラスが生成される（ワークフローに必要な情報が追加される）
pennies_sample %>%specify(response = year) %>% class()
pennies_sample %>%specify(response = year) %>% str()



# ステップ２：信頼区間の計算
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


# 8-3-2 Standard error method ---------------------------

# ＜ポイント＞
# - データの分布から標準誤差を算出して使用する
#   --- 標準誤差法


# ステップ１：シミュレーション
# --- 8-3-1と同様
bootstrap_distribution <-
  pennies_sample %>%
  specify(response = year) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

# 確認
# --- シミュレーション結果
bootstrap_distribution %>% print()


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


# 8-4 信頼区間の算出 ----------------------------------------------


# ＜目次＞
# 8-4-1 rep_sample_n()によるブートストラップ
# 8-4-2 inferのワークフロー
# 8-4-3 パーセンタイル法の信頼区間算出（infer）
# 8-4-4 標準誤差法の信頼区間算出（infer）


# 8-4-1 rep_sample_n()によるブートストラップ -------------------

# ＜ポイント＞
# - infer::rep_sample_n()でリサンプリングと統計量の算出は可能
#   --- ブートストラップのパイプラインとしては悪くない
#   --- 統計的推論へのワークフローは提供されていない


# データ確認
pennies_sample %>% print()
pennies_sample$year %>% table()


# リサンプリング平均の算出
# --- ブートストラップリサンプリングの生成
# --- 50000レコード = 50回抽出 * 1000シミュレーション
# --- replicateごとに平均値を算出
mean_original <- 
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>%
    group_by(replicate) %>%
    summarize(mean_year = mean(year))

# 確認
mean_original %>% print()



# 8-4-2 inferのワークフロー --------------

# ＜ポイント＞
# - {infer}のワークフローで｢リサンプリング｣と｢統計的推論｣を融合する


# ＜目次＞
# ステップ１. specify：統計的推論の対象を特定
# ステップ２. generate：リサンプリング・シミュレーション
# ステップ３. calculate：統計量の計算
# ステップ４. visualize：可視化


# ステップ１. specify：統計的推論の対象を特定 --------------------

# ＜ポイント＞
# - specify()では統計的推論を行う対象を設定する（属性情報として追加されている）
# - ｢引数｣と｢フォーミュラ｣の両方で設定することができる


# 検査対象の指定
# --- 引数方式（応答変数のみ指定）
sepc_1 <-
  pennies_sample %>%
    specify(response = year)

# 検査対象の指定
# --- 引数方式（フォーミュラ方式）
sepc_2 <-
  pennies_sample %>%
    specify(formula = year ~ NULL)

# 確認
# --- 出力は通常のデータフレーム
# --- inferクラスとして統計的推論に必要な情報が格納されている
sepc_1 %>% print()
sepc_1 %>% class()
sepc_1 %>% str()


# ステップ２. generate：リサンプリング・シミュレーション --------------------

# ＜ポイント＞
# - generate()でリサンプリングを行う
# - type引数で｢bootstrap｣｢permute｣｢simulate｣を選択することができる


# リサンプリング・シミュレーションの実行
sim <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap")

# 確認
sim %>% print()
sim %>% group_by(replicate) %>% tally()


# 参考：rep_sample_n()による操作
# --- 検査対象を特定しないでレコード全体をリサンプリング対象としている
# --- replaceとTRUEとすることで｢bootstrap｣によるリサンプリングとなる
# --- sizeは元データの列数に併せておく
sim2 <-
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1000)

# 確認
sim2 %>% print()
sim2 %>% group_by(replicate) %>% tally()


# ステップ３. calculate：統計量の計算 --------------------

# ＜ポイント＞
# - calculate()で算出する統計量を選択する
# - 統計的検定のタイプに合わせた計算方法が準備されている


# シミュレーションの統計量算出
# --- リサンプリングデータから統計量を計算
# --- replicateごとの統計量を元に分布を作成する
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000) %>%
    calculate(stat = "mean")

# 確認
bootstrap_distribution %>% print()


# 参考：rep_sample_n()による操作
# --- specify()によって生成されたデータをdplyrで集計する
bootstrap_distribution_2 <-
  pennies_sample %>%
    rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>%
    group_by(replicate) %>%
    summarize(stat = mean(year)) %>%
    ungroup()

# 確認
bootstrap_distribution_2 %>% print()


# ステップ４. visualize：可視化 --------------------

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



# 8-4-3 パーセンタイル法の信頼区間算出（infer） -------------------------

# ステップ１：リサンプリング平均の算出
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")

# ステップ２：信頼区間の算出（パーセンタイル法）
percentile_ci <-
  bootstrap_distribution %>%
    get_confidence_interval(level = 0.95, type = "percentile")

# ステップ３：プロット（デフォルト）
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = percentile_ci)

# ステップ３：プロット（設定変更）
# --- shade_ci()はshade_confidence_interval()のmisc
# --- 色を変更
bootstrap_distribution %>%
  visualize() +
  shade_ci(endpoints = percentile_ci, color = "hotpink", fill = "khaki")


# 8-4-4 標準誤差法の信頼区間算出（infer） -------------------------------

# ステップ１：リサンプリング平均の算出
bootstrap_distribution <-
  pennies_sample %>%
    specify(response = year) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")

# ステップ２：信頼区間の算出（標準誤差法）
standard_error_ci <-
  bootstrap_distribution %>%
  get_confidence_interval(type = "se", point_estimate = x_bar)

# ステップ３：プロット
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = standard_error_ci)


# 8-5 信頼区間の解釈 -----------------------------------------------------------------

# ＜ポイント＞
# - 信頼区間の有効性は、母集団パラメータの真の値が含まれているかどうかによって判断する
# - 母集団の真の値が分からない場合は推定を行う


# データ確認
# --- ボール全体が母集団
bowl %>% print()

# 赤玉の割合
# --- 母集団の平均値（通常は不明なことが多いが、この問題では計算可能）
# --- 信頼区間の議論の余地はない
bowl %>%
  summarize(p_red = mean(color == "red"))



# 8-5-1 Did the net capture the fish? ---------------

# データ確認
# --- 50レコード
bowl_sample_1 %>% print()
bowl_sample_1 %>% table()


# ステップ１：統計的推論の対象を特定
# --- 文字列を応答値に設定するとファクターとして扱われる
# --- 赤玉に興味があるのでsuccessに指定
bowl_sample_1 %>%
  specify(response = color, success = "red")

# ステップ２：リサンプリング
# --- ブートストラップ法によるリサンプリング
# --- 50000レコード（50個の元データを1000回リサンプリング）
bowl_sample_1 %>%
  specify(response = color, success = "red") %>%
  generate(reps = 1000, type = "bootstrap")

# ステップ３：統計量の算出（比率）
# --- シミュレーションごとの赤玉の比率
sample_1_bootstrap <-
  bowl_sample_1 %>%
    specify(response = color, success = "red") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "prop")

# ステップ４：信頼区間の算出
# --- パーセンタイル法
percentile_ci_1 <-
  sample_1_bootstrap %>%
  get_confidence_interval(level = 0.95, type = "percentile")

# ステップ５：可視化
# --- ブートストラップ分布（ヒストグラム）
# --- 信頼区間を追加
sample_1_bootstrap %>%
  visualize(bins = 15) +
  shade_confidence_interval(endpoints = percentile_ci_1) +
  geom_vline(xintercept = 0.42, linetype = "dashed")


# 50個のボールサンプルの場合 ------------------

# リサンプリング
bowl_sample_2 <- bowl %>% rep_sample_n(size = 50)
bowl_sample_2 %>% print()


# ステップ１to ３
# --- 1: 統計的推論の対象を特定
# --- 2: リサンプリング
# --- 3: 統計量の算出
sample_2_bootstrap <-
  bowl_sample_2 %>%
    specify(response = color, success = "red") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "prop")

# ステップ４：信頼区間の算出
# --- パーセンタイル法
percentile_ci_2 <-
  sample_2_bootstrap %>%
    get_confidence_interval(level = 0.95, type = "percentile")

# ステップ５：シミュレーションごとの信頼区間
bootstrap_ci <- 
  sample_2_bootstrap %>%
    rename(p_hat = stat) %>%
    mutate(n = nrow(bowl_sample_1),
           SE = sqrt(p_hat * (1 - p_hat) / n),
           MoE = 1.96 * SE,
           lower_ci = p_hat - MoE,
           upper_ci = p_hat + MoE)

# ステップ６：プロット作成
# --- 真の赤玉比率は全シミュレーションのXパーセントだけ信頼区間の内側にある
bootstrap_ci %>%
  ggplot(aes(x = replicate, y = p_hat, ymin = lower_ci, ymax = upper_ci))+
    geom_pointrange(alpha =0.2)+
    geom_hline(yintercept = real_red_prop$prop, linetype=2)+
    coord_flip()+
    xlab('Variable')



# 8-6 ケーススタディ ----------------------------------------------------------

# ＜概要＞
# 「あくびは伝染性ですか？」という質問に答えるために、信頼区間の知識を適用


# データ確認
mythbusters_yawn %>% print()
mythbusters_yawn %>% select(group, yawn) %>% table()

# カテゴリ集計
mythbusters_yawn %>%
  group_by(group, yawn) %>%
  summarize(count = n())

# ステップ１：検査対象の特定
# --- 仮説：yesが正と仮定する
mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes")

# ステップ２：ブートストラップシミュレーション
mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap")

# ステップ３：統計量の計算（比率の差）
bootstrap_distribution_yawning <- 
  mythbusters_yawn %>%
    specify(formula = yawn ~ group, success = "yes") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "diff in props", order = c("seed", "control"))

# ステップ４：可視化
bootstrap_distribution_yawning %>% 
  visualize() +
    geom_vline(xintercept = 0)


# 信頼区間の算出
# --- パーセンタイル法
bootstrap_distribution_yawning %>%
  get_confidence_interval(type = "percentile", level = 0.95)

# 元データのみで統計量を算出
obs_diff_in_props <- 
  mythbusters_yawn %>%
    specify(formula = yawn ~ group, success = "yes") %>%
    # generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "diff in props", order = c("seed", "control"))

# 信頼区間の算出
# --- 標準誤差法
bootstrap_distribution_yawning %>%
  get_confidence_interval(type = "se", point_estimate = obs_diff_in_props)

# プロットに信頼区間を追加
boot_ci <- 
  bootstrap_distribution_yawning %>%
    get_confidence_interval(type = "se", point_estimate = obs_diff_in_props)

bootstrap_distribution_yawning %>% 
  visualize() +
  shade_confidence_interval(endpoints = boot_ci) + 
  geom_vline(xintercept = 0)




# 8-7 結論 ------------------------------------------------------------------------------------


# ＜目次＞
# 8-7-1 ブートストラップ分布とサンプル分布の違い
# 8-7-2 理論ベースの信頼区間


# 8-7-1 ブートストラップ分布とサンプル分布の違い ----------------

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
bowl$color %>% table()

# 真の赤玉の比率
real_red <-bowl %>% summarize(p_red = mean(color == "red"))
real_red %>% print()

# ステップ1：リサンプルデータの作成
# --- 非復元抽出はシャベル1回で1シミュレーションで取得することを意味する
# --- ブートストラップリサンプリングの場合は復元抽出なので対照的
virtual_samples <-
  bowl %>%
    rep_sample_n(size = 50, replace = FALSE, reps = 1000)

# ステップ2：統計値の計算
# --- 赤玉の比率を計算
# --- リサンプリング・シミュレーション
sampling_distribution <-
  virtual_samples %>%
    group_by(replicate) %>%
    summarize(red = sum(color == "red")) %>%
    mutate(prop_red = red / 50)

# ステップ3： サンプル分布の作成
# --- 赤玉の比率をヒストグラム化
p1 <-
  sampling_distribution %>%
    ggplot(aes(x = prop_red)) +
      geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
      geom_vline(xintercept = real_red$p_red, color = "red", size = 1.5) +
      labs(x = "Proportion of 50 balls that were red",
           title = "Sampling distribution")


# 確認
print(p1)


# ブートストラップ分布 ------------------------------------

# 元データの確認
# --- 50個のサンプリングのみ（母集団は不明）
bowl_sample_1 %>% print()

# 真の赤玉の比率
# --- サンプル平均
Ilyas_Yohan_red <-
  bowl_sample_1 %>%
  summarize(p_red = mean(color == "red"))

Ilyas_Yohan_red %>% print()


# ステップ１to３
# --- 1: 統計的推論の対象を特定
# --- 2: リサンプリング（復元抽出のブートストラップ法 ※サンプル分布と異なる点）
# --- 3: 統計量の算出
bootstrap_distribution <-
  bowl_sample_1 %>%
    specify(response = color, success = "red") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "prop")

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


# 比較 ------------------------------------------------

# ＜平均値＞
# - サンプル分布は母集団平均のP=0.375を中心に分布する
# - ブートストラップ分布はサンプリング平均のP=0.42を中心に分布する
#   --- ブートストラップは少数サンプルしか得られないことが前提なので、平均値はずれる可能性が高い

# プロット比較
grid.arrange(p1, p2, nrow = 1)


# ＜標準誤差＞
# - 両社はかなり近接している
#   --- ブートストラップは標準誤差に対して適切な推定を提供することを示す

# 標準誤差の比較
sampling_distribution %>% summarize(se = sd(prop_red))
bootstrap_distribution %>% summarize(se = sd(stat))


# 8-7-2 理論ベースの信頼区間 -----------------------------------------

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
