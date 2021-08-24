# **************************************************************************************
# Book        : Modern Dive into R
# Theme       : Ⅲ Statistical Interface with infer
# Title       : 7 Sampling
# Update Date : 2021/8/23
# URL         : https://moderndive.com/7-sampling.html
# **************************************************************************************


# ＜概要＞
# - サンプリングを学習することにより、統計的推論に必要な｢信頼区間｣と｢仮説検定｣の基礎を形成する
# - サンプリングから｢正確性｣と｢精度｣の概念を理解する
# - データのストーリーを伝えるための可視化を行う
# - 7章では｢比率｣に着目しているが、｢平均｣｢差｣｢回帰｣など様々な統計量に対してシミュレーションが可能


# ＜正確性：Accuracy＞
# - 見積もりが真の値にどれくらい近いかを示す
# - シミュレーション回数を増やすことで正確性が高まってくる（真の分布に近づいていく）
# - 中心極限定理により、サンプリング分布は真の値を中心とした正規分布にしたがう
# - 射的で的の真ん中に近づく（FIGURE 7.16）


# ＜精度：Precision＞
# - 推定値の一貫性を示す
# - 抽出数を増やすことで精度が高まってくる（抽出数を増やすほど母集団に近づくため）
# - 標準誤差として一般化することができる
# - FIGURE 7.16


# ＜目次＞
# 7-0 準備
# 7-1 ボウルからのサンプリング活動
# 7-2 抽出回数を変更したシミュレーション（サンプリング回数の影響）
# 7-3 抽出数を変更したシミュレーション（サンプルサイズの影響）
# 7-4 サンプリング・フレームワーク



# 7-0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(infer)
library(moderndive)
library(gridExtra)


# データロード
tactile_prop_red %>% print()


# 7-1 ボウルからのサンプリング活動 ---------------------------------------------

# ＜概要＞
# - ボールに赤白の玉が入っているが、赤の割合はどれくらいだろうか？


# ＜サンプリング活動の目的＞
# 1. サンプリング変動の影響を理解する（赤玉率の変動性）
# 2. サンプリング変動に対するサンプルサイズの影響を理解する（抽出数(50個)の影響）


# ＜サンプリング方法＞
# - シャベルは1回あたり50個の抽出
# - クラスメイト33人が1回ずつの実験を行っている
# - 抽出したボールは元に戻している（復元抽出）


# データ確認
# --- 担当者がシャベルで50個のボールを抽出して赤玉の数を集計（33人が実行しているので33レコード）
# --- 担当者ごとにreplicateの番号が付いている
tactile_prop_red %>% print(n = nrow(.))


# プロット作成
# --- 赤が出た割合をヒストグラム化
# --- 赤の割合がなんとなく見えてくる（サンプル数が少なくてヒストグラムが荒く確信が持てない）
p1 <-
  tactile_prop_red %>%
    ggplot(aes(x = prop_red)) +
      geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
      labs(x = "Proportion of 50 balls that were red",
           title = "Distribution of 33 proportions red")

# プロット出力
print(p1)


# 7-2 抽出回数を変更したシミュレーション（サンプリング回数の影響） --------------------

# ＜概要＞
# - リサンプリング回数を変化させた場合の影響を確認する（抽出数は固定）
#   --- サンプリング回数を増やすほどが理論値に近づく（Accuracyが高まる）
#   --- 理論値は37.5%


# ＜目次＞
# 準備
# サンプリング: 1回
# サンプリング: 33回
# サンプリング: 1000回


# 準備 ------------------------------------------------

# ＜ポイント＞
# - コンピュータでサンプリングでシャベルサンプリングを模倣する
# - 実際のボールに入っている玉に全て仮想IDを付ける（母集団を認識可能にする）


# データ確認
# --- 2400行（ボールに入っている全ての玉の数）
# --- 現実には玉に番号は付いていないが、シミュレーションでは便宜上番号をつけておく
bowl %>% print()

# 玉数の確認
# --- 赤玉: 900 （37.5%）
# --- 白玉: 1500（62.5%）
bowl %>% group_by(color) %>% tally()


# サンプリング: 1回 -----------------------------------

# ＜ポイント＞
# - 仮想サンプリングのイメージを確認


# サンプリング（1回）
# --- サイズ50のサンプリングを1回行う
virtual_shovel <- bowl %>% rep_sample_n(size = 50, reps = 1)

# 確認
virtual_shovel %>% print()
virtual_shovel$replicate %>% table()

# 赤の玉の数を集計
# --- prop_redは母比率pの点推定でありサンプル比率と呼ばれる
virtual_shovel %>%
  summarize(num_red = sum(color == "red")) %>%
  mutate(prop_red = num_red / 50)



# サンプリング: 33回 ---------------------------------

# ＜ポイント＞
# - 7-1で行った生徒のショベル活動を仮想サンプリングで再現する


# サンプリング回数の変更
# --- 50個の抽出を33回行う（7-1の実験と同様）
virtual_samples <- bowl %>% rep_sample_n(size = 50, reps = 33)

# 確認
virtual_samples %>% print()
virtual_samples$replicate %>% table()


# リサンプリング結果の集計
# --- 赤玉の比率を集計
virtual_prop_red <-
  virtual_samples %>%
    group_by(replicate) %>%
    summarize(red = sum(color == "red")) %>%
    mutate(prop_red = red / 50)

# 確認
virtual_prop_red %>% print()


# プロット作成
# --- 赤が出た割合をヒストグラム化
p2 <-
  virtual_prop_red %>%
    ggplot(aes(x = prop_red)) +
      geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
      labs(x = "Proportion of 50 balls that were red",
           title = "Distribution of 33 proportions red")

# プロット比較
# --- p1(左)：手動で実験
# --- p2(右)：シミュレーションで実験
# --- シミュレーションより生徒の実行結果の方がベル型に近いか？
grid.arrange(p1, p2, nrow = 1)


# サンプリング: 1000回 -------------------------------

# ＜ポイント＞
# - サンプリングを増やした場合の結果を確認する


# サンプリング回数の変更
# --- 50個の抽出を1000回行う
virtual_samples <- bowl %>% rep_sample_n(size = 50, reps = 1000)

# 確認
virtual_samples %>% print()
virtual_samples$replicate %>% table()


# リサンプリング結果の集計
# --- 赤玉の比率を集計
virtual_prop_red <-
  virtual_samples %>%
    group_by(replicate) %>%
    summarize(red = sum(color == "red")) %>%
    mutate(prop_red = red / 50)

# 確認
virtual_prop_red %>% print()


# リサンプリング分布の作成
# --- 赤が出た割合をヒストグラム化
# --- 回数が増えると正規分布に近づいてくる
# --- 0.35-0.40の頻度が多く、0.20-0.25や0.55-0.60は稀にしか出現しないことが分かる？
p3 <-
  virtual_prop_red %>%
    ggplot(aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    labs(x = "Proportion of 50 balls that were red",
         title = "Distribution of 1000 proportions red")

# プロット比較
# --- p1(左)：手動で実験
# --- p2(中)：シミュレーションで実験（33回）
# --- p3(右)：シミュレーションで実験（100回）
# --- シミュレーションより生徒の実行結果の方がベル型に近いか？
grid.arrange(p1, p2, p3, nrow = 1)


# 7-3 抽出数を変更したシミュレーション（サンプルサイズの影響）------------------------

# ＜概要＞
# - 抽出数を変化させた場合の影響を確認する（サンプリング数は固定）
#   --- 抽出数が大きいほどブレが小さくなる（Precisionが高まる）


# 関数定義
# --- 比率計算
# --- 抽出数をコントロールすることができる
calc_prop <- function(df){
  result <-
    df %>%
      group_by(replicate) %>%
      summarize(red = sum(color == "red"), 
                all = n()) %>%
      ungroup() %>%
      mutate(prop_red = red / all)

  return(result)
}


# 関数定義
# --- ヒストグラム作成
create_plot <- function(df){
  df %>%
    ggplot(aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    xlim(c(0, 0.8)) +
    labs(title = str_c("Size = ", df$all[1]," / Reps = 1000"))
}

# 仮想サンプリング
# --- シャベルの大きさを変えて1000回の抽出
virtual_samples_25  <- bowl %>% rep_sample_n(size = 25, reps = 1000)
virtual_samples_50  <- bowl %>% rep_sample_n(size = 50, reps = 1000)
virtual_samples_100 <- bowl %>% rep_sample_n(size = 100, reps = 1000)

# 確認
virtual_samples_25 %>% group_by(replicate) %>% tally()
virtual_samples_50 %>% group_by(replicate) %>% tally()
virtual_samples_100 %>% group_by(replicate) %>% tally()

# 割合を計算
virtual_prop_red_25  <- virtual_samples_25 %>% calc_prop()
virtual_prop_red_50  <- virtual_samples_50 %>% calc_prop()
virtual_prop_red_100 <- virtual_samples_100 %>% calc_prop()


# リサンプリング分布の作成
p_25  <- virtual_prop_red_25 %>% create_plot()
p_50  <- virtual_prop_red_50 %>% create_plot()
p_100 <- virtual_prop_red_100 %>% create_plot()


# リサンプリング分布の比較
# --- サンプルサイズが大きいほど｢ばらつき｣による際が小さくなり中心に寄ってくる
grid.arrange(p_25, p_50, p_100, nrow = 1)


# 標準偏差を計算
# --- シミュレーション赤玉の比率のばらつき
# --- サンプル数が多くなるほど標準偏差が小さくなる
# --- サンプルサイズが大きくなると抽出が母集団に近づくため、推測がより正確になることを示す
virtual_prop_red_25 %>% summarize(sd = sd(prop_red))
virtual_prop_red_50 %>% summarize(sd = sd(prop_red))
virtual_prop_red_100 %>% summarize(sd = sd(prop_red))


# 7-4 サンプリング・フレームワーク ---------------------------------

# ＜標準誤差＞
# - シャベルの大きさに起因する結果のばらつきを｢標準偏差｣で示したが、これには｢標準誤差｣という名前がある
#   --- ｢標準誤差｣は｢標準偏差｣の概念の1つ
#   --- 標準誤差は、推定値に起因するサンプリング変動の影響を定量化する


# データ確認
# --- 母集団
bowl %>% print()

# 赤玉の比率
# ---真の値の確認
real_red_prop <-
  bowl %>%
    summarize(sum_red = sum(color == "red"),
              sum_not_red = sum(color != "red")) %>%
    mutate(prop = sum_red / (sum_red + sum_not_red))

# 確認
real_red_prop %>% print()



# 関数定義
# --- 平均値の追加
add_vertical <- function(x){
  geom_vline(xintercept = x, linetype = "dotted", color = "red", size=2)
}


# プロット作成
pv_25  <- virtual_prop_red_25 %>% create_plot() + add_vertical(real_red_prop$prop)
pv_50  <- virtual_prop_red_50 %>% create_plot() + add_vertical(real_red_prop$prop)
pv_100 <- virtual_prop_red_100 %>% create_plot() + add_vertical(real_red_prop$prop)



# プロット比較
# --- サンプルサイズが大きいほど｢ばらつき｣による際が小さくなり中心に寄ってくる
grid.arrange(pv_25, pv_50, pv_100, nrow = 1)

