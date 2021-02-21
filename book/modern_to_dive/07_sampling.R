# Title     : Sampling
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/15
# URL       : https://moderndive.com/7-sampling.html


# ＜概要＞
# - サンプリングを学習することにより、統計的推論に必要な｢信頼区間｣と｢仮説検定｣の基礎を形成する
# - サンプリングから｢正確性｣と｢精度｣の概念を理解する
# - データのストーリーを伝えるための可視化を行う
# - 7章では｢比率｣に着目しているが、｢平均｣｢差｣｢回帰｣など様々な統計量に対してシミュレーションが可能



# ＜本章の流れ＞
# 7-1 ボウルからのサンプリング活動（手動サンプリングの確認）
# 7-2 仮想サンプリング（シミュレーション回数を自由コントロール）
# 7-3 サンプリング・フレームワーク（サンプリングから｢正確性｣と｢精度｣の概念を理解する）




library(tidyverse)
library(infer)
library(moderndive)
library(gridExtra)



#%% 7-1 ボウルからのサンプリング活動 ---------------------------------

# ＜概要＞
# - ボールに赤白の玉が入っているが、赤の割合はどれくらいだろうか？
# - 全て数えるのが最も単純で正確な方法（手間がかかるので避けたい）
# - シャベルを用いてサンプリングを行う



# ＜サンプリング活動の目的＞
# 1. サンプリング変動の影響を理解する。
# 2. サンプリング変動に対するサンプルサイズの影響を理解する。


# ＜サンプリング方法＞
# - シャベルは1回あたり50個の抽出が可能
# - クラスメイト33人が1回ずつの実験を行っている
# - 抽出したボールは元に戻している（復元抽出）



# データ確認
# --- 担当者がシャベルでボール救いをした結果
#     --- 33人が実行しているので33レコード
# --- 担当者ごとにreplicateの番号が付いている
tactile_prop_red %>% print()
tactile_prop_red %>% glimpse()
tactile_prop_red %>% skimr::skim()


# プロット作成
# --- 赤が出た割合をヒストグラム化
# --- 赤の割合がなんとなく見えてくるが、まだヒストグラムが荒く確信が持てない
p1 <-
  tactile_prop_red %>%
    ggplot(aes(x = prop_red)) +
      geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
      labs(x = "Proportion of 50 balls that were red",
           title = "Distribution of 33 proportions red")


print(p1)


#%% 7-2 仮想サンプリング ---------------------------------

# ＜概要＞
# - コンピュータでサンプリングを模倣する
# - 1回の試行について検討する


# データ確認
# --- 2400行（ボールに入っている全ての玉の数）
# --- 現実には玉に番号は付いていないが、シミュレーションでは便宜上番号をつけておく
bowl %>% print()



#%% シャベル抽出の再現 ---------------------------------

# サンプリング:1回 ----------------

# 1回のサンプリング（バーチャルシャベル）
# --- infer::rep_sample_n()を使用する
# --- サイズnのサンプルの繰り返しサンプリングを実行する
# --- replicate列が作成されて、試行回数が管理される
virtual_shovel <- bowl %>% rep_sample_n(size = 50, reps = 1)
virtual_shovel %>% print()



# 赤の玉の数を集計
# --- フラグを付けて合計したうえで割合を計算
# --- ｢color == "red"｣とすることで、if文を書かなくてもTRUE/FALSEが判定できる
virtual_shovel %>%
  mutate(is_red = color == "red") %>%
  summarize(num_red = sum(is_red)) %>%
  mutate(prop_red = num_red / 50)


# 赤の玉の数を集計
# --- 条件集計の部分をコンパクトに記述
virtual_shovel %>%
  summarize(num_red = sum(color == "red")) %>%
  mutate(prop_red = num_red / 50)



# サンプリング:33回 ----------------

# サンプリング回数の変更
# --- 50個の抽出を33回行う（7-1の実験と同様）
virtual_samples <- bowl %>% rep_sample_n(size = 50, reps = 33)
virtual_samples %>% print()
virtual_samples %>% group_by(replicate) %>% tally()


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

print(p2)


# プロット比較
# --- p1(左)：手動で実験
# --- p2(右)：シミュレーションで実験
# --- シミュレーションより生徒の実行結果の方がベル型に近いか？
grid.arrange(p1, p2, nrow = 1)




#%% 仮想サンプリングを1000回に増やす ---------------------------------

# サンプリング回数の変更
# --- 1000回とする
virtual_samples <- bowl %>% rep_sample_n(size = 50, reps = 1000)
virtual_samples %>% print()
virtual_samples %>% group_by(replicate) %>% tally()
virtual_samples$replicate %>% unique() %>% length()


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

print(p3)



#%% 抽出数が異なるシャベルを使う ---------------------------------

# 関数定義
# --- 比率計算
# --- 抽出数をコントロールすることができる
calc_prop <- function(df, n){
  result <-
    df %>%
      group_by(replicate) %>%
      summarize(red = sum(color == "red")) %>%
      ungroup() %>%
      mutate(prop_red = red / n)

  return(result)
}


# 関数定義
# --- ヒストグラム作成
create_plot <- function(df, n){
  df %>%
    ggplot(aes(x = prop_red)) +
    geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
    xlim(c(0, 0.8)) +
    labs(x = str_c("Proportion of ", n," balls that were red"), title = n)
}


# シャベルの大きさを変えて1000回の抽出
virtual_samples_25  <- bowl %>% rep_sample_n(size = 25, reps = 1000)
virtual_samples_50  <- bowl %>% rep_sample_n(size = 50, reps = 1000)
virtual_samples_100 <- bowl %>% rep_sample_n(size = 100, reps = 1000)


# 確認
virtual_samples_25 %>% group_by(replicate) %>% tally()
virtual_samples_50 %>% group_by(replicate) %>% tally()
virtual_samples_100 %>% group_by(replicate) %>% tally()


# 割合を計算
virtual_prop_red_25  <- virtual_samples_25 %>% calc_prop(n =25)
virtual_prop_red_50  <- virtual_samples_50 %>% calc_prop(n =50)
virtual_prop_red_100 <- virtual_samples_100 %>% calc_prop(n =100)


# リサンプリング分布の作成
p_25  <- virtual_prop_red_25 %>% create_plot(n = 25)
p_50  <- virtual_prop_red_50 %>% create_plot(n = 50)
p_100 <- virtual_prop_red_100 %>% create_plot(n = 100)


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




#%% 7-3 サンプリング・フレームワーク ---------------------------------

# ＜標準誤差＞
# - シャベルの大きさに起因する結果のばらつきを｢標準偏差｣で示したが、これには｢標準誤差｣という名前がある
#   --- ｢標準誤差｣は｢標準偏差｣の概念の1つ
#   --- 標準誤差は、推定値に起因するサンプリング変動の影響を定量化する

# - サンプルサイズが上昇すると標準誤差は低下する
#


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
pv_25  <- virtual_prop_red_25 %>% create_plot(n = 25) + add_vertical(real_red_prop$prop)
pv_50  <- virtual_prop_red_50 %>% create_plot(n = 50) + add_vertical(real_red_prop$prop)
pv_100 <- virtual_prop_red_100 %>% create_plot(n = 100) + add_vertical(real_red_prop$prop)



# プロット比較
# --- サンプルサイズが大きいほど｢ばらつき｣による際が小さくなり中心に寄ってくる
grid.arrange(pv_25, pv_50, pv_100, nrow = 1)



# ＜正確性：Accuracy＞
# - 見積もりが真の値にどれくらい近いかを示す
# - シミュレーション回数を増やすことで正確性が高まってくる（真の分布に近づいていく）
# - 中心極限定理により、サンプリング分布は真の値を中心とした正規分布にしたがう
# - 射的で的の真ん中に近づく（FIGURE 7.16）


# ＜精度：Precision＞
# - 推定値の一貫性を示す
# - ショベルを大きくするほど母集団に近づくので,シミュレーションごとの精度が高まる（ばらつきが小さくなる）
# - FIGURE 7.16




