# Title     : rep_sample_n
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/rep_sample_n.html


# ＜ポイント＞
# - サイズnのサンプルのリサンプリングを実行する
# - generate()が{infer}フレームワークに基づくものに対して、rep_sample_n()はアドホックにリサンプリングを行う
# -.サンプリング分布の作成に役立つ


# ＜構文＞
# rep_sample_n(tbl, size, replace = FALSE, reps = 1, prob = NULL)

# - tbl     : サンプリングする母集団のデータフレーム
# - size    : 各サンプルのサンプルサイズ（replace=TRUEの場合はtblの行数以下でないとエラー）
# - replace : サンプリングは交換する必要がありますか？
# - reps    : シミュレーション回数
# - prob    : サンプリングされるベクトルの要素を取得するための確率の重みのベクトル



library(tidyverse)
library(infer)


# データ確認
storms %>% print()
storms %>% glimpse()


# データ選択
population <- storms %>% select(status)
population %>% print()


# リサンプリング
# --- 50個のサンプルを1000セット
samples <-
  population %>%
    rep_sample_n(size = 50, reps = 1000, replace = FALSE)


# 確認
samples %>% print()
samples %>% group_by(replicate) %>% tally()


# レコード割合の計算
# --- status == "hurricane"
p_hats <-
  samples %>%
    group_by(replicate) %>%
    summarize(prop_hurricane = mean(status == "hurricane")) %>%
    ungroup()


# 上記の計算の確認
p_hats <-
  samples %>%
    mutate(flg = ifelse(status == "hurricane", 1, 0)) %>%
    group_by(replicate) %>%
    summarize(prop_hurricane = mean(flg)) %>%
    ungroup()


# プロット作成
p_hats %>%
  ggplot(aes(x = prop_hurricane)) +
    geom_density() +
    labs(x = "p_hat", y = "Number of samples",
    title = "Sampling distribution of p_hat from 1000 samples of size 50")




#%% 復元抽出と非復元抽出 -------------------------------------

# データ確認
# --- リサンプリングデータ
pennies_sample %>% print()


# リサンプリング
# --- 50個のサンプルから50回抽出する
# --- シミュレーション回数は1回
# --- replace = FALSE（非復元抽出）
# --- 復元抽出の場合は｢size引数｣が｢tbl行数｣よりも小さくないとエラーとなる
virtual_resample_False <-
  pennies_sample %>%
  rep_sample_n(size = 50, replace = FALSE, reps = 1)


# リサンプリング
# --- 50個のサンプルから50回抽出する
# --- シミュレーション回数は1回
# --- replace = TRUE（復元抽出）
virtual_resample_True <-
  pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 1)


# 確認
# --- 復元抽出は同じＩＤを抽出することがある
virtual_resample_False %>% arrange(ID)
virtual_resample_True  %>% arrange(ID)
