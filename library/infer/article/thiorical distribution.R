# Title     : Thiorical Distribution
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/13
# URL       : https://infer.netlify.app/articles/infer.html


# ＜概要＞
# - {infer}は特定の検定でなく、一般的な仮説検定のフローを提供する
# - フローは以下の4つの主要関数によって提供される
#   --- specify()     : 関心のある変数または変数間の関係を指定。
#   --- hypothesize() : 帰無仮説を宣言。
#   --- generate()    : 帰無仮説を反映するデータを生成。
#   --- calculate()   : 生成されたデータから統計の分布を計算してNull分布を形成







#%% 理論分布の作成 -------------------------------------


# ＜ポイント＞
# - inferは｢カイ二乗｣｢t｣｢f｣の理論分布を作成するkとができる
# - generate()ステップはスキップする




# 帰無分布から統計量を作成
# --- generate()で実データからシミュレーション
null_f_distn <-
  gss %>%
   specify(age ~ partyid) %>%
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "F")



# 理論分布から統計量を作成
# --- generate()をスキップ
null_f_distn_theoretical <-
  gss %>%
   specify(age ~ partyid) %>%
   hypothesize(null = "independence") %>%
   calculate(stat = "F")



# 実データのF値の計算
F_hat <-
  gss %>%
    specify(age ~ partyid) %>%
    calculate(stat = "F")


# プロット
# --- 理論分布のみ
null_f_distn_theoretical %>%
  visualize(method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")



# プロット
# --- 理論分布と実分布
null_f_distn %>%
  visualize(method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")




