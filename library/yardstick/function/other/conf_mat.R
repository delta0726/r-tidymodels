# Title     : conf_mat
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/19
# URL       : https://yardstick.tidymodels.org/reference/conf_mat.html


# ＜ポイント＞
# - 混合行列は縦方向と横方向に出力するものが決まっている
#   --- 縦方向に予想、横方向に正解
# - グループ集計への対応、


# ＜構文＞
# conf_mat(data, truth, estimate, dnn = c("Prediction", "Truth"), ...)




library(yardstick)
library(tidyverse)
library(magrittr)


# データ準備
data("hpc_cv")


# データ準備
# --- VF:Lには確率が入っている
# --- リサンプリングが行われている
hpc_cv %>% as_tibble()


#%% table関数による混合行列 -------------------------------------------

# データ集計
# --- table関数でも混合行列を作成できる
# --- 縦方向と横方向をデータの順番で指定しなければならない
hpc_cv %>% select(obs, pred) %>% table()
hpc_cv %>% select(pred, obs) %>% table()



#%% 単純な混合行列 -------------------------------------------

# 混合行列
# --- 1つのAssessmentセットを抽出して実行
# --- 縦：予想
# --- 横：正解
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  conf_mat(truth = obs, estimate = pred)



#%% 複数の混合行列の平均化 -------------------------------------------

# グループごとの混合行列
# --- tibbleにconf_matオブジェクトを格納して出力される
# --- 混合行列を象限ごとに集計
cells_per_resample <-
  hpc_cv %>%
    group_by(Resample) %>%
    conf_mat(truth = obs, estimate = pred) %>%
    mutate(tidied = map(conf_mat, tidy)) %>%
    unnest(tidied)


# 確認
# --- 160個の出力
# --- 4クラス * 4クラス * 10Fold
cells_per_resample %>% print()


# Foldごとに平均値を算出
# --- Foldごとの総数から出現割合を算出して、Foldごとに平均
counts_per_resample <-
  hpc_cv %>%
    group_by(Resample) %>%
    summarize(total = n()) %>%
    left_join(cells_per_resample, by = "Resample") %>%
    mutate(prop = value/total) %>%
    group_by(name) %>%
    summarise(prop = mean(prop)) %>%
    ungroup()


# 確認
counts_per_resample %>% print()


# 行列に変換
mean_cmat <-
  counts_per_resample$prop %>%
    matrix(byrow = TRUE, ncol = 4) %>%
    set_colnames(levels(hpc_cv$obs)) %>%
    set_rownames(levels(hpc_cv$obs)) %>%
    round(3)


# 確認
mean_cmat %>% print()



#%% 混合行列をロング型に変換 -------------------------------------------

# 混合行列
hpc_cv %>%
  filter(Resample == "Fold01") %>%
  conf_mat(truth = obs, estimate = pred) %>%
  tidy()




#%% プロット化 -------------------------------------------

# 混合行列の作成
cm <-
  hpc_cv %>%
    filter(Resample == "Fold01") %>%
    conf_mat(truth = obs, estimate = pred)


# モザイクプロット
# --- 全体と行/列の総数から面積に置きなおす
cm %>% autoplot(type = "mosaic")


# headmap
cm %>% autoplot(type = "heatmap")
