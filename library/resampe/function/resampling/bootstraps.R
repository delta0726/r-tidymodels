# Title     : bootstraps
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04
# URL       : https://rsample.tidymodels.org/reference/bootstraps.html



# ＜概要＞
# - ブートストラップ法によるリサンプリングを行う
# - 訓練データは復元抽出で元のデータセットと同じサンプル数になる
# - 検証データは非復元抽出となる


# ＜構文＞
# bootstraps(data, times = 25, strata = NULL, breaks = 4, apparent = FALSE, ...)
#
# times    : リサンプリング回数
# strata   : 層別リサンプリングの基準列
# breaks   : 数値で層化リサンプリングする場合の分位数
# apparent :



library(tidyverse)
library(rsample)



# データ作成
# --- virginicaの一部を削除
iris2 <- iris[1:130, ] %>% mutate(no = row_number())
iris2 %>% group_by(Species) %>% tally()



#%% 概要 ------------------------------------------------


# ブートストラップ・リサンプリング
resample1 <- iris2 %>% bootstraps(times = 3)
resample1 %>% print()


# 要素の確認
resample1$splits[[1]] %>% print()
resample1$splits[[1]] %>% class()


# 分析データは復元抽出
# --- 元のデータセットの同じレコード数
# --- 同じレコードが含まれることがある
resample1$splits[[1]] %>% analysis() %>% as_tibble()
resample1$splits[[1]] %>% analysis() %>% arrange(no)


# 評価データは非復元抽出
# --- 同じレコードは含まれない
resample1$splits[[1]] %>% assessment() %>% as_tibble()
resample1$splits[[1]] %>% assessment() %>% arrange(no)



#%% リサンプリングの性質 ------------------------------------------------


# 関数定義
# --- virginicaが含まれる割合
pct_virginica <- function(x) {
          dat <- as.data.frame(x)$Species
          mean(dat == "virginica")
        }


# 単純なケース
# --- 割合は全て異なる
set.seed(13)
resample1 <- iris2 %>% bootstraps(times = 3)
resample1$splits %>% map_dbl(pct_virginica)


# 層化リサンプリング
# --- グループデータによる層化
# --- 同じ割合で含まれる
set.seed(13)
resample2 <- iris2 %>% bootstraps(strata = "Species", times = 3)
resample2$splits %>% map_dbl(pct_virginica)


# 層化リサンプリング
# --- 数値データによる層化
# --- 要確認！！！
set.seed(13)
resample3 <- iris2 %>% bootstraps(strata = "Sepal.Length", breaks = 6, times = 3)
resample3$splits %>% map_dbl(pct_virginica)

