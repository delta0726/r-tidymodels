# Title     : Model Assessment by using purrr
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/09
# URL       : https://rsample.tidymodels.org/articles/Working_with_rsets.html



# ＜ポイント＞
# - 実際のモデル構築ではクロスバリデーションなどで多くのデータセットで同時にモデル訓練を行う
# - rsampleでは、訓練データとテストデータを分割したrsplitオブジェクトを基本とする
# - vfold_cv()などではrsplitオブジェクトをlisted Dataframeとして扱う
# - listed Dataframeはpurrrによる一括操作を前提としている


library(tidyverse)
library(rsample)


# データロード
data("attrition", package = "modeldata")


# データ概要
attrition %>% as_tibble()
attrition %>% glimpse()
attrition %>% names()



#%% 前処理 -----------------------------------------------

# ラベル頻度
attrition$Attrition %>% table()


# モデルイメージ
glm(Attrition ~ JobSatisfaction + Gender + MonthlyIncome, data = attrition, family = binomial)


# モデル定義
mod_form <- as.formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome)


# バリデーションデータの作成
# --- 10個のfoldを作成
# --- 10セットのリピート
set.seed(4622)
rs_obj <- vfold_cv(attrition, v = 10, repeats = 10)
rs_obj %>% print()



#%% モデルと処理フロー -----------------------------------------------


# テスト用
splits <- rs_obj$splits[[1]]
models <- mod_form

## splits will be the `rsplit` object with the 90/10 partition
holdout_results <- function(splits, models) {

  # モデル構築
  # --- 訓練データ(90%)
  mod <- glm(models, data = analysis(splits), family = binomial)

  # 出力データ
  # --- 評価データ(10%)
  holdout <- splits %>% assessment()

  # 予測データの取得
  # --- モデルを評価データに適用
  res <- mod %>% broom::augment(newdata = holdout)

  # 予測結果の抽出
  lvls <- holdout$Attrition %>% levels()
  predictions <- factor(ifelse(res$.fitted > 0, lvls[2], lvls[1]), levels = lvls)

  # 答え合わせ
  # --- 予測結果と正解データの一致を確認
  res$correct <- predictions == holdout$Attrition
  # Return the assessment data set with the additional columns

  # 結果出力
  res
}



#%% 1つのfoldにおける計算 -----------------------------------------------

# foldの確認
rs_obj$splits[[1]] %>% print()
rs_obj$splits[[1]] %>% class()
rs_obj$splits[[1]] %>% training() %>% as_tibble()
rs_obj$splits[[1]] %>% testing() %>% as_tibble()


# モデル適用
example <- rs_obj$splits[[1]] %>% holdout_results(mod_form)
example %>% print()


# データサイズ
# --- 出力データ
example %>% dim()

# データサイズ
# --- 元データ
rs_obj$splits[[1]] %>% assessment() %>% dim()


## 新しく追加された列
example[1:10, setdiff(names(example), names(attrition))]




#%% purrrを活用した一括処理 -----------------------------------------------

# 元データ
rs_obj %>% print()


# モデル適用
rs_obj$results <-
  rs_obj$splits %>%
    map(holdout_results, mod_form)


# Accuracyの取得
rs_obj$accuracy <-
  rs_obj$results %>%
    map_dbl(function(x) mean(x$correct))


# Accuracy
rs_obj$accuracy %>% summary()


