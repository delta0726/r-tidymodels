# **********************************************************************************
# Library   : workflowsets
# Title     : Evaluating different predictor sets
# Created by: Owner
# Created on: 2021/08/23
# URL       : https://workflowsets.tidymodels.org/reference/autoplot.workflow_set.html
# **********************************************************************************


# ＜概要＞
# - ワークフローセットを活用してLOOモデルを作成して予測精度を確認
#   --- 特定の特徴量を除外した場合の効果をスマートに確認


# ＜目次＞
# 0 準備
# 1 モデル用データの作成
# 2 モデル構築
# 3 ワークフローセットの作成
# 4 ワークフローセットのリサンプル学習
# 5 メトリックの集計
# 6 予測精度の比較


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(workflowsets)


# データロード
data(mlc_churn, package = "modeldata")

# データ確認
mlc_churn %>% print()
mlc_churn %>% glimpse()


# 1 モデル用データの作成 --------------------------------------------------------------

# データ分割
# --- 層別サンプリング
set.seed(1)
trn_tst_split <- mlc_churn %>% initial_split(strata = churn)

# リサンプリングデータの作成
# --- 訓練データから10Foldのデータセットを作成
folds <- 
  trn_tst_split %>% 
    training() %>% 
    vfold_cv(strata = churn)

# 確認
folds %>% print()
folds$splits[[1]]


# 2 モデル構築 ------------------------------------------------------------------------

# モデル構築
# --- ベースモデル
lr_model <- logistic_reg() %>% set_engine("glm")

# フォーミュラの作成
# --- Leave one outフォーミュラ
formulas <- leave_var_out_formulas(churn ~ ., data = mlc_churn)

# 確認
formulas %>% print()
formulas %>% length()
formulas %>% names()


# 3 ワークフローセットの作成 ----------------------------------------------------------

# ワークフローセットの定義
# --- 前処理はフォーミュラのみ
churn_workflows <- 
  workflow_set(preproc = formulas, 
               models = list(logistic = lr_model))

# 確認
# --- idは｢削除した特徴量＋モデル名｣で表現される
churn_workflows %>% print()
churn_workflows$wflow_id

# チェック用
mlc_churn %>% select(-churn) %>% names()



# 4 ワークフローセットのリサンプル学習 ---------------------------------------------------

# リサンプル学習
# --- ワークフローセットのモデルごとに10Foldでリサンプル学習
# --- 単純な計算だが量が多いため計算時間がかかる（200回 = 20モデル*10Fold）
churn_workflows <- 
  churn_workflows %>% 
    workflow_map("fit_resamples", resamples = folds)

# 確認
churn_workflows %>% print()
churn_workflows %>% collect_metrics(summarize = FALSE) %>% filter(.metric == "accuracy")


# 5 メトリックの集計 ------------------------------------------------------------------

# メトリックの抽出
# --- 全モデル
roc_values <- 
  churn_workflows %>% 
    collect_metrics(summarize = FALSE) %>% 
    filter(.metric == "roc_auc") %>% 
    mutate(wflow_id = gsub("_logistic", "", wflow_id))

# メトリックの抽出
# --- フルモデル
full_model <- 
  roc_values %>% 
    filter(wflow_id == "everything") %>% 
    select(full_model = .estimate, id)

# メトリックの抽出
# --- LOOモデル
differences <- 
  roc_values %>% 
    filter(wflow_id != "everything") %>% 
    full_join(full_model, by = "id") %>% 
    mutate(performance_drop = full_model - .estimate)

# 集計
summary_stats <- 
  differences %>% 
  group_by(wflow_id) %>% 
  summarize(std_err = sd(performance_drop)/sum(!is.na(performance_drop)), 
            performance_drop = mean(performance_drop),
            lower = performance_drop - qnorm(0.975) * std_err,
            upper = performance_drop + qnorm(0.975) * std_err,
            .groups = "drop") %>% 
  mutate(wflow_id = factor(wflow_id), 
         wflow_id = reorder(wflow_id, performance_drop))

# 確認
summary_stats %>% print()
summary_stats %>% filter(lower > 0)


# 6 予測精度の比較 ------------------------------------------------------------------------

# プロット作成
summary_stats %>% 
  ggplot(aes(x = performance_drop, y = wflow_id)) + 
    geom_point() + 
    geom_errorbar(aes(xmin = lower, xmax = upper), width = .25) +
    ylab("")
  
