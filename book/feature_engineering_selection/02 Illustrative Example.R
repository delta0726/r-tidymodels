# *********************************************************************************************************************
# Title     : Chapter-2 Illustrative Example
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/30
# URL       : https://bradleyboehmke.github.io/HOML/process.html
#           : https://koalaverse.github.io/homlr/notebooks/02-modeling-process.nb.html 
# *********************************************************************************************************************


# ＜注意事項＞
# - H2Oの使用はRstudioの方が安定する
#


# ＜目次＞
# 2.0 準備
# 2.1 データ分割
# 2.2 前処理
# 2.3 探索的データプロセス
# 2.4 予測モデリング
# 2.5 その他の考慮次項



# 2.0 準備 ----------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(corrplot)
library(gridExtra)
library(caret)
library(utils)
library(pROC)
library(plotly)


# プロットテーマ
theme_set(theme_bw())


# ディレクトリ設定
setwd("I:/Project/R/tidymodels/book/ml_feature")


# データロード
load("data/Ischemic_Stroke/stroke_data.RData")
load("data/Ischemic_Stroke/interactions.RData")


# データ確認
stroke_train %>% as_tibble()
stroke_train %>% glimpse()


# フィールド定義
VC_preds <-
  c("CALCVol", "CALCVolProp", "MATXVol", "MATXVolProp", "LRNCVol",
    "LRNCVolProp", "MaxCALCArea", "MaxCALCAreaProp", "MaxDilationByArea",
    "MaxMATXArea", "MaxMATXAreaProp", "MaxLRNCArea", "MaxLRNCAreaProp",
    "MaxMaxWallThickness", "MaxRemodelingRatio", "MaxStenosisByArea",
    "MaxWallArea", "WallVol", "MaxStenosisByDiameter")


# フィールド定義
risk_preds <-
  c("age", "sex", "SmokingHistory", "AtrialFibrillation", "CoronaryArteryDisease",
    "DiabetesHistory", "HypercholesterolemiaHistory", "HypertensionHistory")



# 2.1 データ分割 -----------------------------------------------------------------

# ＜ポイント＞
# - 分類問題の場合は、結果クラスでランダム分割を行う層別のデータ分割を行うとよい


# データ数
stroke_train %>% dim()
stroke_test %>% dim()


# ラベルデータの確認
# --- 分類問題
stroke_train$Stroke %>% table()


# データカウント
count_train <- stroke_train %>% count(Stroke) %>% mutate(Data = "Training")
count_test  <- stroke_test %>% count(Stroke) %>% mutate(Data = "Testing")
count_train %>% bind_rows(count_test) %>% spread(Stroke, n)


# 2.2 前処理 ---------------------------------------------------------------------

# ＜前処理の内容＞
# - 分布の把握、欠損値の程度、常値の把握、特徴量と予測子の関係、特徴量間の関係性



# 2.2.1 データ変換 -----------------------------------------------

# ＜ポイント＞
# - データが指数関数的に増加する場合は対数変換が有効


# ＜参考＞
# 6-1 Transformation
# https://bookdown.org/max/FES/numeric-one-to-one.html#numeric-one-to-one


# ヒストグラム
# --- 特徴量：MaxLRNCArea
# --- 分布が歪んでいる
fig_2_2_a <-
  stroke_train %>%
    bind_rows(stroke_test) %>%
    ggplot(aes(x = MaxLRNCArea)) +
    geom_histogram(bins = 15, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +
    xlab("MaxLRNCArea") +
    ylab("Frequency") +
    ggtitle("(a)") +
    theme_bw()


# ヒストグラム
# --- 特徴量：MaxLRNCArea
# --- YeoJohnson変換で正規化
fig_2_2_b <-
  stroke_train %>%
    bind_rows(stroke_test) %>%
    recipe(Stroke ~ ., data = .) %>%
    step_YeoJohnson(all_predictors()) %>%
    prep() %>%
    juice() %>%
    ggplot(aes(x = MaxLRNCArea)) +
    geom_histogram(bins = 15, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +
    xlab("Transformed MaxLRNCArea") +
    ylab("Frequency") +
    ggtitle("(b)") +
    theme_bw()


# プロット表示
grid.arrange(fig_2_2_a, fig_2_2_b, nrow = 1)



# 2.2.2 相関関係の把握 -----------------------------------------------

# ＜ポイント＞
# - 概ね相関は低い特徴量が多い
# - corrplot()でhclustを用いることで相関の高い特徴量が相関係数行列で近くに表示されている


# ＜参考＞
# 6-1 Transformation
# https://bookdown.org/max/FES/stroke-preprocessing.html#fig:stroke-corrMatrix


# レシピ作成
# --- 平均がゼロとなるように基準化
# --- データ分布の正規化
rec <-
  recipe(Stroke ~ ., data = stroke_train) %>%
    step_center(VC_preds) %>%
    step_scale(VC_preds) %>%
    step_YeoJohnson(VC_preds) %>%
    prep()


# 相関関係をプロット
# --- corrplot()を使用
rec %>%
  juice() %>%
  select(-one_of(c("Stroke", "NASCET", risk_preds))) %>%
  cor() %>%
  corrplot(addgrid.col = rgb(0, 0, 0, .05), order = "hclust")



# 2.2.3 高相関の特徴量を除外 -----------------------------------------------

# ＜ポイント＞
# - 相関プロットで対角行列状に相関の高い特徴量があることが確認できた
#   --- 相関の高い特徴量が重複しないようにする必要がある
#   --- 関数定義してプロセス上で除外


# 関数定義
compare_models_1way <- function(a, b, metric = a$metric[1], ...) {
  mods <- list(a, b)
  rs <- resamples(mods)
  diffs <- diff(rs, metric = metric[1], ...)
  diffs$statistics[[1]][[1]]
}



# 2.3 探索的データプロセス -----------------------------------------

# ＜ポイント＞
# - モデル構築の前に特徴量の予測力を確認して取捨選別することも有効（特徴量フィルタリング）
# - 単純なモデルで予測力を確認
#   --- NULLモデルをベンチマークとした予測力の確認
# - プロットで視覚的に確認
#   --- ボックスプロット
#   --- ROCカーブ



# 関数定義
compare_models_1way <- function(a, b, metric = a$metric[1], ...) {
  mods <- list(a, b)
  rs <- resamples(mods)
  diffs <- diff(rs, metric = metric[1], ...)
  diffs$statistics[[1]][[1]]
}


# フィールド定義
VC_preds <-
  c("CALCVol", "CALCVolProp", "MATXVol", "MATXVolProp", "LRNCVol",
    "LRNCVolProp", "MaxCALCArea", "MaxCALCAreaProp", "MaxDilationByArea",
    "MaxMATXArea", "MaxMATXAreaProp", "MaxLRNCArea", "MaxLRNCAreaProp",
    "MaxMaxWallThickness", "MaxRemodelingRatio", "MaxStenosisByArea",
    "MaxWallArea", "WallVol", "MaxStenosisByDiameter")


# フィールド定義
risk_preds <-
  c("age", "sex", "SmokingHistory", "AtrialFibrillation", "CoronaryArteryDisease",
    "DiabetesHistory", "HypercholesterolemiaHistory", "HypertensionHistory")




# 2.3.1 特徴量の予測力 ------------------------------------------------

# Null Modelの定義 ------------------------------------------------

# ＜ポイント＞
# - Null Model(帰無モデル)とは、説明変数を含まない切片のみからなる統計モデルのことをいう
# - 分類問題のパフォーマンスのベースラインとしてNull Model(帰無モデル)を定義する


# 切片項のみのデータセット
null_mat <- data.frame(intercept = rep(1, nrow(stroke_train)))
null_mat %>% as_tibble()


# トレーニング設定
ctrl <-
  trainControl(method = "repeatedcv", repeats = 5,
               classProbs = TRUE,
               summaryFunction = twoClassSummary)


# モデル定義
# --- X: 切片項のみ
# --- Y: stroke_train$Stroke
set.seed(63331)
null_mod <- train(x = null_mat,
                  y = stroke_train$Stroke,
                  preProc = "YeoJohnson",
                  method = "glm",
                  metric = "ROC",
                  trControl = ctrl)



# 実際のモデルの定義 ------------------------------------------------

# パフォーマンスリスト
# --- 空のリストを定義
# --- シミュレーション結果を格納
one_predictor_res <-
  data.frame(Predictor = c(VC_preds, risk_preds),
             Improvement = NA,
             Pvalue = NA,
             ROC = NA,
             stringsAsFactors = FALSE)


# 確認
one_predictor_res %>% print()


# シミュレーション
# --- 分類問題
# --- glmでモデリング
# --- ROCを用いたクロスバリデーション
i <- 1
for (i in 1:nrow(one_predictor_res)) {
  set.seed(63331)
  var_mod <- train(Stroke ~ .,
                   data = stroke_train[, c("Stroke", one_predictor_res$Predictor[i])],
                   method = "glm",
                   metric = "ROC",
                   trControl = ctrl)
  tmp_diff <- compare_models_1way(var_mod, null_mod, alternative = "greater")
  one_predictor_res$ROC[i] <- getTrainPerf(var_mod)[1, "TrainROC"]
  one_predictor_res$Improvement[i] <- tmp_diff$estimate
  one_predictor_res$Pvalue[i] <- tmp_diff$p.value
}


# p値によるランキング
# --- 統計学での回帰モデルではt値やp値が重視
# --- risk_predsのみ表示
one_predictor_res %>%
  dplyr::filter(Predictor %in% risk_preds) %>%
  arrange(Pvalue)



# 2.3.2 ボックスプロット ------------------------------------------------

# ＜ポイント＞
# - 特徴量ごとにROCカーブを描いてみると、特徴量に判別能力があるかを確認できる

# 処理用データ
df_raw <- stroke_train %>% dplyr::select(Stroke, !!!VC_preds)


# レシピ作成
# --- データ正規化
rec <-
  recipe(Stroke ~ ., data = df_raw) %>%
  step_YeoJohnson(all_predictors()) %>%
  prep()


# プロット用データ
# --- ベース
vc_pred <- rec %>% juice() %>% gather(Predictor, value, -Stroke)
vc_pred %>% print()


# テキスト表示データ
pred_max <-
  vc_pred %>%
  group_by(Predictor) %>%
  summarize(max_val = max(value)) %>%
  inner_join(one_predictor_res %>% dplyr::select(Pvalue, Predictor)) %>%
  mutate(
    x = 1.5,
    value = 1.25 * max_val,
    label = paste0("p-value: ", format.pval(Pvalue, digits = 2, sci = FALSE, eps = .0001))
  )


# ファクター順序の定義
new_order <- pred_max$Predictor[order(pred_max$Pvalue)]
vc_pred   <- vc_pred %>% mutate(Predictor = factor(Predictor, levels = new_order))
pred_max  <- pred_max %>% mutate(Predictor = factor(Predictor, levels = new_order))


# プロット作成
fig_2_4 <-
  vc_pred %>%
    ggplot(aes(x = Stroke, y = value)) +
    geom_boxplot() +
    geom_point(alpha = 0.3, cex = .5) +
    geom_text(data = pred_max, aes(x = x, label = label), size = 3) +
    facet_wrap(~Predictor, scales = "free_y") +
    ylab("")


# 確認
print(fig_2_4)



# 2.3.3 ROCカーブ -------------------------------------------------------

# ＜ポイント＞
# - 特徴量ごとにROCカーブを描いてみると、特徴量に判別能力があるかを確認できる


# データ確認
stroke_train %>% glimpse()


# ROCカーブの作成
fig_2_5 <-
  roc_curve(stroke_train, Stroke, MaxRemodelingRatio) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_abline(alpha = .5, lty = 2) +
  geom_path()


# 確認
print(fig_2_5)




# 2.3.4 相互効果 -------------------------------------------------------

# ＜ポイント＞
# - 予測子の数が少ないデータの場合は全てのペアワイズ相互作用を作成できる
#   --- 数値予測子の場合、交互作用は各予測子の値を乗算することによって簡単に生成することが可能
#
#


# 特徴量のペアワイズを作成
pairs <-
  VC_preds %>%
    combn(2) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate(Improvement = NA,
           Pvalue = NA,
           ROC = NA)


# 確認
pairs %>% as_tibble()


# シミュレーション
# --- ペアワイズの特徴量で相互効果の｢あり/なし｣でデータを分割
i <- 1
for (i in 1:nrow(pairs)) {
  tmp_vars <- c("Stroke", pairs$V1[i], pairs$V2[i])
  set.seed(63331)
  main_eff <- train(Stroke ~ .,
                    data = stroke_train[, tmp_vars],
                    preProc = c("center", "scale", "YeoJohnson"),
                    method = "glm",
                    metric = "ROC",
                    trControl = ctrl)
  set.seed(63331)
  main_int <- train(Stroke ~ (.)^2,
                    data = stroke_train[, tmp_vars],
                    preProc = c("center", "scale", "YeoJohnson"),
                    method = "glm",
                    metric = "ROC",
                    trControl = ctrl)
  tmp_diff <- compare_models_1way(main_int, main_eff, alternative = "greater")
  pairs$ROC[i] <- getTrainPerf(main_eff)[1, "TrainROC"]
  pairs$Improvement[i] <- tmp_diff$estimate
  pairs$Pvalue[i] <- tmp_diff$p.value
}




retained_pairs <-
  pairs %>%
  dplyr::filter(ROC > 0.5  & Pvalue <= 0.2)

# ------------------------------------------------------------------------------
# Figure 2.6
# https://bookdown.org/max/FES/stroke-tour.html#fig:stroke-interactionScreening

vol_plot <-
  pairs %>%
  dplyr::filter(ROC > 0.5) %>%
  mutate(Term = paste(V1, "by", V2, "\nROC:", round(ROC, 2))) %>%
  ggplot(aes(x = Improvement, y = -log10(Pvalue))) +
  xlab("Improvement") +
  geom_point(alpha = .2, aes(size = ROC, text = Term))

vol_plot <- ggplotly(vol_plot, tooltip = "Term")
# vol_plot

# ------------------------------------------------------------------------------

## Create interaction formula
int_form <-
  pairs %>%
  dplyr::filter(ROC > 0.5  & Pvalue <= 0.2 & Improvement > 0) %>%
  mutate(form  = paste0(V1, ":", V2)) %>%
  pull(form) %>%
  paste(collapse = "+")
int_form <- paste("~", int_form)
int_form <- as.formula(int_form)






# 2.4 予測モデリング ---------------------------------------------



risk_train <-
  stroke_train %>%
  dplyr::select(one_of(risk_preds), Stroke)

image_train <-
  stroke_train %>%
  dplyr::select(one_of(VC_preds), Stroke)



# ------------------------------------------------------------------------------


fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
internal_ctrl = trainControl(method = "none", classProbs = TRUE,
                             allowParallel = FALSE)

lrFuncsNew <- caretFuncs
lrFuncsNew$summary <- fiveStats
rfeCtrl <- rfeControl(functions = lrFuncsNew,
                      method = "repeatedcv",
                      repeats = 5,
                      rerank = FALSE,
                      returnResamp = "all",
                      saveDetails = TRUE,
                      verbose = FALSE)

# ------------------------------------------------------------------------------
# Here I'll run in parallel using doMC. For Windows, a different package can
# be used.

library(doMC)
registerDoMC(cores = parallel::detectCores(logical = FALSE) - 1)

# ------------------------------------------------------------------------------
# RFE procedure using risk predictors

# All pair-wise interactions
risk_int_filtered_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_interact(~ all_predictors():all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_int_filtered_rfe <- rfe(
  risk_int_filtered_recipe,
  data = risk_train,
  sizes = 1:36,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# Main effects
risk_main_filtered_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_main_filtered_rfe <- rfe(
  risk_main_filtered_recipe,
  data = risk_train,
  sizes = 1:8,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------
# RFE procedure using imaging predictors

img_int_filtered_recipe <-
  recipe(Stroke ~ ., data = image_train) %>%
  step_interact(int_form)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_int_filtered_rfe <- rfe(
  img_int_filtered_recipe,
  data = image_train,
  sizes = 1:35,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

img_main_filtered_recipe <-
  recipe(Stroke ~ ., data = image_train)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_main_filtered_rfe <- rfe(
  img_main_filtered_recipe,
  data = image_train,
  sizes = 1:19,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

both_int_filtered_recipe <-
  recipe(Stroke ~ ., data = stroke_train) %>%
  step_interact(int_form)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_int_filtered_rfe <- rfe(
  both_int_filtered_recipe,
  data = stroke_train,
  sizes = 1:44,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

both_main_filtered_recipe <-
  recipe(Stroke ~ ., data = stroke_train)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_main_filtered_rfe <- rfe(
  both_main_filtered_recipe,
  data = stroke_train,
  sizes = 1:28,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

risk_int_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_interact(~ all_predictors():all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_int_rfe <- rfe(
  risk_int_recipe,
  data = risk_train,
  sizes = 1:36,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

risk_main_recipe <-
  recipe(Stroke ~ ., data = risk_train) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
risk_main_rfe <- rfe(
  risk_main_recipe,
  data = risk_train,
  sizes = 1:8,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

img_int_recipe <-
  recipe(Stroke ~ ., data = image_train) %>%
  step_interact(int_form)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_int_rfe <- rfe(
  img_int_recipe,
  data = image_train,
  sizes = 1:35,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

img_main_recipe <-
  recipe(Stroke ~ ., data = image_train)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
img_main_rfe <- rfe(
  img_main_recipe,
  data = image_train,
  sizes = 1:19,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

both_int_recipe <-
  recipe(Stroke ~ ., data = stroke_train) %>%
  step_interact(int_form)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_int_rfe <- rfe(
  both_int_recipe,
  data = stroke_train,
  sizes = 1:44,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

both_main_recipe <-
  recipe(Stroke ~ ., data = stroke_train)  %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

set.seed(63331)
both_main_rfe <- rfe(
  both_main_recipe,
  data = stroke_train,
  sizes = 1:28,
  rfeControl = rfeCtrl,
  metric = "ROC",
  ## train options
  method = "glm",
  trControl = internal_ctrl
)

# ------------------------------------------------------------------------------

format_data <- function(x, lab, int = FALSE) {
  dat <-
    x %>%
    pluck("results") %>%
    mutate(Predictors = !!lab) %>%
    dplyr::select(ROC, Variables, Predictors, Variables, Num_Resamples) %>%
    mutate(Model = "Main Effects")
  if (int)
    dat$Model <- "Interactions"
  dat

}

filtered_dat <-
  format_data(risk_main_filtered_rfe, lab = "Risk Predictors") %>%
  bind_rows(
    format_data(risk_int_filtered_rfe, lab = "Risk Predictors", TRUE),
    format_data(img_main_filtered_rfe, lab = "Imaging Predictors"),
    format_data(img_int_filtered_rfe, lab = "Imaging Predictors", TRUE),
    format_data(both_main_filtered_rfe, lab = "All Predictors"),
    format_data(both_int_filtered_rfe, lab = "All Predictors", TRUE)
  ) %>%
  mutate(
    Predictors = factor(
      Predictors,
      levels = c("Risk Predictors", "Imaging Predictors", "All Predictors")
    ),
    Model = factor(Model, levels = c("Main Effects", "Interactions")),
    Filtering = "Correlation Filter"
  )

unfiltered_dat <-
  format_data(risk_main_rfe, lab = "Risk Predictors") %>%
  bind_rows(
    format_data(risk_int_rfe, lab = "Risk Predictors", TRUE),
    format_data(img_main_rfe, lab = "Imaging Predictors"),
    format_data(img_int_rfe, lab = "Imaging Predictors", TRUE),
    format_data(both_main_rfe, lab = "All Predictors"),
    format_data(both_int_rfe, lab = "All Predictors", TRUE)
  ) %>%
  mutate(
    Predictors = factor(
      Predictors,
      levels = c("Risk Predictors", "Imaging Predictors", "All Predictors")
    ),
    Model = factor(Model, levels = c("Main Effects", "Interactions")),
    Filtering = "No Filter"
  )

rfe_data <-
  bind_rows(filtered_dat, unfiltered_dat) %>%
  mutate(
    Filtering = factor(Filtering, levels = c("No Filter", "Correlation Filter"))
  )

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/predictive-modeling-across-sets.html#fig:stroke-rfe-res
ggplot(rfe_data, aes(x = Variables, y = ROC, col = Model)) +
  geom_point(size = 0.75) +
  geom_line() +
  facet_grid(Filtering ~ Predictors) +
  scale_color_manual(values = c("#6A3D9A", "#CAB2D6"))

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/predictive-modeling-across-sets.html#tab:stroke-rfe-tab
rfe_tab <-
  img_main_filtered_rfe %>%
  pluck("variables") %>%
  filter(Variables == img_main_filtered_rfe$optsize) %>%
  group_by(var) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(final = ifelse(var %in% img_main_filtered_rfe$optVariables, "Yes", "No")) %>%
  ungroup()

# ------------------------------------------------------------------------------



