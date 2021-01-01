# Title     : Evaluating Submodels with the Same Model Object
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/12
# URL       : https://parsnip.tidymodels.org/articles/articles/Submodels.html



library(tidyverse)
library(tidymodels)



# サブモデル可能なオブジェクト一覧
# --- {utils}のmethods
methods("multi_predict")


# データロード
data(attrition, package = "modeldata")


# データ確認
attrition %>% as_tibble()
attrition %>% names()
attrition %>% glimpse()



#%% データ準備 ---------------------------------------------------------

# データ分割
set.seed(4595)
data_split <- attrition %>% initial_split(strata = "Attrition")
attrition_train <- data_split %>% training()
attrition_test  <- data_split %>% testing()


# 確認
attrition_train %>% dim()
attrition_test  %>% dim()


# バリデーションデータの作成
set.seed(616)
folds <- attrition_train %>% vfold_cv()
folds %>% print()


# 学習用データ
model_data <- folds$splits[[1]] %>% analysis()
pred_data  <- folds$splits[[1]] %>% assessment()


# 確認
# --- 評価データは111レコード
model_data %>% as_tibble()
pred_data %>% as_tibble()



#%% モデリング ---------------------------------------------------------

# モデル構築（初期モデル）
# --- 勾配ブースティング
attrition_boost <-
  boost_tree(mode = "classification", trees = 100) %>%
  set_engine("C5.0")




#%% 学習 ---------------------------------------------------------


# メインモデルの学習
# --- モデル：初期モデル
# --- データ：訓練データ
fold_1_model <-
  attrition_boost %>%
  fit_xy(x = model_data %>% dplyr::select(-Attrition), y = model_data$Attrition)


# 確認
fold_1_model %>% print()
fold_1_model %>% summary()


# サブモデルの学習
# --- モデル：学習済メインモデル
# --- データ：評価データ（学習ではないのでYを入れる必要はない）
fold_1_pred <-
  fold_1_model %>%
    multi_predict(new_data = pred_data %>% dplyr::select(-Attrition),
                  trees = 1:100,
                  type = "prob")


# 確認
# --- 評価データ111個についてtreeごとの予測結果が入っている
fold_1_pred %>% print()
fold_1_pred$.pred[[1]]




#%% 評価 ---------------------------------------------------------

# プロット用データ作成
# --- 予測データと正解データの結合
fold_1_df <-
  fold_1_pred %>%
    bind_cols(pred_data %>% dplyr::select(Attrition)) %>%
    add_rowindex() %>%
    tidyr::unnest(.pred)


# 確認
fold_1_df %>% print()



# プロット作成
fold_1_df %>%
  dplyr::filter(.row %in% c(1, 88)) %>%
  ggplot(aes(x = trees, y = .pred_No, col = Attrition, group = .row)) +
  geom_step() +
  ylim(0:1) +
  theme(legend.position = "top")


# プロット作成
fold_1_df %>%
  group_by(trees) %>%
  roc_auc(truth = Attrition, .pred_No) %>%
  ggplot(aes(x = trees, y = .estimate)) +
  geom_step()