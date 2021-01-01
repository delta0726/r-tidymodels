# Title     : Regression models two ways
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/12
# URL       : https://www.tidymodels.org/learn/models/parsnip-ranger-glmnet/



# ＜ポイント＞
# - {parsnip}の基本的な使い方を確認する
# - .pred()などのCharacteristic Variableの使い方を確認する
# - モデル比較の手順を確認する



library(tidyverse)
library(tidymodels)
library(Hmisc)
library(AmesHousing)



#%% データ準備 ------------------------------------------------------------

# データロード
ames <- make_ames()


# データ確認
ames %>% print()
ames %>% names()
ames %>% glimpse()


# データ分割
set.seed(4595)
data_split <- ames %>% initial_split(strata = "Sale_Price", p = 0.75, breaks = 4)
ames_train <- data_split %>% training()
ames_test  <- data_split %>% testing()



#%% 基本的なモデル構築 ------------------------------------------------------------


# モデル構築
# --- ランダムフォレスト回帰
rf_defaults <- rand_forest(mode = "regression")
rf_defaults


# 説明変数
preds <- c("Longitude", "Latitude", "Lot_Area", "Neighborhood", "Year_Sold")


# モデル構築
# --- 変数方式でフォーミュラを定義
rf_xy_fit <-
  rf_defaults %>%
  set_engine("ranger") %>%
  fit_xy(
    x = ames_train[, preds],
    y = log10(ames_train$Sale_Price)
  )


# 確認
rf_xy_fit %>% print()


# 正解値と推定値の比較
test_results <-
  ames_test %>%
    select(Sale_Price) %>%
    mutate(Sale_Price = log10(Sale_Price)) %>%
    bind_cols(predict(rf_xy_fit, new_data = ames_test[, preds]))


# 確認
test_results %>% slice(1:5)


# パフォーマンス指標
test_results %>%
  metrics(truth = Sale_Price, estimate = .pred)





#%% 計算エンジンの変更 ------------------------------------------------------------

# {ranger}
rf_ranger <-
  rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
    set_engine("ranger") %>%
    fit(
      log10(Sale_Price) ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
      data = ames_train
    )


# {randomForest}
rf_randomForest <-
  rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
    set_engine("randomForest") %>%
    fit(
      log10(Sale_Price) ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
      data = ames_train
    )


# 出力結果
# --- ライブラリごとに表示も結果も異なる
rf_ranger %>% print()
rf_randomForest %>% print()


# オブジェクト構造
# --- 第１階層は同じ構造になっている
rf_ranger %>% names()
rf_randomForest %>% names()


# オブジェクト構造
# --- 第２階層以降はそれぞれのライブラリの構造
rf_ranger %>% list.tree(2)
rf_randomForest %>% list.tree(2)




#%% 属性変数の利用 ------------------------------------------------------------

# データの属性情報はショートカットで取得できる
# --- .preds()： 説明変数の数
rand_forest(mode = "regression", mtry = .preds(), trees = 1000) %>%
  set_engine("ranger") %>%
  fit(
    log10(Sale_Price) ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
    data = ames_train
  )




#%% 正則化回帰 ------------------------------------------------------

# レシピ作成
norm_recipe <-
  recipe(
    Sale_Price ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
    data = ames_train
  ) %>%
    step_other(Neighborhood) %>%
    step_dummy(all_nominal()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_log(Sale_Price, base = 10) %>%
    prep(training = ames_train, retain = TRUE)



# モデル構築＆学習
glmn_fit <-
  linear_reg(penalty = 0.001, mixture = 0.5) %>%
    set_engine("glmnet") %>%
    fit(Sale_Price ~ ., data = juice(norm_recipe))


# 確認
glmn_fit %>% print()


# レシピ適用
# --- テストデータ
test_normalized <-
  norm_recipe %>%
    bake(new_data = ames_test, all_predictors())


# ランダムフォレストでの結果
test_results %>% print()


# 結果比較
# --- ランダムフォレストの結果に正則化回帰の結果を追加
results <-
  test_results %>%
  rename(`random forest` = .pred) %>%
  bind_cols(
    predict(glmn_fit, new_data = test_normalized) %>%
      rename(glmnet = .pred)
  )

# 確認
results %>% print()


# パフォーマンス指標
results %>%
  metrics(truth = Sale_Price, estimate = glmnet)


# プロット作成
# --- パフォーマンス指標
results %>%
  gather(model, prediction, -Sale_Price) %>%
  ggplot(aes(x = prediction, y = Sale_Price)) +
  geom_abline(col = "green", lty = 2) +
  geom_point(alpha = .4) +
  facet_wrap(~model) +
  coord_fixed()

