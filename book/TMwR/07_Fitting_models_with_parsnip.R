#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 7 Fitting models with parsnip
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/13
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜ポイント＞
# - {parsnip}では主要なモデルの同一インターフェースで扱えるようになっている
#   --- モデルメンテナンス性の観点から{discrim}など別パッケージで提供されるモデルもある
#   --- アルゴリズム自体はベースパッケージを利用して、{parsnip}はインターフェースを提供しているだけ


# ＜目次＞
# 0 準備
# 1 共通インターフェース
# 2 線形回帰モデルの実行
# 3 フォーミュラでモデル定義するメリット
# 4 統一化されたハイパーパラメータ
# 5 モデル結果の抽出


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)

# データロード
data(ames)

# データ加工
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# データ分割
set.seed(123)
ames_split <- ames %>% initial_split(prob = 0.80, strata = Sale_Price)
ames_train <- ames_split %>% training()
ames_test  <-  ames_split %>% testing()

# レシピ作成
ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Latitude, Longitude, deg_free = 20)


# 1 共通インターフェース ----------------------------------------------------------------------

# ＜ポイント＞
# - {parsnip}では線形回帰モデルは3つのエンジンで実装可能


# ベースパッケージのlm
linear_reg() %>% set_engine("lm")
linear_reg() %>% set_engine("lm") %>% translate()

# 正則化回帰を扱うglmnet
# --- family = "gaussian"がデフォルト設定された
# --- オプションの変更は可能
linear_reg() %>% set_engine("glmnet")
linear_reg() %>% set_engine("glmnet") %>% translate()

# MCMCを扱うstan
linear_reg() %>% set_engine("stan")
linear_reg() %>% set_engine("stan") %>% translate()


# 2 線形回帰モデルの実行 ----------------------------------------------------------------------

# データ確認
ames_train %>% select(Sale_Price, Longitude, Latitude)
ames_train$Longitude %>% hist()
ames_train$Latitude %>% hist()

# モデル構築
# --- 線形回帰
lm_model <-
  linear_reg() %>%
  set_engine("lm")

# 学習
# --- fit()にフォーミュラを渡して学習
# --- フォーミュラは自動的にダミー変数を生成する
lm_form_fit <-
  lm_model %>%
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

# 学習
# --- fit_xy()にデータを渡して学習
lm_xy_fit <-
  lm_model %>%
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
    )

# 結果確認
lm_form_fit %>% tidy()
lm_xy_fit %>% tidy()


# 3 フォーミュラでモデル定義するメリット -------------------------------------------------------

# ＜ポイント＞
# - 2で確認したfit()はフォーミュラ形式でモデルを定義する
# - フォーミュラ形式で定義すると、カテゴリカル変数をダミー変数に自動変換してくれる
#   --- 線形回帰モデルなど数値データ(ダミー変換)が要求されるモデルのみ
#   --- ツリー系モデルのようにダミー変換を要しないモデルはそのまま


# データ確認
ames_train %>%
  select(Sale_Price, Street, Alley) %>%
  glimpse()


# 線形回帰 *************************************

# モデリング
# --- 線形回帰モデル
# --- 説明変数はカテゴリカルデータのまま
lm_fit_fct <-
  linear_reg() %>%
    set_engine("lm") %>%
    fit(Sale_Price ~ Street + Alley, data = ames_train)

# 結果確認
# --- ダミー変換されている
lm_fit_fct %>% tidy()


# ランダムフォレスト *****************************

# モデリング
# --- ランダムフォレスト
# --- 説明変数はカテゴリカルデータのまま
lm_fit_fct <-
  rand_forest("regression") %>%
    set_engine("ranger", importance = "impurity") %>%
    fit(Sale_Price ~ Street + Alley, data = ames_train)

# 結果確認
# --- カテゴリカル変数のまま扱われる
lm_fit_fct$fit$variable.importance


# 4 統一化されたハイパーパラメータ -------------------------------------------------------

# ＜ポイント＞
# - {parsnip}のAPIで指定したパラメータは個々のモデルの対応引数に割り当てられる


# モデル定義
# --- ranger
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  translate()

# モデル定義
# --- randomForest
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("randomForest") %>%
  set_mode("regression") %>%
  translate()


# 5 モデル結果の抽出 -------------------------------------------------------

# ＜ポイント＞
# - モデル結果へのアクセス方法は多数あるが、ここではpurrr::pluch()を用いている


# オブジェクト構造
lm_form_fit %>% names()

# 結果の抽出
# --- purrr::pluch()を使用
lm_form_fit %>% pluck("fit")

# 抽出結果を用いて計算
# --- 分散共分散行列
lm_form_fit %>% pluck("fit") %>% vcov()

# サマリー
model_res <- lm_form_fit %>% pluck("fit") %>% summary()

# 回帰係数
model_res %>% coef()
lm_form_fit %>% tidy()

# 予測
ames_test_small <- ames_test %>% slice(1:5)
lm_form_fit %>% predict(new_data = ames_test_small)

# 信頼区間
# --- 予測結果を元データに結合
ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(lm_form_fit, ames_test_small)) %>%
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))

