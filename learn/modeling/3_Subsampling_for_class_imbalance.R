# Title     : Subsampling for class imbalance
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/12
# URL       : https://www.tidymodels.org/learn/models/sub-sampling/



# ＜ポイント＞
# - ｢アンダーサンプリング｣｢オーバーサンプリング｣をレシピで実装する
# - 一般的にインバランスなデータはサンプルが多いほうにオーバーフィットする
# - この問題の軽減策として｢サブサンプリング｣が挙げられる
#   --- themis::step_rose()はサブサンプリングを行うレシピ
#   --- サブサンプリングは交差検証などリサンプリングを併せて行う必要がある



# ライブラリ
library(tidyverse)
library(tidymodels)
library(themis)     # インバランス分析に対応したライブラリ
library(discrim)    # parsnipの拡張（判別分析）



# データ準備
imbal_data <-
  read_csv("https://bit.ly/imbal_data") %>%
  mutate(Class = factor(Class))


# データ確認
imbal_data %>% print()
imbal_data %>% names()
imbal_data %>% glimpse()


# ラベル確認
# --- かなりインバラス
imbal_data$Class %>% table()




#%% ワークフロー設定 -----------------------------------------------------

# レシピ作成
# --- サブサンプリング
imbal_rec <-
  recipe(Class ~ ., data = imbal_data) %>%
  step_rose(Class)


# モデル構築
# --- 正則化判別分析
qda_mod <-
  discrim_regularized(frac_common_cov = 0, frac_identity = 0) %>%
  set_engine("klaR")


# ワークフロー設定
qda_rose_wflw <-
  workflow() %>%
    add_model(qda_mod) %>%
    add_recipe(imbal_rec)


# ワークフロー確認
qda_rose_wflw %>% print()



#%% リサンプリングデータの準備 --------------------------------------------

# 交差検証データの作成
# --- サブサンプリングは複数回行うことで意味を持つのでリサンプリングは必須
set.seed(5732)
cv_folds <- imbal_data %>% vfold_cv(strata = "Class", repeats = 5)
cv_folds %>% print()


#%% モデル学習（サンプリングあり） ------------------------------------------

# パフォーマンス指標の設定
cls_metrics <- metric_set(roc_auc, j_index)


# 学習(フィッティング)
# --- step_rose()でサンプリングを実施
set.seed(2180)
qda_rose_res <-
  qda_rose_wflw %>%
    fit_resamples(resamples = cv_folds,
                  metrics = cls_metrics)


# パフォーマンス指標の確認
# --- j_indexが大きい（サブサンプリングの効果）
qda_rose_res %>% collect_metrics()



#%% モデル学習（サンプリングなし） ------------------------------------------

# ワークフロー設定
# --- レシピ(step_rose)を使わないので、add_formula()でモデル定義
qda_wflw <-
  workflow() %>%
    add_model(qda_mod) %>%
    add_formula(Class ~ .)


set.seed(2180)
qda_only_res <-
  qda_wflw %>%
    fit_resamples(resamples = cv_folds,
                  metrics = cls_metrics)


# パフォーマンス指標の確認
# --- j_indexが小さい
qda_only_res %>% collect_metrics()


#%% パフォーマンス比較 -------------------------------

# データ集計
# --- サンプリングなし
no_sampling <-
  qda_only_res %>%
    collect_metrics(summarize = FALSE) %>%
    dplyr::select(-.estimator) %>%
    mutate(sampling = "no_sampling")


# データ集計
# --- サンプリングあり
with_sampling <-
    qda_rose_res %>%
    collect_metrics(summarize = FALSE) %>%
    dplyr::select(-.estimator) %>%
    mutate(sampling = "rose")


# プロット作成
# --- サブサンプリングがメトリックに影響を与えることが確認できる
no_sampling %>%
  bind_rows(with_sampling) %>%
  mutate(label = paste(id2, id)) %>%
  ggplot(aes(x = sampling, y = .estimate, group = label)) +
  geom_line(alpha = .4) +
  facet_wrap(~ .metric, scales = "free_y")



