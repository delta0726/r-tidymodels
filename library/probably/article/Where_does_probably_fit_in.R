# Title     : Where does probably fit in?
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/31
# URL       : https://probably.tidymodels.org/articles/where-to-use.html



# ＜ポイント＞
# - 分類問題はクラス確率予測に基づいて分類の判断を行う
#   --- 通常のバイナリ分類では閾値を0.5と指定される
# - クラス不均衡な問題ではクラス確率が0.5を中心に分布しないこともある
#   --- {probably}は閾値を変更することで分類結果を調整する



# 1.準備 ---------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(probably)
library(yardstick)
library(modeldata)


# データ準備
# --- ローンデータ
data("lending_club")


# データ確認
lending_club %>% print()
lending_club %>% glimpse()



# 2.データ再定義 ------------------------------------------------------------

# レベルの確認
# --- bad / good
# --- アルファベット順
lending_club$Class %>% levels()
lending_club$Class %>% table()


# レベルの変更
lending_club <-
  lending_club %>%
    mutate(Class = relevel(Class, "good"))


# レベルの確認
# --- good / bad
lending_club$Class %>% levels()
lending_club$Class %>% table()


# データ再定義
lending_club <- lending_club %>% select(Class, annual_inc, verification_status, sub_grade)
lending_club %>% print()



# 3.データ分割 ---------------------------------------------------------------------

# ＜ポイント＞
# - ｢Good｣と｢Bad｣のラベル数には大きな不均衡が発生している
#   --- ダウンサンプリングがソリューションの1つ
#   --- ここでは特に調整せずに継続する



# データ分割
set.seed(123)
split <- lending_club %>% initial_split(prop = 0.75)
lending_train <- split %>% training()
lending_test  <- split %>% testing()


# 確認
lending_train %>% count(Class)
lending_test %>% count(Class)



# 4.モデリング ---------------------------------------------------------------------

# ＜ポイント＞
# - GLMのロジスティック回帰
#


# モデル定義
logi_reg <- logistic_reg()
logi_reg_glm <- logi_reg %>% set_engine("glm")
logi_reg_glm


# 学習
logi_reg_fit <-
  logi_reg_glm %>%
    fit(formula = Class ~ annual_inc + verification_status + sub_grade,
        data = lending_train)


# 確認
logi_reg_fit



# 5.予測 ---------------------------------------------------------------------


# クラス確率を取得
predictions <-
  logi_reg_fit %>%
    predict(new_data = lending_test, type = "prob")


# 確認
predictions %>% head(n = 2)


# 元データ結合
# --- 予測値と元データ
lending_test_pred <- predictions %>% bind_cols(lending_test)
lending_test_pred %>% print()




# 6.ラベルの割り当て ---------------------------------------------------------------------

# ＜ポイント＞
# - 通常は2クラス分類問題では0.5を境にクラスを分類する
# - 今回は不均衡データなので、全てが0.5を超える値を返してくる


# プロット作成
# --- クラス確率の分布
# --- 全てが0.5を超えている
lending_test_pred %>% ggplot(aes(x = .pred_good)) + geom_histogram()



# **** 閾値=0.5 *****************************************************

# ＜ポイント＞
# - 全てを｢Good｣と予測してしまう


# クラス確率から結果を予測
# --- probably::make_two_class_pred()
# --- 閾値を指定することができる
hard_pred_0.5 <-
  lending_test_pred %>%
    mutate(.pred = make_two_class_pred(.pred_good, levels(Class), threshold = .5)) %>%
    select(Class, contains(".pred"))


# 確認
# --- Predictionは全て｢Good｣を出している
hard_pred_0.5 %>% count(.truth = Class, .pred)



# **** 閾値=0.75 *****************************************************

# ＜ポイント＞
# - 3例のみ｢Bad｣と予測できた


# クラス確率から結果を予測
# --- probably::make_two_class_pred()
# --- 閾値を0.75に指定する
hard_pred_0.75 <-
  lending_test_pred %>%
    mutate(.pred = make_two_class_pred(.pred_good, levels(Class), threshold = .75)) %>%
    select(Class, contains(".pred"))



# 確認
# --- Predictionは全て｢Good｣を出している
hard_pred_0.75 %>% count(.truth = Class, .pred)




# 7.メトリック ---------------------------------------------------------------------


# ラベルをファクターに変換
hard_pred_0.5  <- hard_pred_0.5 %>% mutate(.pred = as.factor(.pred))
hard_pred_0.75 <- hard_pred_0.75 %>% mutate(.pred = as.factor(.pred))


# Sensitivity
# --- 全てのGoodのうち、Goodと予測できた割合(0-1)
hard_pred_0.5 %>% sens(Class, .pred)
hard_pred_0.75 %>% sens(Class, .pred)


# Specify
# --- 全てのBadのうち、Badと予測できた割合(0-1)
# --- ローン問題ではこちらに興味がある
hard_pred_0.5 %>% spec(Class, .pred)
hard_pred_0.75 %>% spec(Class, .pred)


# J-Index
# --- Sensitivity + Specify - 1
# --- -1 to +1
hard_pred_0.5 %>% j_index(Class, .pred)
hard_pred_0.75 %>% j_index(Class, .pred)



# 8.閾値ごとのメトリック --------------------------------------------------------

# 適切な閾値を探る
# --- probably::threshold_perf()
# --- thresholdsはベクトルで渡すことができる
threshold_data <-
  lending_test_pred %>%
    threshold_perf(Class, .pred_good, thresholds = seq(0.5, 1, by = 0.0025))

threshold_data %>%
  filter(.threshold %in% c(0.5, 0.6, 0.7))




# 9.プロット -------------------------------------------------------


# プロットデータの作成
threshold_data <-
  threshold_data %>%
    filter(.metric != "distance") %>%
    mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                             TRUE ~ "2"
    ))


# 確認
threshold_data


# J-Indexの最大値
max_j_index_threshold <-
  threshold_data %>%
    filter(.metric == "j_index") %>%
    filter(.estimate == max(.estimate)) %>%
    pull(.threshold)


# プロット
threshold_data %>%
  ggplot(aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
    geom_line() +
    theme_minimal() +
    scale_color_viridis_d(end = 0.9) +
    scale_alpha_manual(values = c(.4, 1), guide = "none") +
    geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
    labs(
      x = "'Good' Threshold\n(above this value is considered 'good')",
      y = "Metric Estimate",
      title = "Balancing performance by varying the threshold",
      subtitle = "Sensitivity or specificity alone might not be enough!\nVertical line = Max J-Index"
    )



threshold_data %>%
  filter(.threshold == max_j_index_threshold)




