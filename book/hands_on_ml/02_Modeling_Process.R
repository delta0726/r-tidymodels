# Title     : Chapter 2 Modeling Process
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/30
# URL       : https://bradleyboehmke.github.io/HOML/process.html
#           : https://koalaverse.github.io/homlr/notebooks/02-modeling-process.nb.html 


# ＜ポイント＞
# - EDAと同様に、MLプロセスは非常に反復的でヒューリスティックな処理
# - 1.学習データと検証データを分割して賢く費やす
# - 2.特徴量とターゲット変数を適切に前処理（データ漏洩を最小限に抑える）
# - 3.ハイパーパラメーターを調整
# - 4.モデルのパフォーマンスを評価



# ＜目次＞
# 2.1 データ準備
# 2.2 データ分割
# 2.3 Rによるモデル構築
# 2.4 リサンプリング方法
# 2.5 バイアスとバリアンスのトレードオフ
# 2.6 モデル評価
# 2.7 Putting the processes together




# Helper packages
library(dplyr)
library(ggplot2)


# Modeling process packages
library(tidymodels)
library(AmesHousing)
library(modeldata)
library(caret)
library(h2o)


# H2Oの開始
h2o.init()





# 2.1 データ準備 ---------------------------------------

# attrition ------------------------------

# データロード
data(attrition)

# データ準備
churn <- attrition %>% mutate_if(is.ordered, .funs = factor, ordered = FALSE)

# データ概要
churn %>% as_tibble()
churn %>% glimpse()

# H2Oフレームに変換
churn.h2o <-churn %>% as.h2o()


# ames ------------------------------------

# データ準備
ames <- make_ames()

# データ概要
ames %>% as_tibble()
ames %>% glimpse()

# H2Oフレームに変換
ames.h2o <- ames %>% as.h2o()



# 2.2 データ分割 ---------------------------------------

# ＜ポイント＞
# - モデルの汎化性能を確保＆評価するため、データを分割して使用するのが慣例となっている
# - 訓練データは50-80％程度を費やすのが一般的
# - データ分割は各種ライブラリで提供されるが結果はほぼ同じ
#   --- 現在ではtidymodelフレームワークの{rsample}が一般的







# 2.2.1 プロット作成 --------------------------------

# 方法1：ベースR
set.seed(123)
index_1 <- sample(1:nrow(ames), round(nrow(ames) * 0.7))
train_1 <- ames[index_1, ]
test_1  <- ames[-index_1, ]
train_1 %>% nrow()
test_1 %>% nrow()


# 方法2：{caret}を使用
set.seed(123)  # for reproducibility
index_2 <- createDataPartition(ames$Sale_Price, p = 0.7, list = FALSE)
train_2 <- ames[index_2, ]
test_2  <- ames[-index_2, ]
train_2 %>% nrow()
test_2 %>% nrow()

# 方法3：{rsample}を使用
set.seed(123)  # for reproducibility
split_1  <- ames %>% initial_split(prop = 0.7)
train_3  <- split_1 %>% training()
test_3   <- split_1 %>% testing()
train_3 %>% nrow()
test_3 %>% nrow()

# 方法4：{h2o}を使用
split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7, seed = 123)
train_4 <- split_2[[1]]
test_4  <- split_2[[2]]
train_3 %>% as.data.frame() %>% nrow()
test_3 %>% as.data.frame() %>% nrow()


# プロット作成--------------------------------

# 個別プロット
p1 <- 
  train_1 %>% 
    ggplot(aes(x = Sale_Price)) + 
    geom_density(trim = TRUE) + 
    geom_density(data = test_1, trim = TRUE, col = "red") +
    ggtitle("Base R")

p2 <- 
  train_2 %>% 
    ggplot(aes(x = Sale_Price)) + 
    geom_density(trim = TRUE) + 
    geom_density(data = test_2, trim = TRUE, col = "red") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle("caret") 

p3 <- 
  train_3 %>% 
    ggplot(aes(x = Sale_Price)) + 
    geom_density(trim = TRUE) + 
    geom_density(data = test_3, trim = TRUE, col = "red") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle("rsample")

p4 <-
  train_4 %>%
    as.data.frame() %>% ggplot(aes(x = Sale_Price)) +
    geom_density(trim = TRUE) +
    geom_density(data = as.data.frame(test_4), trim = TRUE, col = "red") +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    ggtitle("h2o")


# プロット結合
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 1)



# 2.2.2 層化抽出サンプリング --------------------------------

# ラベルごとの割合
# --- 元データ
churn$Attrition %>% table() %>% prop.table()


# データ分割
# --- 層別サンプリング
set.seed(123)
split_strat  <- initial_split(churn, prop = 0.7, strata = "Attrition")
train_strat  <- split_strat %>% training()
test_strat   <- split_strat %>% testing()


# ラベルごとの割合
# --- 分割後のデータ
# --- 元の割合が維持されている
train_strat$Attrition %>% table() %>% prop.table()
test_strat$Attrition %>% table() %>% prop.table()




# 2.2.3 データの不均衡 --------------------------------

# ＜課題＞
# - 分類問題において｢データ不均衡｣はモデル予測のパフォーマンスに大きな影響を与える
#   --- 数が多いほうにオーバーフィットしてしまう
#   --- ｢ダウンサンプリング｣｢アップサンプリング｣などの対処方法がある（どちらかに特に優位性があるわけではない）


# ＜ダウンサンプリング＞
# - 少ないほうのクラスに合わせて多いほうのクラスのサンプル数を減らすことでバランスを取る
# - この方法は、データの量が十分な場合に使用される


# ＜アップサンプリング＞
# - まれなサンプルのサイズを増やすことで、データセットのバランスを取る
# - ブートストラップ法などを用いて、稀なサンプルを生成する




# 2.3 Rによるモデル構築 ---------------------------------------

# 2.3.1 フォーミュラ・インターフェース

# Sale price as function of neighborhood and year sold
model_fn(Sale_Price ~ Neighborhood + Year_Sold,
         data = ames)

# Variables + interactions
model_fn(Sale_Price ~ Neighborhood + Year_Sold +
           Neighborhood:Year_Sold, data = ames)

# Shorthand for all predictors
model_fn(Sale_Price ~ ., data = ames)

# Inline functions / transformations
model_fn(log10(Sale_Price) ~ ns(Longitude, df = 3) +
           ns(Latitude, df = 3), data = ames)



model_fn(
  x = c("Year_Sold", "Longitude", "Latitude"),
  y = "Sale_Price",
  data = ames.h2o
)



# 2.3.2 様々なエンジン

# ＜ポイント＞
# - Rは計算アルゴリズムごとに多様なエンジン(ライブラリ)が用意されている
# - インターフェースはそれぞれ異なる
# - tidymodelの{parsnip}はインターフェースを統一するラッパー関数を提供する


# 線形回帰モデル
# --- 3つのエンジン
lm_lm    <- lm(Sale_Price ~ ., data = ames)
lm_glm   <- glm(Sale_Price ~ ., data = ames,family = gaussian)
lm_caret <- train(Sale_Price ~ ., data = ames, method = "lm")





# 2.4 リサンプリング方法 ---------------------------------------

# ＜ポイント＞
# - モデル汎化性能を評価するためリサンプリングによるクロスバリデーションが行われる
# - 訓練データを様々なパターンで｢Analysisデータ｣｢Assessmentデータ｣に分けてモデル性能の安定性を評価する
# - リサンプリングでは｢クロスバリデーション｣と｢ブートストラップ｣が一般的



# 2.4.1 k-fold cross validation ---------------

# ＜ポイント＞
# - 各レコードはいずれか1つのFoldに含まれる
# - 分割数は5-10程度が一般的（増やすと計算コストが高まる）



# クロスバリデーションによるリサンプリング
#   --- この段階ではデータの分割セットを作っているだけで具体的な計算はしていない
#   --- fit_resampling()でクロスバリデーション
#   --- tune()を用いてチューニング
cv <- mtcars %>% vfold_cv(10)
cv %>% print()


# プロット
# --- Foldごとの分割イメージ
cv_plot <-
  cv$splits %>%
    purrr::map2_dfr(seq_along(cv$splits), ~ mtcars %>% mutate(
      Resample = paste0("Fold_", stringr::str_pad(.y, 2, pad = 0)),
      ID = row_number(),
      Data = ifelse(ID %in% .x$in_id, "Training", "Validation"))
      ) %>%
    ggplot(aes(Resample, ID, fill = Data)) +
    geom_tile() +
    scale_fill_manual(values = c("#f2f2f2", "#AAAAAA")) +
    scale_y_reverse("Observation ID", breaks = 1:nrow(mtcars), expand = c(0, 0)) +
    scale_x_discrete(NULL, expand = c(0, 0)) +
    theme_classic() +
    ggtitle("10-fold cross validation") +
    theme(legend.title=element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))

print(cv_plot)


# H2Oによるクロスバリデーション
h2o.cv <-
  h2o.glm(x = x,
          y = y,
          training_frame = ames.h2o,
          nfolds = 10)




# 2.4.2 Bootstrap ---------------

# ＜ポイント＞
# - 各レコードは複数のFoldに含まれる
# - 元のデータセットと同じ数のサンプリングを行う（同じサンプルが含まれることもある）
# - データセットが少ない場合に効果的とされる
# - ブートストラップは、特定のMLアルゴリズムに自然に組み込まれるリサンプリングプロシージャ
#   --- バギングとランダムフォレストを説明する第10章-第11章でさらに明らかになる



# ブートストラップ・リサンプリング
#   --- この段階ではデータの分割セットを作っているだけで具体的な計算はしていない
#   --- fit_resampling()でクロスバリデーション
#   --- tune()を用いてチューニング
boots <- mtcars %>% bootstraps(10)
boots %>% print()


# プロット
# --- Foldごとの分割イメージ
# --- 重複して使われるサンプル(Replicates)があることに注意
boots_plot <-
  boots$splits %>%
    purrr::map2_dfr(seq_along(boots$splits), ~ mtcars %>%
               mutate(
                 Resample = paste0("Bootstrap_", stringr::str_pad(.y, 2, pad = 0)),
                 ID = row_number()
               ) %>%
               group_by(ID) %>%
               mutate(Replicates = factor(sum(ID == .x$in_id)))) %>%
    ggplot(aes(Resample, ID, fill = Replicates)) +
    geom_tile() +
    scale_fill_manual(values = c("#FFFFFF", "#F5F5F5", "#C8C8C8", "#A0A0A0", "#707070", "#505050", "#000000")) +
    scale_y_reverse("Observation ID", breaks = 1:nrow(mtcars), expand = c(0, 0)) +
    scale_x_discrete(NULL, expand = c(0, 0)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Bootstrap sampling")


print(boots_plot)


# プロット比較
# ---
cowplot::plot_grid(boots_plot, cv_plot, align = "h", nrow = 1)




# 2.5 バイアスとバリアンスのトレードオフ ----------------------------

# ＜ポイント＞
# - 予測エラーは｢バイアス｣と｢バリアンス(分散)｣の2つの重要なサブコンポーネントに分解できる
# - 多くの場合｢バイアス｣と｢バリアンス｣はトレードオフの関係にある
# - エラー原因が｢バイアス｣と｢バリアンス｣にどのようにつながっているかを理解する
#   --- プロセス改善の精度を高めることにつながる



# ＜バイストとバリアンスの分解＞
# 平均2乗誤差 ＝ バイアス2 ＋ バリアンス ＋ ノイズ(消去不能)


# ＜バイアスとバリアンスのトレードオフ＞
# - ｢バイアスの減らし方｣｢バリアンスの減らし方｣をそれぞれ分けて理解する
#   --- バイアス  :
#   --- バリアンス：モデル表現力を下げる / 正則化


# - ＜参考資料＞
# - https://daviddalpiaz.github.io/r4sl/biasvariance-tradeoff.html
# - https://punhundon-lifeshift.com/bias_variance




# バイアス -----------------------------------------

# ＜バイアス＞
# - モデルの予測値と実現値の差のことを指している（正解に近いほどよい）
#   --- 予測モデルが単純すぎることが原因で発生する
#   --- モデルがデータの基本構造にどれだけよく適合できるかという感覚を提供
#   --- RMSEで評価する


# ＜事例＞
# - 線形回帰モデルのような単純なモデルでバイアスが大きくなる
# - ディープラーニングのような複雑なモデルではバイアスが小さくなる


# データ作成
# --- 線形回帰用データ
set.seed(123)
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>% filter(x < 4.5)
df %>% as_tibble()


# 線形回帰モデル
# --- 1回
bias_model <- lm(y ~ I(x^3), data = df)
df$predictions <- predict(bias_model, df)


# 線形回帰モデル
# --- ブートストラップ法で25回
# --- サンプルを変えても結果が安定することを確認
bootstrap_n <- 25
bootstrap_results <- NULL
for(i in seq_len(bootstrap_n)) {
  set.seed(i)  # for reproducibility
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]
  fit <- lm(y ~ I(x^3), data = df_sim)
  df_sim$predictions <- predict(fit, df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}


# プロット作成
p1 <-
  df %>%
    ggplot(aes(x, y)) +
    geom_point(alpha = .3) +
    geom_line(aes(x, predictions), size = 1.5, color = "dodgerblue") +
    scale_y_continuous("Response", limits = c(-1.75, 1.75), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
    ggtitle("Single biased model fit")

p2 <-
  bootstrap_results %>%
    ggplot(aes(x, predictions, color = model)) +
    geom_line(show.legend = FALSE, size = .5) +
    scale_y_continuous("Response", limits = c(-1.75, 1.75), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
    ggtitle("25 biased models fit to bootstrap samples")


# プロット比較
# --- 左図：線形回帰は単純なモデルなのでバイアスが大きくなる
# --- 右図：リサンプリングしても結果が安定している（安定してバイアスが大きい）
gridExtra::grid.arrange(p1, p2, nrow = 1)




# バリアンス(分散) -----------------------------------------

# ＜バリアンス＞
# - データが変わることによって発生する誤差を指す（予測値のばらつき）
#   --- 同じようなインプットなのに結果が大きく異なる
#   --- 予測モデルが複雑すぎることが原因（オーバーフィッティング）


# ＜事例＞
# - 線形回帰モデルのような単純なモデルでバイアスが大きくなる
# - ディープラーニングのような複雑なモデルではバイアスが小さくなる


# Simulate some nonlinear monotonic data
set.seed(123)  # for reproducibility
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>%
  filter(x < 4.5)

# Single model fit
variance_model <- knnreg(y ~ x, k = 3, data = df)
df$predictions <- predict(variance_model, df)
p1 <- ggplot(df, aes(x, y)) +
  geom_point(alpha = .3) +
  geom_line(aes(x, predictions), size = 1.5, color = "dodgerblue") +
  scale_y_continuous("Response", limits = c(-1.75, 1.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
  ggtitle("Single high variance model fit")

# Bootstrapped model fit
bootstrap_n <- 25
bootstrap_results <- NULL
for(i in seq_len(bootstrap_n)) {
  set.seed(i)  # for reproducibility
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]
  fit <- knnreg(y ~ x, k = 3, data = df_sim)
  df_sim$predictions <- predict(fit, df_sim)
  df_sim$model <- paste0("model", i)
  df_sim$ob <- index
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}

p2 <- ggplot(bootstrap_results, aes(x, predictions, color = model)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous("Response", limits = c(-1.75, 1.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
  ggtitle("25 high variance models fit to bootstrap samples")

gridExtra::grid.arrange(p1, p2, nrow = 1)





# 2.6 モデル評価 ----------------------------

# ＜ポイント＞
# - 統計学の世界ではモデルのパフォーマンスを｢適合度テスト｣と｢残差の評価｣に基づいていた
#   --- 誤解を招く結論が導き出されることもあった

# - 機械学習では損失関数を介して予測精度を評価することであることが広く受け入れられている
#   --- 損失関数とは、｢予測値｣と｢実際値｣を比較するメトリック
#   --- サンプリング手法を実行する場合、検証セットの予測値を実際のターゲット値と比較して評価

# - 損失関数のメトリックには、特定の偏差を強調するものなど多様な指標が用意されている
#   --- 目的に合わせて指標を選ぶことが肝要



# ＜回帰モデル＞

# MSE
# - RMSEに平方根を適用しない指標（2乗のまま使用）
# - 誤差が大きいほど強調される（誤差に弱い）
# - MSEの値自体は解釈できない数値
# - 小さいほど良好


# RMSE
# - MSEに平方根を適用した指標
# - 誤差が大きいほど強調される（誤差に弱い）
# - 予測値と同じ尺度で解釈できる数値
# - 小さいほど良好


# MAE
# - 予測値と実際値の偏差の絶対値を平均したもの
# - 異常値に対して頑健（MSEは2乗するので誤差が強調される）
# - MSEの値自体は解釈できない数値
# - 小さいほど良好


# RMSLE
# - RMSEを計算する際に予測値を対数変換した指標
# - 誤差が大きいほど強調されるRMSEの課題を解消
# - 小さいほど良好


# RSQ
# - 独立変数から予測可能な従属変数の分散の割合を表す一般的なメトリック
# - このメトリックに過度の重点を置くべきではありません。
# - 大きいほど良好



# ＜分類モデル＞




# 2.7 Putting the processes together ----------------------------

# ＜ポイント＞
# - ハイパーパラメータのチューニングプロセスを確認
# - {caret}は全てのプロセスが1つの関数にまとめて実行するので内部処理が分かりにくい
# - チューニング結果の解釈とRMSEの読み方を確認



# データ分割
set.seed(123)
split <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# リサンプリングの定義
# --- 10回のクロスバリデーションを5セット実行
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)


# チューニング・グリッド
# --- k=1-25でチューニング
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))
hyper_grid %>% print()


# チューニング
# --- {caret}は1つの関数で多くのプロセスを実施する（学習・クロスバリデーション・チューニング）
# --- k近傍法で学習
# --- RMSEを損失関数としたクロスバリデーションでチューニングも併せて実施
knn_fit <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric = "RMSE"
)


# 確認
knn_fit %>% print()


# プロット
# --- RMSEは42000-43000程度
# --- モデルが住宅の予想販売価格を43,439ドル誤って予測することを意味している
knn_fit %>% ggplot()


