# Title     : Chapter 3 Feature & Target Engineering
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/30
# URL       : https://bradleyboehmke.github.io/HOML/engineering.html
#           : https://koalaverse.github.io/homlr/notebooks/03-feature-engineering.nb.html
#           : http://www.feat.engineering/




# ＜注意事項＞
# - この章では代表的な特徴量エンジニアリングの手法のみが紹介されている
# - より詳細な手法は｢Feature Engineering and Selection｣(Max Kuhn)を参照



# ＜目次＞
# 3.1 準備
# 3.2 ラベルデータのエンジニアリング
# 3.3 欠損値の扱い
# 3.4 特徴量フィルタリング
# 3.5 数値の特徴量エンジニアリング
# 3.6 カテゴリの特徴量エンジニアリング
# 3.7 次元削減
# 3.8 適切なデータ処理




# 3.1 準備 ---------------------------------------

# ＜ポイント＞
# - Amesデータセットの｢Sales Price｣のラベルは歪んでいる


# ライブラリ
library(magrittr)
library(tidyverse)
library(tidymodels)
library(caret)
library(stringr)
library(kableExtra)
library(visdat)


# Set the graphical theme
ggplot2::theme_set(ggplot2::theme_light())


# Load and split the Ames housing data using stratified sampling
set.seed(123)  # for reproducibility


# データロード
ames <- AmesHousing::make_ames()


# データ概要
ames %>% as_tibble()
ames %>% glimpse()


# データ分割
split      <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train <- split %>% training()
ames_test  <- split %>% testing()



# プロット作成
# --- ラベルデータ：Sale_Price
ames_train %>%
  ggplot(aes(x = Sale_Price)) +
    geom_histogram(bins = 75) +
    ylab(NULL) +
    xlab("Sale_Price")



# 3.2 ラベルデータのエンジニアリング ---------------------------------------

# ＜ポイント＞
# - ラベルデータを特徴量エンジニアリングで変換するとモデル精度の改善につながることがある
# - 線形回帰モデルの場合、｢残差の正規性の仮定｣を担保するためラベルデータを正規化する
# - 正規化の方法として対数変換が有効
#   --- 元のラベルをRMSLEで測定することに近い


# 3.2.1 対数変換 ---------

# ＜ポイント＞
# - 対数変換で概ね正規的に変換することができる


# 線形回帰モデルの定義
# --- モデル1：ラベルは元データのまま
# --- モデル2：ラベルは元データを対数変換
model_list <-
  list(m1 = lm(Sale_Price ~ Year_Built, data = ames_train), 
       m2 = lm(log(Sale_Price) ~ Year_Built, data = ames_train))


# 確認
model_list %>% print()


# モデルラベルの定義
models <- c("m1: Non-log transformed model residuals",
            "m2: Log transformed model residuals")


# プロット用データ
X_Plot <-
  model_list %>%
    map2_dfr(models, ~ broom::augment(.x) %>% mutate(model = .y))


# プロット作成
# --- 残差のヒストグラム
# --- purrrとbroomで一括処理
# --- 対数変換することで残差の正規性が確保された
X_Plot %>%
  ggplot(aes(.resid)) +
    geom_histogram(bins = 75) +
    facet_wrap(~ model, scales = "free_x") +
    ylab(NULL) +
    xlab("Residuals")




# 3.2.2 {recipe}による対数変換 --------------------------------

# ＜ポイント＞
# - 対数変換はstep_log()で実行することができる


# レシピの定義
ames_recipe <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_log(all_outcomes())


# レシピ概要
ames_recipe %>% print()
ames_recipe %>% summary()
ames_recipe %>% tidy()


# 確認
# --- レシピのstep_log()とlog()の結果を比較
ames_recipe %>%
  prep() %>%
  juice() %>%
  select(Sale_Price) %>%
  rename(Sale_Price_Recipe = Sale_Price) %>%
  mutate(Sale_Price_Log = log(ames_train$Sale_Price))



# 3.2.3 対数変換の問題点 --------------------------------

# ＜ポイント＞
# - 対数は｢負の値｣や｢ゼロ｣に対して定義することができない
#   --- 負の値の場合は｢NaN｣、ゼロの場合は｢-Infs｣となる
#   --- 負の値の範囲が小さい場合、かつ範囲が分かっている場合は1以上になるようにオフセットする
#   --- Yao-Johnson変換を行う


# 対処法1：マイナス値の対数変換
# --- 1以上の値にシフトして対数変換
# --- log1p()は元の値を+1して対数変換（-1よりも値が小さければNaNとなる）
log(-0.5)
log1p(-0.5)
log(-1.5)
log1p(-1.5)


# 対処法2： yao-Johnson変換
# --- log1p()は元の値を+1して対数変換（-1よりも値が小さければNaNとなる）
VGAM::yeo.johnson(-0.5, lambda = 0.5)
VGAM::yeo.johnson(-1.5, lambda = 0.5)
(ames_train$Sale_Price * +1)  %>% VGAM::yeo.johnson(lambda = 0.5) %>% hist()

x <- seq(-100, 100, by = 0.1)
y <- VGAM::yeo.johnson(x, lambda = 0.5)
tibble(x = x, y = y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()


# Yao-Johnson変換
ames_recipe_yj <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_YeoJohnson(all_outcomes())


ames_recipe_yj %>%
  prep() %>%
  juice() %>%
  select(Sale_Price) %>%
  rename(Sale_Price_YaoJohnson = Sale_Price) %>%
  mutate(num = row_number(),
         Sale_Price_Log = log(ames_train$Sale_Price),
         Sale_Price = ames_train$Sale_Price) %>%
  pivot_longer(cols = -num, names_to = "Label", values_to = "value") %>%
  ggplot(aes(x = value, fill = Label)) +
    geom_histogram(bins = 75) +
    facet_wrap(~ Label, scales = "free_x") +
    ylab(NULL)



# 3.2.4 Box-Cox変換 ----------------------------

# - Box-Cox変換を使うと通常データと対数変換の間で変換レベルをコントロールできる
#   --- 正規分布に限りなく近いラムダを見つけ出すことも可能
#   --- 入力値が正の値である必要があるというのは同じ


# 対数変換
train_log_y <- ames_train$Sale_Price %>% log()
test_log_y  <- ames_train$Sale_Price %>% log()


# Box-Cox変換の準備
# --- 最適なラムダを推定
lambda  <- forecast::BoxCox.lambda(ames_train$Sale_Price)


# Box-Cox変換
train_bc_y <- forecast::BoxCox(ames_train$Sale_Price, lambda)
test_bc_y  <- forecast::BoxCox(ames_test$Sale_Price, lambda)


# データ作成
df_comp <-
  data.frame(
    Normal = ames_train$Sale_Price,
    Log_Transform = train_log_y,
    BoxCox_Transform = train_bc_y
  )


# プロット作成
df_comp %>%
  gather(Transform, Value) %>%
  mutate(Transform = factor(Transform, levels = c("Normal", "Log_Transform", "BoxCox_Transform"))) %>%
  ggplot(aes(Value, fill = Transform)) +
    geom_histogram(show.legend = FALSE, bins = 40) +
    facet_wrap(~ Transform, scales = "free_x")




# 3.3 欠損値の扱い ---------------------------------------


# ＜アルゴリズムと欠損値＞
# - ほとんどのアルゴリズムは欠落を処理できないため事前に欠損値処理をする必要がある
#   --- 一般化線形モデル、ニューラルネットワーク、サポートベクターマシーン

# - モデルによっては欠損値処理の仕組みが備わっている
#    ---主にツリーベースモデル



# データ準備
# --- 欠損値ありのamesデータ
ames_raw <-
  AmesHousing::ames_raw %>%
    set_names(str_replace_all(names(.), " ", "_")) %>%
    rename(Sale_Price = SalePrice)




# 3.3.1 欠損値の可視化 -----------------


# データフレームの欠損値カウント
ames_raw %>% is.na() %>% sum()


# 欠損値の可視化
ames_raw %>%
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var2, Var1, fill=value)) +
    geom_raster() +
    coord_flip() +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    scale_fill_grey(name = "",
                    labels = c("Present",
                               "Missing")) +
    xlab("Observation") +
    theme(axis.text.y  = element_text(size = 4))


# 欠損データの確認
ames_raw %>%
  filter(is.na(Garage_Type)) %>%
  select(Garage_Type, Garage_Cars, Garage_Area)


# 欠損値の可視化
# --- 参考：visdat (https://docs.ropensci.org/visdat/)
ames_raw %>% vis_miss(cluster = TRUE)




# 3.3.2 欠損値の補完 -----------------

## 中央値で補完

# ＜ポイント＞
# - ｢平均値｣｢中央値｣｢モード｣などの記述統計を計算してNAを置換する
# - 単純であるものの、他のデータとの関係などは一切考慮していない


# 欠損値の行NOの取得
na_no <-
  ames_raw %>%
    select(Garage_Yr_Blt) %>%
    mutate(no = row_number(),
           flg = is.na(Garage_Yr_Blt)) %>%
    filter(flg == TRUE) %>%
    use_series(no)


# レシピ作成
# --- 中央値で補完（Garage_Yr_Blt）
ames_recipe <-
  recipe(Sale_Price ~ ., data = ames_raw) %>%
    step_medianimpute(Garage_Yr_Blt)


# 中央値の確認
ames_raw$Garage_Yr_Blt %>% median(na.rm = TRUE)


# レシピ処理の確認
# --- 欠損値の行NOのみ抽出
# ---
ames_recipe %>%
  prep() %>%
  juice() %>%
  mutate(no = row_number()) %>%
  slice(na_no) %>%
  select(no, Garage_Yr_Blt)


# 参考:プロット

df_raw <- ames_raw %>% select(Garage_Yr_Blt) %>% mutate(type = "raw")
df_new <- ames_recipe %>% prep() %>% juice() %>% select(Garage_Yr_Blt) %>% mutate(type = "new")

pt1 <-
  df_raw %>%
    bind_rows(df_new) %>%
    ggplot(aes(Garage_Yr_Blt, fill = type)) +
      geom_histogram(show.legend = FALSE, bins = 40) +
      facet_wrap(~ type)

print(pt1)



## 3.3.3 k近傍法で補完 -----------------

# ＜ポイント＞
# - ある欠損値を識別して最も類似している他の観測値を使用して欠損値を代入する
# - KNN補完は、大規模なデータセットでは計算負荷が大きくなる
#   --- 中規模から小規模のデータセットにおける補完に使用
# - kは調整可能なハイパーパラメーター（補完の推奨値は5〜10）


# 欠損値の行NOの取得
na_no <-
  ames_raw %>%
    select(Garage_Yr_Blt) %>%
    mutate(no = row_number(),
           flg = is.na(Garage_Yr_Blt)) %>%
    filter(flg == TRUE) %>%
    use_series(no)


# レシピ作成
# --- 中央値で補完
ames_recipe <-
  recipe(Sale_Price ~ ., data = ames_raw) %>%
    step_knnimpute(Garage_Yr_Blt, neighbors = 6)


# レシピ処理の確認
ames_recipe %>%
  prep() %>%
  juice() %>%
  mutate(no = row_number()) %>%
  slice(na_no) %>%
  select(no, Garage_Yr_Blt)


# 参考:プロット

df_raw <- ames_raw %>% select(Garage_Yr_Blt) %>% mutate(type = "raw")
df_new <- ames_recipe %>% prep() %>% juice() %>% select(Garage_Yr_Blt) %>% mutate(type = "new")

pt2 <-
  df_raw %>%
    bind_rows(df_new) %>%
    ggplot(aes(Garage_Yr_Blt, fill = type)) +
      geom_histogram(show.legend = FALSE, bins = 40) +
      facet_wrap(~ type)

print(pt2)



## 3.3.4 ツリーモデルで補完 -----------------

# ＜ポイント＞
# - 単一ツリーは分散が大きくなるが、多くのツリーにわたって集約すると堅牢で低分散の予測子が作成される
#   --- 決定木では不十分
#   --- ランダムフォレストでは計算量が多い
# - バギングツリー、予測精度と計算負荷の間の妥協点を提供する
#   --- step_bagimpute()

# 欠損値の行NOの取得
na_no <-
  ames_raw %>%
    select(Garage_Yr_Blt) %>%
    mutate(no = row_number(),
           flg = is.na(Garage_Yr_Blt)) %>%
    filter(flg == TRUE) %>%
    use_series(no)


# レシピ作成
# --- 中央値で補完
ames_recipe <-
  recipe(Sale_Price ~ ., data = ames_raw) %>%
    step_bagimpute(Garage_Yr_Blt)


# レシピ処理の確認
ames_recipe %>%
  prep() %>%
  juice() %>%
  mutate(no = row_number()) %>%
  slice(na_no) %>%
  select(no, Garage_Yr_Blt)


# 参考:プロット

df_raw <- ames_raw %>% select(Garage_Yr_Blt) %>% mutate(type = "raw")
df_new <- ames_recipe %>% prep() %>% juice() %>% select(Garage_Yr_Blt) %>% mutate(type = "new")

pt3 <-
  df_raw %>%
    bind_rows(df_new) %>%
    ggplot(aes(Garage_Yr_Blt, fill = type)) +
      geom_histogram(show.legend = FALSE, bins = 40) +
      facet_wrap(~ type)


print(pt3)


# プロット比較
# --- NAが少ないうちはそれほど違いは見えない
# --- しかし、次項のシミュレーションを見ると、ツリー系の補完が好ましく思える
gridExtra::grid.arrange(pt1, pt2, pt3, nrow = 2)



## 3.3.5 補完方法のシミュレーション -----------------

# ＜概要＞
# - 人工的に欠損値(NA)を生成して、欠損値補完のアルゴリズムの精度を確認する
#   --- 赤い点が欠損値

# ＜ポイント＞
# - 単純な記述統計量による補完より｢KNN補完｣｢バギング補完｣の方が尤もらしい結果となる
# - 線形回帰モデルを想定すると分布が集中しない方が好ましい

# ＜結論＞
# ツリー系のKNN又はバギングが欠損値補完として適切



# ******* 元データ ********

# データ作成
# --- 50個のNAを作成（正解がわかるように人工的にNAを作成）
impute_ames <- ames_train
set.seed(123)
index <- sample(seq_along(impute_ames$Gr_Liv_Area), 50)
actuals <- ames_train[index, ]
impute_ames$Gr_Liv_Area[index] <- NA

p1 <-
  ggplot() +
    geom_point(data = impute_ames, aes(Gr_Liv_Area, Sale_Price), alpha = .2) +
    geom_point(data = actuals, aes(Gr_Liv_Area, Sale_Price), color = "red") +
    scale_x_log10(limits = c(300, 5000)) +
    scale_y_log10(limits = c(10000, 500000)) +
    ggtitle("Actual values")


# ******* 平均値で補完 ********

# Mean imputation
mean_juiced <- recipe(Sale_Price ~ ., data = impute_ames) %>%
  step_meanimpute(Gr_Liv_Area) %>%
  prep(training = impute_ames, retain = TRUE) %>%
  juice()
mean_impute <- mean_juiced[index, ]

p2 <- ggplot() +
  geom_point(data = actuals, aes(Gr_Liv_Area, Sale_Price), color = "red") +
  geom_point(data = mean_impute, aes(Gr_Liv_Area, Sale_Price), color = "blue") +
  scale_x_log10(limits = c(300, 5000)) +
  scale_y_log10(limits = c(10000, 500000)) +
  ggtitle("Mean Imputation")


# ******* KNNで補完 ********

# KNN imputation
knn_juiced <-
  recipe(Sale_Price ~ ., data = impute_ames) %>%
    step_knnimpute(Gr_Liv_Area) %>%
    prep(training = impute_ames, retain = TRUE) %>%
    juice()
knn_impute <- knn_juiced[index, ]

p3 <-
  ggplot() +
    geom_point(data = actuals, aes(Gr_Liv_Area, Sale_Price), color = "red") +
    geom_point(data = knn_impute, aes(Gr_Liv_Area, Sale_Price), color = "blue") +
    scale_x_log10(limits = c(300, 5000)) +
    scale_y_log10(limits = c(10000, 500000)) +
    ggtitle("KNN Imputation")



# ******* バギングで補完 ********

# Bagged imputation
bagged_juiced <-
  recipe(Sale_Price ~ ., data = impute_ames) %>%
    step_bagimpute(Gr_Liv_Area) %>%
    prep(training = impute_ames, retain = TRUE) %>%
    juice()
bagged_impute <- bagged_juiced[index, ]
bagged_impute$Gr_Liv_Area

p4 <-
  ggplot() +
    geom_point(data = actuals, aes(Gr_Liv_Area, Sale_Price), color = "red") +
    geom_point(data = bagged_impute, aes(Gr_Liv_Area, Sale_Price), color = "blue") +
    scale_x_log10(limits = c(300, 5000)) +
    scale_y_log10(limits = c(10000, 500000)) +
    ggtitle("Bagged Trees Imputation")


# ******* 比較 ********

# プロット比較
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)




# 3.4 特徴量フィルタリング ---------------------------------------

# ＜問題意識＞
# - 特徴量が多くなるほどモデル解釈は複雑になる
#   --- アルゴリズムによっては、特徴量選択を実装していてノイズに対しても頑健な結果を返す
#   --- ブラックボックスな処理のため、人間には分かりにくい
# - 特徴量が多いほど確実に計算コストは高まる


# ＜チャート解釈＞
# - Figure3.6
# - ノイズ特徴量の数とRMSEの関係を表記
#   --- アルゴリズムによっては無関係な特徴量に対して頑健な推定を行うことができる
#   --- 頑健： ツリー系、LASSO、Nnetが頑健
#   --- 脆弱： PLS回帰、PCA回帰、SVM

# - Figure3.7
# - ノイズ特徴量の数と計算実行時間の関係を表記
# - 無関係な特徴量が多くなると確実に計算時間がかかる


# ＜インセンティブ＞
# - 頑健性や計算コストの観点から特徴量フィルタリングは効果的な選択肢


# ＜フィルタリングの候補＞
# - ゼロ分散の特徴量
#    --- 特徴量が1つの値となるためモデルに有用な情報を提供しない
#    --- 予期しない挙動を引き起こすこともある（ツリーモデル以外）
# - 分散量が小さい特徴量
#   --- 欠損値が多い可能性がある


# ＜レシピ＞
# step_zv()
# step_nzv()


# 参考資料
# https://qiita.com/aokikenichi/items/200e91300dba99408c39


# 分散量の小さい列の確認
ames_train %>%
  caret::nearZeroVar(saveMetrics = TRUE) %>%
    tibble::rownames_to_column() %>%
    filter(nzv)




# 3.5 特徴量エンジニアリング（数値データ） ---------------------------------------

# ＜問題意識＞
# - ｢分布の歪み｣｢外れ値｣｢数値単位の違い｣などはモデルに影響を及ぼする可能性がある
#   --- ツリーモデルは全く影響を受けない（分布に対して頑健）
#   --- 多くのモデルは影響を受ける(GLM、正則回帰、KNN、サポートベクターマシン、ニューラルネットワーク)


## 3.5.1 分布の歪み --------------------

# ＜ポイント＞
# - 対数変換系の変換で対応する
# - 対数変換は1以上の正の値のみに有効
# - Box-Cox変換はlambdaの値によって対数変換の度合いを変更する
#   --- step_boxcox()ではlambdas引数はNULLにしておく（正規性た最大となるように最適化される）
# - Yao-Johnson変換は負の値に対しても適用することができる


# Yao-Johnson
ames_yj <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_YeoJohnson(all_numeric()) %>%
    prep() %>%
    juice()

# Box-Cox
ames_bc <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_BoxCox(all_numeric()) %>%
    prep() %>%
    juice()

# 対数変換
ames_lg <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_log(all_numeric()) %>%
    prep() %>%
    juice()

# プロット比較
p1 <- ames_train %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("Original")
p2 <- ames_yj %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("Yao-Johnson")
p3 <- ames_bc %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("Box-Cox")
p4 <- ames_lg %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("Log")

# プロット比較
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)



## 3.5.2 基準化 --------------------

# ＜ポイント＞
# - 分散を1にするだけでも水準を揃えるには効果的
#   --- step_scale()
# - 一般的にはZスコア化を行うことが多い
#   --- step_normalize()



# **** step_center() *****************

# 平均ゼロに変換
ames_ce <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_center(all_numeric()) %>%
    prep() %>%
    juice() %>%
    select(Sale_Price)

# 確認
ames_ce$Sale_Price %>% mean()
ames_ce$Sale_Price %>% sd()
ames_ce$Sale_Price %>% range()


# **** step_scale() *****************

# 分散を1に変換
ames_sc <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_scale(all_numeric()) %>%
    prep() %>%
    juice() %>%
    select(Sale_Price)

# 確認
ames_sc$Sale_Price %>% mean()
ames_sc$Sale_Price %>% sd()
ames_sc$Sale_Price %>% range()


# **** step_normalize() *****************

# 基準化
ames_no <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_normalize(all_numeric()) %>%
    prep() %>%
    juice() %>%
    select(Sale_Price)

# 確認
ames_no$Sale_Price %>% mean()
ames_no$Sale_Price %>% sd()
ames_no$Sale_Price %>% range()


# **** step_range() *****************

# 特定のレンジに縮尺
# --- デフォルトでは0-1
ames_ra <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_range(all_numeric()) %>%
    prep() %>%
    juice() %>%
    select(Sale_Price)

# 確認
ames_ra$Sale_Price %>% mean()
ames_ra$Sale_Price %>% sd()
ames_ra$Sale_Price %>% range()



# **** 比較 *****************


# プロット比較
p1 <- ames_train %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("Raw")
p2 <- ames_ce %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("step_center")
p3 <- ames_sc %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("step_scale")
p4 <- ames_no %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("step_normalize")
p5 <- ames_ra %>% ggplot(aes(x = Sale_Price)) + geom_histogram() + ggtitle("step_range")

# プロット比較
gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow = 2)




# 3.6 特徴量エンジニアリング（カテゴリデータ） --------------------

# ＜ポイント＞
# - ほとんどのモデルでは、Predictorは数値形式である必要がある
# - ツリーモデルモデルの場合、内部的には数値をカテゴリデータに別けるような処理を行っている



## 3.6.1 カテゴリをまとめる --------------------

# ＜ポイント＞
# - レベルが少ないサンプルを多く含むカテゴリデータは情報量が少ない
#   --- 一定の頻度を下回るカテゴリをまとめるというのも一案
#   --- まとめることに合理性があるかを考慮するべき
# - 数値データもカテゴリデータに変換することが可能


# ＜レシピ＞
# - step_other()


# データ確認
ames_train %>%
  select(Neighborhood, Screen_Porch) %>%
  glimpse()


# レベルを確認
# --- 出現頻度の低いデータも含まれる
ames_train %>% count(Neighborhood) %>% arrange(n)
ames_train %>% count(Screen_Porch) %>% arrange(n)


# プロット作成
# --- Neighborhood: 頻度の少ないカテゴリを含む
# --- Screen_Porch: 数値データ
ames_train %>% ggplot(aes(x = Neighborhood)) + geom_bar() + coord_flip()
ames_train %>% ggplot(aes(x = Screen_Porch)) + geom_histogram()


# レシピ作成
# --- カテゴリカルデータのレベルをまとめる
rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_other(Neighborhood, threshold = 0.01, other = "other") %>%
    step_other(Screen_Porch, threshold = 0.1, other = ">0")


# データ確認
df_lumping <- rec %>% prep() %>% juice()
df_lumping %>% count(Neighborhood) %>% arrange(n)
df_lumping %>% count(Screen_Porch) %>% arrange(n)


# プロット作成
# --- Neighborhood: 頻度の少ないカテゴリを含む
# --- Screen_Porch: 数値データ
df_lumping %>% ggplot(aes(x = Neighborhood)) + geom_bar() + coord_flip()
df_lumping %>% ggplot(aes(x = Screen_Porch)) + geom_bar() + coord_flip()




## 3.6.2 One-hot & dummy encoding --------------------

# ＜ポイント＞
# - カテゴリカル変数を数値表現することでモデルに使用できる
#   --- {h2o}や{caret}は自動的に変換する機能を持つ
#   --- {glmnet}や{keras}は変換してインプットする必要がある

# - 特徴量の各レベルがブール値として表されるように、カテゴリ変数を置換する
#   --- {recipe}ではstep_dummy()でサポート
#   --- 全てのカテゴリを1/0で表現する（機械学習上の呼称）
#   --- 全てのレベルを表示するので多重共線性が発生するので1つのレベルを除外する必要がある



# ＜One-hotとダミー変換＞
# - ｢One-hot｣は機械学習上の呼称、｢ダミー変換｣は統計学上の呼称
# - step_dummy()はone_hotは引数としてコントロール（同じ処理であることが分かる）



# データ準備
ames_Neighborhood <- ames_train %>% select(Sale_Price, Neighborhood)
ames_Neighborhood %>% group_by(Neighborhood) %>% count()


# レシピ作成
# --- カテゴリカルデータをダミー変数に変換
rec <-
  recipe(Sale_Price ~ ., data = ames_Neighborhood) %>%
    step_dummy(Neighborhood, one_hot = TRUE)


# データ確認
ames_Neighborhood %>% glimpse()
rec %>% prep() %>% juice() %>% glimpse()




## 3.6.3 ラベル・エンコーディング --------------------

# ＜ポイント＞
# - ラベルエンコーディングは、カテゴリ変数のレベルの純粋な数値変換
# - カテゴリー変数が因子であり、事前にレベルが指定されている場合、数値変換はレベル順になる
#   --- {glmnet}や{keras}は変換してインプットする必要がある


# ****** 不適切な変換 *********************

# ＜ポイント＞
# - step_integer()はファクター値のへレベルによって変換が行われる
#   --- レベルが設定されていない場合、テゴリ変数をアルファベット順に数値変換される
#   --- 順序がないカテゴリ変数を変換するのは好ましくない


# データ準備
ames_MS_SubClass <- ames_train %>% select(Sale_Price, MS_SubClass)
ames_MS_SubClass %>% print()


# データ確認
# --- レベルごとの頻度
ames_MS_SubClass %>% count(MS_SubClass)


# レシピ作成
# --- ラベルデータをカテゴリの頻度に変換
rec <-
  recipe(Sale_Price ~ ., data = ames_MS_SubClass) %>%
    step_integer(MS_SubClass)


# データ確認
# --- ラベルデータが数値データに変換されている
# --- 数値は出現頻度
df_int <- rec %>% prep() %>% juice()
df_int %>% count(MS_SubClass)



# ****** 適切な変換 *********************

# ＜ポイント＞
# - カテゴリ変数をアルファベット順に数値変換する
#   --- 順序がないカテゴリ変数を変換するのは好ましくない

# データ確認
# --- 順序を持つカテゴリ
ames_train %>% select(contains("Qual"))


# データ確認
# --- レベルごとの頻度
# --- カテゴリに順序が設定されている
ames_train %>% count(Overall_Qual)
ames_train$Overall_Qual %>% levels()



# データ確認
# --- レベルごとの頻度
ames_train %>% count(Overall_Qual)


# レシピ作成
# --- ラベルデータをカテゴリの頻度に変換
rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_integer(Overall_Qual)



# データ確認
# --- ラベルデータが数値データに変換されている
# --- 数値は出現頻度
df_int <- rec %>% prep() %>% juice()
df_int %>% count(MS_SubClass)




# 3.7 次元削減 ---------------------------------------

# ＜ポイント＞
# - 情報のない機能を手動で削除せずに除外するための代替アプローチ
# - 特徴エンジニアリングで次元削減アプローチを含めることは非常に一般的であることを強調したい


# レシピ作成
# --- PCAにより次元削減
rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_pca(all_numeric(), threshold = .95)


# データ作成
df_raw <- ames_train
df_pca <- rec %>% prep() %>% juice()


# データ確認
# --- 数値データがPCAに変換されている
df_raw %>% glimpse()
df_pca %>% glimpse()


# 列数確認
df_raw %>% dim()
df_pca %>% dim()




# 3.8 適切なデータ処理 ---------------------------------------

## 3.8.1 前処理の順番 --------------------


# 1. ゼロまたはゼロに近い分散機能をフィルターで除外
# 2. 必要に応じて欠損値代入を実行
# 3. 正規化して数値フィーチャの歪度を解決（対数変換、Box-Cox変換、Yao-Johnson変換）
# 4. 数値特徴を標準化（中央揃え、スケーリング）
# 5. 数値フィーチャに対して次元削減を実行（PCAなど）
# 6. ワンホットまたはダミーのカテゴリカル機能をエンコードします



## 3.8.3 ハンズオン --------------------


# レシピ作成
blueprint <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_nzv(all_nominal())  %>%
    step_integer(matches("Qual|Cond|QC|Qu")) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes()) %>%
    step_pca(all_numeric(), -all_outcomes())


# レシピ確認
blueprint %>% print()
blueprint %>% tidy()


# レシピ完成
prepare <- blueprint %>% prep(training = ames_train)
prepare


# 前処理の適用
baked_train <- prepare %>% bake(new_data = ames_train)
baked_test  <- prepare %>% bake(new_data = ames_test)


# データ確認
baked_train %>% print()
baked_train %>% glimpse()


#
blueprint <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_nzv(all_nominal()) %>%
    step_integer(matches("Qual|Cond|QC|Qu")) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes()) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)


# Specify resampling plan
cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

# Construct grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
knn_fit2 <- train(
  blueprint,
  data = ames_train,
  method = "knn",
  trControl = cv,
  tuneGrid = hyper_grid,
  metric = "RMSE"
)


# 結果確認
knn_fit2


# プロット
knn_fit2 %>% ggplot()
