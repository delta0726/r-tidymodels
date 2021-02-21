#***************************************************************************************
# Title     : TIDY MODELING WITH R
# Chapter   : 6 Feature engineering with recipes
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/13
# URL       : https://www.tmwr.org/
#***************************************************************************************


# ＜概要＞
# - モデルが異なれば前処理要件も異なる
#   --- ツリーベースのモデルなど前処理がほとんど必要ないアルゴリズムもある
#   --- 付録Aには、さまざまなモデルに推奨される前処理手法の一覧表として収録されている


# ＜ポイント＞
# - 前処理の大半の処理が{recipe}と関連パッケージに含まれている
# - レシピを使うことでフォーミュラを単純化することが可能（重要）
#   --- フォーミュラ段階でlog(Feature1)などの表現をする必要がない
#   --- 単純な列変換など前処理として都度行う必要がないものは、レシピの外で行うことが推奨される


# ＜目次＞
# 0 準備
# 1 単純なレシピ
# 2 連続データの離散化
# 3 相互効果の項を追加
# 4 前処理のスキップ
# 5 スプラインを用いた前処理
# 6 その他の前処理
# 7 レシピを含めた一連のプロセス
# 8 レシピidの活用


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(patchwork)
library(splines)

# データ準備
data(ames)

# データ変換
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

# データ分割
set.seed(123)
ames_split <- ames %>% initial_split(prob = 0.80, strata = Sale_Price)
ames_train <- ames_split %>% training()
ames_test  <- ames_split %>% testing()


# 1 単純なレシピ ----------------------------------------------------------------

# ＜ポイント＞
# - レシピの一連の操作を確認する
# - workflow()に乗せるとprep()やbake()などを使用する必要がない


# フォーミュラ
# --- この式を想定して前処理を作成
# lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type)

# データ確認
ames_train %>%
  select(Sale_Price, Neighborhood, Gr_Liv_Area, Year_Built, Bldg_Type)

# レシピ定義
simple_ames <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_dummy(all_nominal())

# レシピ確認
simple_ames %>% print()

# レシピ訓練
# --- 訓練データに対してレシピが適用された
simple_ames <- simple_ames %>% prep(training = ames_train)
simple_ames %>% print()

# レシピ適用
# --- データ全体に適用（一般的なユースケース）
test_ex <- simple_ames %>% bake(new_data = ames_test)
test_ex %>% glimpse()

# レシピ適用
# --- tidyselectによる列指定をすることも可能（あまり用途が思いつかない）
simple_ames %>%
  bake(new_data = ames_test, starts_with("Neighborhood_")) %>%
  glimpse()

# レシピ適用
# --- NULLを渡すとトレーニングセットをすばやく返すことができます
# --- prep()でretain=TRUEが使用された場合
# --- juice()と同様の機能
simple_ames %>%
  bake(new_data = NULL) %>%
  glimpse()


# 2 カテゴリカルデータのレシピ ---------------------------------------------------------------

# ＜ポイント＞
# - カテゴリカルデータはダミー変換を行うのがセオリーだが、以下の点を同時に確認する必要がある
#   --- step_dummy()： ダミー変換かOne-Hot変換化を選択
#   --- step_other()： 頻度が閾値を下回るカテゴリを｢other｣としてまとめる
#   --- step_unknown()： 欠落しているカテゴリを補完（エラー回避）
#   --- step_novel(): 将来データで新しいレベルが発生することを想定して｢newdata｣のレベルを準備（エラー回避）


# プロット作成
# --- Neighborhoodをカテゴリごとにカウント
# --- 少数カテゴリが含まれる（少数カテゴリは一般的に予測しにくい）
ames_train %>%
  ggplot(aes(y = Neighborhood)) +
    geom_bar() +
    labs(y = NULL)

# レシピ作成
# --- step_other()で少数カテゴリをまとめる
# --- step_dummy()でダミー変換（1カテゴリを除く方法）
simple_ames <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal())

# レシピ確認
simple_ames %>% print()

# レシピ適用後のデータ
# --- Neighborhood_otherが追加されている
simple_ames %>% prep() %>% juice() %>% glimpse()


# 3 相互効果の項を追加 ------------------------------------------------------------

# ＜ポイント＞
# - 特徴量が相互に影響を与えている場合の効果を抽出する
#   --- カテゴリカルデータでも数値データでも定義することが可能
#   --- 数値データでは特徴量間の交互作用項はそれらの積としてエンコードされる


# データ確認
ames_train %>% select(Sale_Price, Gr_Liv_Area, Bldg_Type)

# プロット作成
# --- カテゴリごとに線形回帰直線を追加
ames_train %>%
  ggplot(aes(x = Gr_Liv_Area, y = 10^Sale_Price)) +
    geom_point(alpha = .2) +
    facet_wrap(~ Bldg_Type) +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = "red") +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = "General Living Area", y = "Sale Price (USD)")

# フォーミュライメージ
# Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Bldg_Type + log10(Gr_Liv_Area):Bldg_Type

# レシピ作成
# --- 上記のフォーミュラをレシピで表現
# --- step_interact()で相互効果を追加
simple_ames <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )

# レシピ確認
simple_ames %>% print()

# レシピ適用後のデータ
# --- Gr_Liv_Area_x_*で相互効果が追加されている
simple_ames %>% prep() %>% juice() %>% glimpse()


# 4 前処理のスキップ ---------------------------------------------------------------

# ＜ポイント＞
# - step_*()の関数にはskip引数が用意されており、bake()の際に無視させることができる
#   --- 訓練データと検証データで同じ前処理を適用するのが適切でない場合に使用する


# ＜事例＞
# アンダーサンプリング
# - クラス不均衡データの場合は、アンダーサンプリングを行ったデータで学習器を作成する
# - テストデータに学習器を適用する場合は元データをそのまま適用（bake-skip）
# - bake(new_data=NULL)とすると、訓練時のスキップしなかったデータがそのまま使用されてしまう


# 5 スプラインを用いた前処理 ------------------------------------------------------------

# 関数定義
# --- プロット作成
plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = Sale_Price)) +
    geom_point(alpha = .2) +
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      col = "red",
      se = FALSE
    ) +
    ggtitle(paste(deg_free, "Spline Terms"))
}

# プロット作成
# --- 2*2で並べて表示
( plot_smoother(2) + plot_smoother(5) ) /
  ( plot_smoother(20) + plot_smoother(100) )

# レシピ作成
# --- step_ns()で
rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
           data = ames_train) %>%
    step_log(Gr_Liv_Area, base = 10) %>%
    step_other(Neighborhood, threshold = 0.01) %>%
    step_dummy(all_nominal()) %>%
    step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
    step_ns(Latitude, deg_free = 20)

# レシピ確認
rec %>% print()

# レシピ適用後のデータ
# --- Gr_Liv_Area_x_*で相互効果が追加されている
rec %>% prep() %>% juice() %>% glimpse()


# 6 その他の前処理 ------------------------------------------------------------

# ＜特徴量抽出＞
# - PCAなどの次元圧縮手法により新たな特徴量を抽出する
# - 特徴量の相関を低減させるのに非常に効果的
# - 一般的にstep_normalize()などでデータ基準化を事前に行う必要がある


# ＜データサンプリング＞
# - ラベル不均衡問題においてダウンサンプリングやオーバーサンプリングは非常に効果的
# - {themis}で多くの手法を扱っている
# - 訓練データのみに適用して、検証データ/テストデータには使用しないのが一般的



# 7 レシピを含めた一連のプロセス ------------------------------------------------------------

# レシピ作成
ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Latitude, Longitude, deg_free = 20)

# レシピ訓練
ames_rec_prepped <- ames_rec %>% prep()

# レシピの適用
# ---- 訓練データ/テストで^他に適用
# --- 訓練データはprep()の時に作成したデータを使用
ames_train_prepped <- ames_rec_prepped %>% bake(new_data = NULL)
ames_test_prepped <- ames_rec_prepped %>% bake(ames_test)

# モデリング
# --- レシピ適用後の訓練データを使用
lm_fit <- lm(Sale_Price ~ ., data = ames_train_prepped)

# モデルサマリー
# --- {broom}でモデル概要をtidyに取得
lm_fit %>% glance()
lm_fit %>% tidy()

# 予測
lm_fit %>% predict(ames_test_prepped) %>% head()


# 8 レシピidの活用 ----------------------------------------------------------------

# レシピ手順の確認
# --- 前章のレシピのステップ
# --- idフィールドにはランダムなサフィックスで名前が生成されている
ames_rec_prepped %>% tidy()

# レシピ作成
# --- step_log()でidを指定
ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10, id = "my_id") %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>%
  step_ns(Latitude, Longitude, deg_free = 20)

# レシピ訓練
ames_rec_prepped <- ames_rec %>% prep()

# ステップを抽出
# --- id
ames_rec_prepped %>% tidy()
ames_rec_prepped %>% tidy(id = "my_id")

# ステップを抽出
# --- number
ames_rec_prepped %>% tidy(number = 2)

