# *********************************************************************************************************************
# Title     : 5 Encoding Categorical Predictors
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/30
# URL       : http://www.feat.engineering/encoding-categorical-predictors.html
# *********************************************************************************************************************


# ＜ポイント＞
#
#
#



# ＜目次＞
# 5.0 準備
# 5.1 ダミー変数の作成
# 5.2 カテゴリを持つ予測子のエンコード
# 5.3 新規カテゴリーへのアプローチ
# 5.4 教師あり学習によるエンコーディング
# 5.5 順序データのエンコーディング
# 5.6 テキストデータからの特徴量作成
# 5.7 ツリーベースモデルの因子とダミー変数




# 5.0 準備 ----------------------------------------------------------------------

library(tidymodels)
library(FeatureHashing)
library(stringr)


# プロットテーマ
theme_set(theme_bw())


# ディレクトリ設定
setwd("I:/Project/R/tidymodels/book/ml_feature")


# データロード
load("data/OkCupid/okc.RData")


options(width = 150)


sample_towns <- c(
  'alameda', 'belmont', 'benicia', 'berkeley', 'castro_valley', 'daly_city',
  'emeryville', 'fairfax', 'martinez', 'menlo_park', 'mountain_view', 'oakland',
  'other', 'palo_alto', 'san_francisco', 'san_leandro', 'san_mateo',
  'san_rafael', 'south_san_francisco', 'walnut_creek'
)


location <-
  okc_train %>%
  dplyr::select(where_town) %>%
  distinct(where_town) %>%
  arrange(where_town)


# 5.2 ダミー変数の作成 -------------------------------------------------------

# ＜ポイント＞
# - カテゴリ値を数値データとして表す最も基本的な方法は、ダミー変数またはインジケータ変数を作成すること
#   --- 例えば7カテゴリの場合は6個のダミー変数で表現する



# 5.3 新規カテゴリーへのアプローチ--------------------------------------------------


# ------------------------------------------------------------------------------
# Create hash features using binary representations

binary_hashes <-
  hashed.model.matrix(
    ~ where_town,
    data = location,
    hash.size = 2 ^ 4,
    signed.hash = FALSE,
    create.mapping = TRUE
  )

binary_mapping <- hash.mapping(binary_hashes)
names(binary_mapping) <- str_remove(names(binary_mapping), "where_town")
binary_calcs <-
  binary_mapping %>%
  enframe() %>%
  set_names(c("town", "column_num_16")) %>%
  mutate(integer_16 = hashed.value(names(binary_mapping))) %>%
  dplyr::filter(town %in% sample_towns) %>%
  arrange(town)

# Table 5.2
# https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html#tab:categorical-hash-values

binary_calcs

binary_df <-
  binary_hashes %>%
  as.matrix() %>%
  as_tibble() %>%
  bind_cols(location) %>%
  dplyr::rename(town = where_town) %>%
  dplyr::filter(town %in% sample_towns) %>%
  arrange(town)

# TODO update links
# Table 5.3
# https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html#tab:categorical-hash-values
binary_df

# ------------------------------------------------------------------------------
# Create hash features using signed integer representations

signed_hashes <-
  hashed.model.matrix(
    ~ where_town,
    data = location,
    hash.size = 2 ^ 4,
    signed.hash = TRUE
  )

signed_df <-
  signed_hashes %>%
  as.matrix() %>%
  as_tibble() %>%
  bind_cols(location) %>%
  dplyr::rename(town = where_town) %>%
  dplyr::filter(town %in% sample_towns) %>%
  arrange(town)

# TODO update links
# Table 5.4
# https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html#tab:categorical-hash-values
signed_df