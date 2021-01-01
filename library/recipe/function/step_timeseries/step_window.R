# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/03


# ＜課題＞
# - 頻度がまれな分類問題はオーバーフィッティングしやすい
# - 多くの場合、ほとんどのモデルは過半数のクラスに適合しすぎる
# - 出現頻度の高いクラスを含むクラスの非常に優れた統計を生成
# - 少数派のクラスはパフォーマンスが低下します。


# ＜ポイント＞
# - サブサンプリングは、まれにしか発生しない分類データを処理するのに役立つアプローチ
# - 少数派クラスと同じ頻度で発生するまで、多数派クラスのデータをサンプリングする
# - 直感に反するが、データの大部分を破棄することが結果を生成するのに効果的
# - レシピではstep_downsampleが用意されている


# ＜注意点＞
# - ダウンサンプリングは、トレーニングセットのみで実行することを目的としています
# - このため、デフォルトはskip = TRUE
# - prep(recipe, retain = TRUE)レシピを準備するときに使用することを推奨
# - juice()を適用して、データのダウンサンプリングされたバージョンを取得




library(recipes)
library(modeldata)


# データロード
data(okc)


# データ概要
okc %>% glimpse()


# 頻度確認
# --- 出現頻度の偏り大きい
okc$diet %>%
  table(useNA = "always") %>%
  sort() %>%
  as_tibble()

# レシピ作成
# --- ダウンサンプリング
# --- step_downsampleはデフォルトで skip=TRUE
# --- 後でjuiceするためprepを実施
ds_rec <-
  recipe( ~ ., data = okc) %>%
    step_downsample(diet) %>%
    prep(training = okc)


# レコード数の比較
okc %>% dim()
ds_rec %>% juice() %>% dim()


# データ抽出(juice)
# --- ダウンサンプリング適用される
ds_rec %>%
  juice() %>%
  use_series(diet) %>%
  table(useNA = "always") %>%
  sort() %>%
  as_tibble()


# 新たなデータにレシピ適用(bake)
# --- ダウンサンプリングが適用されない
ds_rec %>%
  bake(new_data = okc) %>%
  use_series(diet) %>%
  table(useNA = "always") %>%
  sort() %>%
  as_tibble()



