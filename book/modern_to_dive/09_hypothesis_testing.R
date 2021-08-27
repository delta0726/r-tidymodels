# **************************************************************************************
# Book      : Modern Dive into R
# Theme     : Ⅲ Statistical Interface with infer
# Title     : Hypothesis Testing
# Created on: 2021/08/28
# URL       : https://moderndive.com/9-hypothesis-testing.html
# **************************************************************************************


# ＜概要＞
# - inferのフレームワークによりシミュレーションベースの統計的仮説検定を行うことが可能
#   --- 長らく使われていた理論ベースの統計的仮説検定の近似値
#   --- 個別の理論ベースに焦点を当てるよりも高い学習効果が得られる


# ＜目次＞
# 9-0 準備
# 9-1 ケーススタディ：プロモーション活動
# 9-2 仮説検定を理解する
# 9-3 仮説検定を行う
# 9-4 仮説検定を解釈する
# 9-5. ケーススタディ：映画の評価


# 9-0 準備 -------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(infer)
library(gridExtra)
library(moderndive)
library(nycflights13)
library(ggplot2movies)


# 9-1 ケーススタディ：プロモーション活動 --------------------------------------

# ＜概要＞
# - 銀行の昇格において性別が影響するかを｢比率の差｣を計算することで検証する
#   --- 仮説検定への導入


# ＜目次＞
# 9-1-1 性別は銀行での昇進に影響するか
# 9-1-2 1回だけシャッフルしてみる
# 9-1-3 シャッフルを16回実施
# 9-1-4 まとめ


# 9-1-1 性別は銀行での昇進に影響するか --------------------------

# ＜ポイント＞
# - サンプルデータを見た限りでは、女性であると昇格に不利に思える
#   --- 有意に差があるといえるのだろうか？


# データ確認
# --- 48人の男女の昇進結果（母集団からサンプリングした一例）
# --- id列が含まれており個人の特定が可能
promotions %>% print()

# テーブル集計
# --- 男女はそれぞれ24人
# --- 女性のnotが多いように見える
promotions %>% select(decision, gender) %>% table()

# サンプル抽出
# --- 6/48
promotions %>%
  sample_n(size = 6) %>%
  arrange(id)

# プロット
# --- サンプル抽出の48件だけを見ると女性不利
# --- 男性：87.5％  女性：58.3％
# --- 差は29-2%
p1 <-
  promotions %>%
    ggplot(aes(x = gender, fill = decision)) +
    geom_bar() +
    labs(x = "Gender of name on résumé")

# 確認
# --- プロット
# --- 構成比率
print(p1)
promotions %>% select(gender, decision) %>% table() %>% prop.table() %>% round(2)


# 9-1-2 1回だけシャッフルしてみる -----------------------------------

# ＜ポイント＞
# - 昇進における性差別が存在しなかったと仮定する（重要）
# - 性別ラベルに関係なく無作為にシャッフルして抽出することができる
# - サンプルデータのgenderラベルシャフルすることで大きな影響はないはず


# ＜ポイント＞
# - ｢比率の差｣に注目
# - シャッフルによって｢比率の差｣は縮小した


# データ確認
# --- 母集団からシャッフルして48レコードを抽出
# --- 9-1-1で使用したサンプリング結果をpromotionsをシャッフルしたわけではない
promotions_shuffled %>% print()

# プロット
p2 <-
  promotions_shuffled %>%
    ggplot(aes(x = gender, fill = decision)) +
    geom_bar() +
    labs(x = "Gender of résumé name : shuffled")

# 確認
# --- プロット
# --- 構成比率
print(p2)
promotions_shuffled %>% select(gender, decision) %>% table() %>% prop.table() %>% round(2)


# プロット比較
# --- 左：promotions
# --- 右：promotions_shuffled
grid.arrange(p1, p2, nrow = 1)


# 9-1-3 シャッフルを16回実施 ------------------

# ＜スプレッドシート＞
# - シャッフル実験をするために16人で抽出実験を行う
# - 列ごとに16人が順番にシミュレーションを行う
# - 行には昇進結果が表示されている
# - データには48人の男女が性別で表示されている


# ＜グラフ＞
# - ヒストグラムは性差別がないと仮定した世界の｢比率の差｣を示している
#   --- ランダム抽出した際の男女の昇進の比率の差を示している
#   --- ゼロを中心に分布しており、性別による比率の差はないことを示している
#   --- ただし、ヒストグラムにばらつきは存在しており、サンプリングによる変動があることを示している

# - 赤い縦線は元データの｢比率の差｣を示している


# 9-1-4 まとめ ------------------

# - ここで示したのは、順列検定を使用した仮説検定と呼ばれる統計手法
# - ｢順列｣というのは｢シャッフル｣のことを意味する


# 9-2 仮説検定を理解する -------------------------------------------------------

# 省略


# 9-3 仮説検定を行う -----------------------------------------------------------


# ＜ポイント＞
# - 8章の信頼区間の算出では、specify()とgenerate()の順番でプロセスを進めた
# - 仮説検定では、上記のプロセスの間にhypothesize()を追加する
# - この仮説検定では、事前に指定された有意水準α= 0.05を使用する


# ＜目次＞
# 9-3-1 {infer}を用いたワークフロー
# 9-3-2 信頼区間との比較


# 9-3-1 {infer}を用いたワークフロー -----------------------------

# ＜ステップ＞
# 1. データ指定（specify）
# 2. 帰無仮説の設定（hypothesize）
# 3. シミュレーション（generate）
# 4. 統計量の計算（calculate）
# 5. 可視化（visualize）
# 6. p値の取得（get_p_value）


# データ確認
# --- 48人の男女の昇進結果（母集団から抽出したサンプル）
# --- id列が含まれており個人の特定が可能
promotions %>% print()
promotions %>% select(decision, gender) %>% table() %>% prop.table() %>% round(2)

# 1. データ指定（specify）
# --- 応答変数：decision（昇進結果）
# --- 説明変数：gender（性別）
# --- 興味がある対象：promoted(応答変数のカテゴリ)
# --- グループ情報のようにヘッダーにメタ情報が表示される（データに変化なし）
promotions %>%
  specify(formula = decision ~ gender, success = "promoted")


# 2. 帰無仮説の設定（hypothesize）
# --- H0: Pm - Pf = 0 (帰無仮説) ⇒ 男性(m)と女性(f)に差はない
# --- HA: Pm - Pf > 0 (対立仮説) ⇒ 男性(m)は女性(f)よりも大きい
# --- null = point(点推定) / independence(区間推定)
# --- グループ情報のようにヘッダーにメタ情報が表示される（データに変化なし）
promotions %>%
  specify(formula = decision ~ gender, success = "promoted") %>%
  hypothesize(null = "independence")


# 3. シミュレーション（generate）
# --- 帰無仮説がTrueであると想定してシャッフルされたデータセットを生成する
# --- 9-1で説明した性別ラベルのみを無作為に変化させる
# --- ｢bootstrap｣でなく｢permute｣で実行している(非復元抽出のリサンプリング)
promotions_generate <-
  promotions %>%
    specify(formula = decision ~ gender, success = "promoted") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute")

# 確認
# --- 48000レコード（元データが48レコードで*1000回のシミュレーション）
promotions_generate %>% nrow()
promotions_generate %>% group_by(replicate) %>% tally()


# 4. 統計量の計算（calculate）
# --- diff in props(比率の差)を計算する
# --- orderで比較対象を提示する（要素の順序は重要ではない）
null_distribution <-
  promotions %>%
    specify(formula = decision ~ gender, success = "promoted") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "diff in props", order = c("male", "female"))

# 確認
# --- シミュレーションごとの比率の差を計算
null_distribution %>% print()


# サンプルにおける比率差を取得
# --- 9-1-1で計算した結果と同じ
# --- プロットのshadeラインに使用
obs_diff_prop <-
  promotions %>%
    specify(decision ~ gender, success = "promoted") %>%
    calculate(stat = "diff in props", order = c("male", "female"))

# 確認
obs_diff_prop %>% print()


# 5. 可視化（visualize）
# --- 帰無分布を表示(比率に差がないと仮定した分布)
# --- 色を変えた部分の面積はp値に相当
null_distribution %>%
  visualize(bins = 10) +
  shade_p_value(obs_stat = obs_diff_prop, direction = "right")


# 6. p値の取得（get_p_value）
# --- このp値は｢性別による差別がない｣という仮説の世界を棄却するのに十分なほど小さい
# --- つまり、｢性別による差別がない｣とは言えない
null_distribution %>%
  get_p_value(obs_stat = obs_diff_prop, direction = "right")



# 9-3-2 信頼区間との比較 ---------------------------------

# ＜ポイント＞
# - inferでは｢仮説検定の実行｣と｢信頼区間の構築｣をシームレスに移動することができる


# 帰無分布
# --- 9-3.1で作成したもの
null_distribution <-
  promotions %>%
    specify(formula = decision ~ gender, success = "promoted") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "diff in props", order = c("male", "female"))


# ブートストラップ分布
# Change 1 - Remove hypothesize():
# Change 2 - Switch type from "permute" to "bootstrap":
bootstrap_distribution <-
  promotions %>%
    specify(formula = decision ~ gender, success = "promoted") %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "diff in props", order = c("male", "female"))


# 信頼区間の取得
# --- パーセンタイル方式
percentile_ci <-
  bootstrap_distribution %>%
  get_confidence_interval(level = 0.95, type = "percentile")


# 確認
percentile_ci %>% print()


# プロット
# --- 信頼区間
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = percentile_ci)


# 信頼区間の取得
# --- 標準誤差方式
se_ci <-
  bootstrap_distribution %>%
  get_confidence_interval(level = 0.95, type = "se", point_estimate = obs_diff_prop)


# 確認
se_ci %>% print()


# プロット
# --- 信頼区間
bootstrap_distribution %>%
  visualize() +
  shade_confidence_interval(endpoints = se_ci)



# 9-4 仮説検定を解釈する -------------------------------------------------------------


# 説明文



# 9-5. ケーススタディ：映画の評価 -----------------------------------------


# ＜課題＞
# アクションまたはロマンス映画はIMDbでより高く評価されていますか？


# ＜目次＞
# 1 課題認識
# 2 {infer}のワークフロー



# 1 課題認識 -------------------------------------------------

# データ確認
# --- 母集団
movies %>% print()
movies %>% glimpse()

# データ確認
# --- サンプル
movies_sample %>% print()

# プロット作成
# --- ロマンス映画のほうが中央値が高い
# --- ロマンス映画のほうがレンジが広い
movies_sample %>%
  ggplot(aes(x = genre, y = rating)) +
    geom_boxplot() +
    labs(y = "IMDb rating")

# データ集計
# --- ジャンルごとの統計量
# --- 中央値はロマンス映画のほうが1.047だけ高い
movies_sample %>%
  group_by(genre) %>%
  summarize(n           = n(),
            mean_rating = mean(rating),
            std_dev     = sd(rating))


# 2 {infer}のワークフロー ----------------------------------

# 1. データ指定（specify）
# --- 応答変数：rating
# --- 説明変数：genre
# --- グループ情報のようにヘッダーにメタ情報が表示される（データに変化なし）
movies_sample %>%
  specify(formula = rating ~ genre)


# 2. 帰無仮説の設定（hypothesize）
# --- H0: Pm - Pf = 0 (帰無仮説) ⇒ 男性(m)と女性(f)に差はない
# --- HA: Pm - Pf <> 0 (対立仮説) ⇒ 男性(m)は女性(f)に差はある
# --- null = point(点推定) / independence(区間推定)
# --- グループ情報のようにヘッダーにメタ情報が表示される（データに変化なし）
movies_sample %>%
  specify(formula = rating ~ genre) %>%
  hypothesize(null = "independence")


# 3. シミュレーション（generate）
# --- 帰無仮説がTrueであると想定してシャッフルされたデータセットを生成する
# --- ｢bootstrap｣でなく｢permute｣で実行している(非復元抽出のリサンプリング)
movies_sample %>%
  specify(formula = rating ~ genre) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")


# 4. 統計量の計算（calculate）
# --- diff in means(平均の差)を計算する
# --- orderで比較対象を提示する（要素の順序は重要ではない）
null_distribution_movies <-
  movies_sample %>%
    specify(formula = rating ~ genre) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 1000, type = "permute") %>%
    calculate(stat = "diff in means", order = c("Action", "Romance"))

# 確認
null_distribution_movies


# 5. サンプルの平均差
# --- p値の算出
obs_diff_means <-
  movies_sample %>%
    specify(formula = rating ~ genre) %>%
    calculate(stat = "diff in means", order = c("Action", "Romance"))

# 確認
obs_diff_means


# 6. 可視化
# --- 分布とp値
null_distribution_movies %>%
  visualize(bins = 10) +
    shade_p_value(obs_stat = obs_diff_means, direction = "both")


# p値の取得
null_distribution_movies %>%
  get_p_value(obs_stat = obs_diff_means, direction = "both")


