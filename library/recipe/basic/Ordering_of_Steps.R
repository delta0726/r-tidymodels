# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/02



# ＜推奨ステップ＞
#
# 1. 欠損値の補完
# 2. 列単位の変換処理（分布の歪みなど）
# 3. 離散化（連続変数）
# 4. ダミー変数の作成（カテゴリカル変数）
# 5. 相互効果の作成
# 6. 基準化 (center, scale, range, etc)
# 7. 多変量変換 (e.g. PCA, spatial sign, etc)

# ※ 上記のカテゴリごとにstep_関数群が用意されている



# ＜注意事項＞

# - Box-Cox変換を行う前にセンタリングの処理を行わない（負値があればYao-Johnson変換を使う）
# - recipesではカテゴリカル変数の自動変換はない（step_dummy関数で変換）



library(recipes)

# step_関数群
ls("package:recipes", pattern = "^step_")