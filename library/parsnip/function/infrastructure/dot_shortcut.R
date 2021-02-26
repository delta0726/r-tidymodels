# *****************************************************************************
# Title     : dot_shortcut
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/15
# URL       : https://parsnip.tidymodels.org/reference/descriptors.html
# *****************************************************************************


# ＜ポイント＞
# - fit()を使用する場合、引数で使用できるいくつかの変数が設定されている
# - 分離された｢データ｣や｢フォーミュラ｣とモデルを紐づける



# ＜ドット引数＞
# .obs()  : データセットの行数 (150)
# .preds(): ダミー変数の作成前に予測子に関連付けられているデータセット内の列数(4)
# .cols() : ダミー変数(存在する場合)が作成された後に使用可能な予測子の列数(5: Speciesで+2)
# .facts(): データセット内の因子予測子の列数(1)
# .lvls() :
# .x()    : フォーミュラで指定されたX
# .y()    : フォーミュラで指定されたY
# .dat()  : 元のデータセット



library(tidyverse)
library(tidymodels)
library(modeldata)


# データロード
data("lending_club")


# データ概要
lending_club %>% as_tibble()
lending_club %>% names()


# モデル構築
rand_forest(mode = "classification", mtry = .cols() - 2)


