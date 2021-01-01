# Title     : Characteristic Variable
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/12



# ＜ポイント＞
# - モデルに使用するデータセットの情報を取得する際のショートカットが準備されている


# ＜一覧＞
#.obs()  ： データセットの行数。
#.preds()： 説明変数の数(ダミー変数の作成前)
#.cols() ： 説明変数の数(ダミー変数の作成後)
#.facts()： 説明変数のうち因子の数。
#.lvls() ： 結果が因子である場合、これは各レベル（およびNAその他）のカウントを含むテーブルです。
#.x()    ： 説明変数(データフレームまたは行列)
#.y()    ： 説明変数(ベクトル、行列又はデータフレーム)
#.dat()  ： データフレーム全体



# ＜例＞ iris： Sepal.Width ~ .
#.preds() =   4          (the 4 columns in `iris`)
#.cols()  =   5          (3 numeric columns + 2 from Species dummy variables)
#.obs()   = 150
#.lvls()  =  NA          (no factor outcome)
#.facts() =   1          (the Species predictor)
#.y()     = <vector>     (Sepal.Width as a vector)
#.x()     = <data.frame> (The other 4 columns as a data frame)
#.dat()   = <data.frame> (The full data set)



# ＜例＞ iris： Species ~ .
# .preds() =   4          (the 4 numeric columns in `iris`)
# .cols()  =   4          (same)
# .obs()   = 150
# .lvls()  =  c(setosa = 50, versicolor = 50, virginica = 50)
# .facts() =   0
# .y()     = <vector>     (Species as a vector)
# .x()     = <data.frame> (The other 4 columns as a data frame)
# .dat()   = <data.frame> (The full data set)