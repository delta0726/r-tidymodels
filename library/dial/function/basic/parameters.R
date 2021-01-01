# Title     : parameters
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/23
# URL       : https://dials.tidymodels.org/reference/parameters.html




# ＜ポイント＞
# - 複数のパラメータをまとめてparametersオブジェクトに変換する
# - グリッドサーチのパターン生成に使用する


# パラメータリストの作成
params <- list(lambda = penalty(), alpha = mixture(), rand_forest = mtry())
params %>% print()
params %>% class()


# パラメータリストをオブジェクト化
pset <- params %>% parameters()
pset %>% print()
pset %>% class()
pset %>% glimpse()


# パラメータの確認
pset$object$rand_forest
pset$object$alpha

