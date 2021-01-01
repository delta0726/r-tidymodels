# Title     : update
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/29
# URL       : https://dials.tidymodels.org/reference/update.parameters.html



# ＜ポイント＞
# - parametersオブジェクトに格納されたアイテムのパラメータを更新する
# - finalize()でデータに応じたパラメータを決定し、それを更新することで設定する




library(tidyverse)
library(tidymodels)


#%% 準備 ----------------------------------------------------

# パラメータリストの作成
# --- listオブジェクト
params <- list(lambda = penalty(), alpha = mixture(), rand_forest = mtry())
params %>% print()
params %>% class()


# パラメータリストをオブジェクト化
# --- parametersオブジェクト
pset <- params %>% parameters()
pset %>% print()
pset %>% class()
pset %>% glimpse()


# パラメータの確認
# --- rand_forestのrangeは[1, ?]となっている
# --- ?はデータフレームの列数によって決まる
pset$object$rand_forest
pset$object$alpha



#%% update() ----------------------------------------------------

# データフレーム
# --- 11列
mtcars %>% as_tibble()


# パラメータの更新
# --- finalize()でデータフレームの大きさを与える
pset_update <-
  pset %>%
    update(rand_forest = finalize(mtry(), mtcars),
           alpha = mixture(c(.1, .2)))


# パラメータの確認
pset_update$object$rand_forest
pset$object$alpha

