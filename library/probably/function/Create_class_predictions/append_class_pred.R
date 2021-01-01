# Title     : append_class_pred
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/2
# URL       : https://probably.tidymodels.org/reference/append_class_pred.html




# ＜ポイント＞
# - presdict()の出力値からpred_classオブジェクトを作成する
#   ---データフレームの中で列として使用することが想定されている
# - make_class_pred()と同様にマルチクラスを扱うが、以下の2点が異なる
#   --- append_class_pred()はmutate()を使わずに列追加できる
#   --- append_class_pred()はtidyselectが使える


# ＜構文＞
# append_class_pred(
#   .data,
#   ...,
#   levels,
#   ordered = FALSE,
#   min_prob = 1/length(levels),
#   name = ".class_pred"
# )



# 1 使用例 --------------------------------------------------------------------


library(tidyverse)
library(probably)



# mutate()で追加
species_probs %>%
  mutate(.class_pred = make_class_pred(.pred_bobcat, .pred_coyote, .pred_gray_fox,
                                       levels = levels(Species),
                                       min_prob = .5))


# 直接追加
species_probs %>%
  append_class_pred(contains(".pred_"),
                    levels = levels(.$Species),
                    min_prob = .5)

