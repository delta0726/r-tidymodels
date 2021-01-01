# Title     : Applicability domain methods for binary data
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/11
# URL       : https://applicable.tidymodels.org/articles/binary-data.html




# ＜ポイント＞
# - 予測子がバイナリであるデータセットを比較するために使用する
# - Jaccard Indexを使用して類似性を計算する




# 1.準備 -------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(applicable)
library(AmesHousing)


# データ準備
data(qsar_binary)


# データ確認
binary_tr %>% print()
binary_tr %>% glimpse()



# 2.類似性の検証 -------------------------------------------------------------

# 類似度の計算
jacc_sim <- binary_tr %>% apd_similarity()
jacc_sim %>% print()




# Plot the empirical cumulative distribution function for the training set
jacc_sim %>% autoplot()



# 3.新しいデータの評価 -------------------------------------------------------

# 類似性スコア
mean_sim <- jacc_sim %>% score(new_data = binary_unk)
mean_sim


