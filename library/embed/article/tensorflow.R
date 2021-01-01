# ******************************************************************************
# Title     : Tensorflow
# Objective : TODO
# Created by: Owner
# Created on: 2020/11/24
# URL       : https://embed.tidymodels.org/articles/Applications/Tensorflow.html
# ******************************************************************************



# 0 環境構築
# 1 データ準備
# 2 レシピ作成
# 3 データ確認
# 4 プロット確認



# 0 環境構築 --------------------------------------------------------------------

# ライブラリ
library(reticulate)
library(magrittr)
library(tidyverse)
library(tidymodels)
library(embed)
library(ggiraph)


# 仮想環境の選択
use_condaenv("C:/Users/Owner/Anaconda3/envs/r-reticulate", required = TRUE)
py_config()




# 1 データ準備 --------------------------------------------------------------------

# データロード
data(ames)


# データ確認
ames %>% glimpse()


# レベル数の確認
# --- Neighborhood列
ames$Neighborhood %>% levels() %>% length()


# ヒストグラム
# --- Neighborhood列
ames %>%
  ggplot(aes(x = Neighborhood)) +
    geom_bar() +
    coord_flip() +
    xlab("") +
    theme_bw()


# 基本統計量
# --- 後ほどleft_join()で使用
# --- Sale_Price(mean, n)
# --- Longitude(median)
# --- Latitude(median)
means <-
  ames %>%
    group_by(Neighborhood) %>%
    summarise(mean = mean(log10(Sale_Price)),
              n    = length(Sale_Price),
              lon  = median(Longitude),
              lat  = median(Latitude))



# 2 レシピ作成 ------------------------------------------------------------------------

# レシピ定義
# --- NeuralNetを行うためのレシピを適用
# --- Neighborhood列にstep_embed()を適用する
rec <-
  recipe(Sale_Price ~ ., data = ames) %>%
    step_log(Sale_Price, base = 10) %>%
    step_YeoJohnson(Lot_Area, Full_Bath, Gr_Liv_Area)  %>%
    step_range(Lot_Area, Full_Bath, Gr_Liv_Area)  %>%
    step_embed(Neighborhood,
               outcome = vars(Sale_Price),
               predictors = vars(Lot_Area, Full_Bath, Gr_Liv_Area),
               num_terms = 5,
               hidden_units = 10,
               options = embed_control(epochs = 75, validation_split = 0.2))


# レシピ作成
# --- この段階でtensorflowが実行される
tf_embed <- rec %>% prep(training = ames)


# プロット作成
# --- 学習曲線
# --- 4番目のステップがstep_embed()
tf_embed$steps[[4]]$history %>%
  filter(epochs > 1) %>%
  ggplot(aes(x = epochs, y = loss, col = type)) +
  geom_line() +
  scale_y_log10() +
  theme_bw() +
  theme(legend.position = "top")



# 3 データ確認 ------------------------------------------------------------------------

# データ確認
tf_embed %>%
  tidy(number = 4) %>%
  glimpse()


# データ加工
# --- 列名変換
hood_coef <-
  tf_embed %>%
    tidy(number = 4) %>%
    dplyr::select(-terms, -id)  %>%
    dplyr::rename(Neighborhood = level) %>%
    # Make names smaller
    rename_at(vars(contains("emb")), funs(gsub("Neighborhood_", "", ., fixed = TRUE)))


# データ確認
hood_coef %>% print()


# データ結合
hood_coef <-
  hood_coef %>%
  inner_join(means, by = "Neighborhood")


# データ確認
hood_coef %>% print()



# 4 プロット確認 ------------------------------------------------------------------------

# プロット作成
tf_plot <-
  hood_coef %>%
    dplyr::select(-lon, -lat) %>%
    gather(variable, value, starts_with("embed")) %>%
    # Clean up the embedding names and add a new variable as a hover-over/tool tip
    # aesthetic for the plot
    mutate(label = paste0(gsub("_", " ", Neighborhood), " (n=", n, ")"),
           variable = gsub("_", " ", variable)) %>%
    ggplot(aes(x = value, y = mean)) +
    geom_point_interactive(aes(size = sqrt(n), tooltip = label), alpha = .5) +
    facet_wrap(~variable, scales = "free_x") +
    theme_bw() +
    theme(legend.position = "top") +
    ylab("Mean (log scale)") +
    xlab("Embedding")


# Convert the plot to a format that the html file can handle
ggiraph(ggobj = tf_plot)


# 相関係数
# --- Predictor同士では相関が見られる
hood_coef %>%
  dplyr::select(contains("emb")) %>%
  cor() %>%
  round(2)



