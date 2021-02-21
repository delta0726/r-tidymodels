# ********************************************************************************
# Title   : K近傍法
# Theme   : Measuring similarity
# Chapter : 9
# URL     : https://bradleyboehmke.github.io/HOML/knn.html
# Support : https://koalaverse.github.io/homlr/notebooks/08-knn.nb.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(recipes)
library(rsample)
library(caret)
library(ggmap)



# 0. ポイント整理 ----------------------------------------------

# ＜ポイント＞




# 1. データ準備 ----------------------------------------------


# データ取得
ames <- read_csv("data/ames.csv")


# データ分割
set.seed(123) 
split  <- ames %>% initial_split(prop = 0.7, strata = "Sale_Price")
ames_train  <- split %>% training()
ames_test   <- split %>% testing()


# データ確認
ames_train %>% dim()
ames_test %>% dim()




# 2. 前処理 ----------------------------------------------

# レシピ作成
recipe_ames <- 
  recipe(Sale_Price ~ ., data = ames_train) %>%
    step_nzv(all_nominal()) %>%
    step_integer(matches("Qual|Cond|QC|Qu")) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes()) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
    prep(training = ames_train, retain = TRUE)

# データ作成
df <- 
  recipe_ames %>%
    juice() %>%
    select(-Sale_Price)

#　データ確認
df %>% glimpse()



# 3. プロット作成 ----------------------------------------------

home <- 30
k = 10
index <- as.vector(FNN::knnx.index(df[-home, ], df[home, ], k = k))
knn_homes <- ames_train[c(home, index), ]



knn_homes %>% 
  select(Longitude, Latitude) %>%
  mutate(desc = factor(c('House of interest', rep('Closest neighbors', k)), 
                       levels = c('House of interest', 'Closest neighbors'))) %>%
  qmplot(Longitude, Latitude, data = ., 
         maptype = "toner-background", darken = .7, color = desc, size = I(2.5)) + 
  theme(legend.position = "top",
        legend.title = element_blank())



# 3. プロット作成 ----------------------------------------------


(two_houses <- ames_train[1:2, c("Gr_Liv_Area", "Year_Built")])



# Euclidean
dist(two_houses, method = "euclidean")

# Manhattan
dist(two_houses, method = "manhattan")




p1 <- ggplot(two_houses, aes(Gr_Liv_Area, Year_Built)) +
  geom_point() +
  geom_line(lty = "dashed") +
  ggtitle("(A) Euclidean distance")


p2 <- ggplot(two_houses, aes(Gr_Liv_Area, Year_Built)) +
  geom_point() +
  geom_step(lty = "dashed") +
  ggtitle("(B) Manhattan distance")




gridExtra::grid.arrange(p1, p2, nrow = 1)



home1 <- ames %>%
  mutate(id = row_number()) %>%
  select(Bedroom_AbvGr, Year_Built, id) %>%
  filter(Bedroom_AbvGr == 4 & Year_Built == 2008) %>%
  slice(1) %>%
  mutate(home = "home1") %>%
  select(home, everything())

home2 <- ames %>%
  mutate(id = row_number()) %>%
  select(Bedroom_AbvGr, Year_Built, id) %>%
  filter(Bedroom_AbvGr == 2 & Year_Built == 2008) %>%
  slice(1) %>%
  mutate(home = "home2") %>%
  select(home, everything())

home3 <- ames %>%
  mutate(id = row_number()) %>%
  select(Bedroom_AbvGr, Year_Built, id) %>%
  filter(Bedroom_AbvGr == 3 & Year_Built == 1998) %>%
  slice(1) %>%
  mutate(home = "home3") %>%
  select(home, everything())







features <- c("Bedroom_AbvGr", "Year_Built")

# distance between home 1 and 2
dist(rbind(home1[,features], home2[,features]))


# distance between home 1 and 3
dist(rbind(home1[,features], home3[,features]))

scaled_ames <- recipe(Sale_Price ~ ., ames_train) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep(training = ames, retain = TRUE) %>%
  juice()

home1_std <- scaled_ames %>%
  mutate(id = row_number()) %>%
  filter(id == home1$id) %>%
  select(Bedroom_AbvGr, Year_Built, id) %>%
  mutate(home = "home1") %>%
  select(home, everything())

home2_std <- scaled_ames %>%
  mutate(id = row_number()) %>%
  filter(id == home2$id) %>%
  select(Bedroom_AbvGr, Year_Built, id) %>%
  mutate(home = "home2") %>%
  select(home, everything())

home3_std <- scaled_ames %>%
  mutate(id = row_number()) %>%
  filter(id == home3$id) %>%
  select(Bedroom_AbvGr, Year_Built, id) %>%
  mutate(home = "home3") %>%
  select(home, everything())

home1_std
home2_std
home3_std



# distance between home 1 and 2
dist(rbind(home1_std[,features], home2_std[,features]))


# distance between home 1 and 3
dist(rbind(home1_std[,features], home3_std[,features]))




