# ********************************************************************************
# Title   : 一般化低ランクモデル(GLRM)
# Theme   : 
# Chapter : 18
# URL     : https://bradleyboehmke.github.io/HOML/GLRM.html
# Support : https://koalaverse.github.io/homlr/notebooks/18-glrm.nb.html
# H2O　   : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glrm.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(broom)
library(magrittr)
library(h2o)
library(Hmisc)



# 0. ポイント整理 ----------------------------------------------

# ＜前提＞
#・PCAなどのよく知られている行列分解法の拡張
#・任意の数の欠損値を持つ数値/カテゴリ/順序/ブールの混合データを処理
#・次元圧縮だけでなく、クラスタリング/欠損値代入/計算メモリ削減などにも使える



# 1. データ準備 ----------------------------------------------


# データ取得
my_basket <- read_csv("data/my_basket.csv")

# データ確認
my_basket %>% dim()




# 2.H2OによるGLRM ----------------------------------------------

# H2Oの起動
h2o.no_progress()
h2o.init(max_mem_size = "5g")


# H2Oフレームに変換
my_basket.h2o <- my_basket %>% as.h2o()


# run basic GLRM
basic_glrm <- 
  my_basket.h2o %>% 
    h2o.glrm(k = 20, 
             loss = "Quadratic",
             regularization_x = "None", 
             regularization_y = "None", 
             transform = "STANDARDIZE", 
             max_iterations = 2000,
             seed = 123)


# サマリー
basic_glrm %>% summary()


# プロット
# --- オブジェクトの圧縮速度
basic_glrm %>% plot()



# 4.プロット -------------------------------------------------------------


# 主成分の確認
basic_glrm@model$importance


# 
data.frame(
  PC  = basic_glrm@model$importance %>% seq_along(),
  PVE = basic_glrm@model$importance %>% .[2,] %>% unlist(),
  CVE = basic_glrm@model$importance %>% .[3,] %>% unlist()
  ) %>%
  gather(metric, variance_explained, -PC) %>%
  ggplot(aes(PC, variance_explained)) +
  geom_point() +
  facet_wrap(~ metric, ncol = 1, scales = "free")



t(basic_glrm@model$archetypes)[1:5, 1:5]

p1 <- t(basic_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, reorder(feature, Arch1))) +
  geom_point()

p2 <- t(basic_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, Arch2, label = feature)) +
  geom_text()

gridExtra::grid.arrange(p1, p2, nrow = 1)






# 4.GLRMの最適なkを探す ----------------------------------------------

# Re-run model with k = 8
k8_glrm <- 
  my_basket.h2o %>% 
    h2o.glrm(k = 8, 
             loss = "Quadratic",
             regularization_x = "None", 
             regularization_y = "None", 
             transform = "STANDARDIZE", 
             max_iterations = 2000,
             seed = 123)

# Reconstruct to see how well the model did
my_reconstruction <- 
  k8_glrm %>% 
    h2o.reconstruct(my_basket.h2o, reverse_transform = TRUE)

# Raw predicted values
my_reconstruction[1:5, 1:5]


# Round values to whole integers
my_reconstruction[1:5, 1:5] %>% round(0)


# Use non-negative regularization
k8_glrm_regularized <- 
  my_basket.h2o %>% 
    h2o.glrm(k = 8, 
             loss = "Quadratic",
             regularization_x = "NonNegative", 
             regularization_y = "NonNegative",
             gamma_x = 0.5,
             gamma_y = 0.5,
             transform = "STANDARDIZE", 
             max_iterations = 2000,
             seed = 123
             )

# Show predicted values
k8_glrm_regularized %>% predict(my_basket.h2o)[1:5, 1:5]


# Compare regularized versus non-regularized loss
par(mfrow = c(1, 2))
plot(k8_glrm)
plot(k8_glrm_regularized)



# Split data into train & validation
split <- h2o.splitFrame(my_basket.h2o, ratios = 0.75, seed = 123)
train <- split[[1]]
valid <- split[[2]]

# Create hyperparameter search grid
params <- expand.grid(
  regularization_x = c("None", "NonNegative", "L1"),
  regularization_y = c("None", "NonNegative", "L1"),
  gamma_x = seq(0, 1, by = .25),
  gamma_y = seq(0, 1, by = .25),
  error = 0,
  stringsAsFactors = FALSE
)

# Perform grid search
for(i in seq_len(nrow(params))) {
  
  # Create model
  glrm_model <- h2o.glrm(
    training_frame = train,
    k = 8, 
    loss = "Quadratic",
    regularization_x = params$regularization_x[i], 
    regularization_y = params$regularization_y[i],
    gamma_x = params$gamma_x[i],
    gamma_y = params$gamma_y[i],
    transform = "STANDARDIZE", 
    max_runtime_secs = 1000,
    seed = 123
  )
  
  # Predict on validation set and extract error
  validate <- h2o.performance(glrm_model, valid)
  params$error[i] <- validate@metrics$numerr
}

# Look at the top 10 models with the lowest error rate
params %>%
  arrange(error) %>%
  head(10)


# Apply final model with optimal hyperparamters
final_glrm_model <- h2o.glrm(
  training_frame = my_basket.h2o,
  k = 8, 
  loss = "Quadratic",
  regularization_x = "L1", 
  regularization_y = "NonNegative",
  gamma_x = 1,
  gamma_y = 0.25,
  transform = "STANDARDIZE", 
  max_iterations = 2000,
  seed = 123
)

# New observations to score
new_observations <- as.h2o(sample_n(my_basket, 2))

# Basic scoring
predict(final_glrm_model, new_observations) %>% round(0)


