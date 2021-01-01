# ******************************************************************************
# Title     : Using Generalized Linear Models
# Objective : TODO
# Created by: Owner
# Created on: 2020/11/25
# URL       : https://embed.tidymodels.org/articles/Applications/GLM.html
# ******************************************************************************


# ＜ポイント＞
# - 般化線形モデルを用いて因子特徴量(X)の各レベルが結果(Y)に与える影響を推定
# - これらの値は、因子レベルの新しいエンコーディングとして機能するために保持される
#   --- 尤度エンコーディング（プーリングあり/なし）


# ＜ポイント＞
# 1 準備
# 2 データ集計
# 3 プーリングなし
# 4 部分的プーリング



# 1 準備 -------------------------------------------------------------------------

library(tidymodels)
library(embed)
library(modeldata)


# データロード
data(okc)


# データ確認
okc %>% print()
okc %>% glimpse()




# 2 データ集計 -------------------------------------------------------------------------

# データ集計
# --- 地域別のstemの数を集計
# --- stemの割合
# --- stemの尤度
props <-
  okc %>%
    group_by(location) %>%
    summarise(prop = mean(Class == "stem"),
              log_odds  = log(prop/(1-prop)),
              n = length(Class)) %>%
    mutate(label = paste0(gsub("_", " ", location), " (n=", n, ")"))


# データ確認
props %>% select(-label)



# 3 プーリングなし -------------------------------------------------------------

# レシピ作成
okc_glm <-
  recipe(Class ~ ., data = okc) %>%
  step_lencode_glm(location, outcome = vars(Class)) %>%
  prep()


# エンコーディング確認
# --- 因子レベルごとに数値が割り当てられている
glm_estimates <-
  okc_glm %>%
    tidy(number = 1) %>%
    select(-terms, -id)


# 因子レベルの確認
glm_estimates


# データ結合
glm_estimates <-
  glm_estimates%>%
    set_names(c("location", "glm")) %>%
    inner_join(props, by = "location")


# 計算証明
# --- glmとlod_oddsが一致
glm_estimates %>%
  dplyr::filter(is.finite(log_odds)) %>%
  mutate(difference = log_odds - glm) %>%
  dplyr::select(difference) %>%
  summary()


# 未知のラベルに対する対応
okc_glm %>%
  tidy(number = 1) %>%
  dplyr::filter(level == "..new") %>%
  select(-id)



# 4 部分的プーリング -------------------------------------------------------------

# ＜ポイント＞
# - 階層ベイズ一般化線形モデルを使用して、すべての場所を一度に使用することで効果を推定
# - 部分プーリングは、対数オッズの個別の経験的推定と事前分布の組み合わせとして各効果を推定
#  -サンプルサイズが小さい場所の場合、最終的な推定値は対数オッズの全体的な平均に向かって縮小される



# レシピ作成
okc_glmer <-
  recipe(Class ~ ., data = okc) %>%
  step_lencode_bayes(location, outcome = vars(Class),
                     options = list(chains = 4, cores = 4, iter = 500, seed = 8779)) %>%
  prep()


all_estimates <-
  okc_glmer %>%
    tidy(number = 1) %>%
    dplyr::select(-terms, -id) %>%
    set_names(c("location", "glmer")) %>%
    inner_join(glm_estimates, by = "location")


all_estimates %>% dplyr::select(location, log_odds, glm, glmer)


theme_set(theme_bw() + theme(legend.position = "top"))

pooled_plot <-
  all_estimates %>%
  dplyr::filter(is.finite(log_odds)) %>%
  ggplot(aes(x = log_odds, y = glmer)) +
  geom_abline(col = "red", alpha = .5) +
  geom_point_interactive(aes(size = sqrt(n), tooltip = label), alpha = .5) +
  xlim(rng) + ylim(rng)

# Convert the plot to a format that the html file can handle
ggiraph(ggobj = pooled_plot)




okc_mixed <-
  recipe(Class ~ ., data = okc) %>%
  step_lencode_mixed(
    location,
    outcome = vars(Class),
  ) %>%
  prep(training = okc)

all_estimates <-
  tidy(okc_mixed, number = 1) %>%
  dplyr::select(-terms, -id) %>%
  set_names(c("location", "mixed")) %>%
    inner_join(all_estimates, by = "location")
all_estimates %>%
  dplyr::select(location, log_odds, glm, glmer, mixed)



mixed_plot <-
  all_estimates %>%
  dplyr::filter(is.finite(log_odds)) %>%
  ggplot(aes(x = log_odds, y = mixed)) +
  geom_abline(col = "red", alpha = .5) +
  geom_point_interactive(aes(size = sqrt(n), tooltip = label), alpha = .5) +
  xlim(rng) + ylim(rng)

ggiraph(ggobj = mixed_plot)