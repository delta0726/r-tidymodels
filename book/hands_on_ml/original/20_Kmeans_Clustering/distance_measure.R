# ********************************************************************************
# Title   : k平均法クラスタリング
# Theme   : 距離測度
# Chapter : 20
# URL     : https://bradleyboehmke.github.io/HOML/kmeans.html
# Support : https://koalaverse.github.io/homlr/notebooks/20-kmeans.nb.html
# H2O　   : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/k-means.html
# ********************************************************************************


library(tidyverse)
library(tidyquant)
library(magrittr)
library(stringr)
library(Hmisc)
library(kableExtra)
library(cluster)
library(factoextra)



# generate data
corr_ex <- tibble(
  v = 1:20,
  obs_1 = sample(5:7, 20, replace = TRUE),
  obs_2 = sample(4:10, 20, replace = TRUE)
) %>%
  mutate(obs_3 = obs_2 * 2 + sample(0:1, 1))

corr_ex %>%
  gather(Observation, value, obs_1:obs_3) %>%
  ggplot(aes(v, value, color = Observation)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_x_continuous("Variable index") +
  scale_y_continuous("Some arbitrary measure")




# Generate data
create_data <- function(sd) {
  data_frame(
    x1 = c(rnorm(100, sd = sd), rnorm(100, sd = sd) + 3),
    x2 = c(rnorm(100, sd = sd), rnorm(100, sd = sd) - 2)
  ) %>%
    mutate(`W(Ck)` = case_when(
      sd == 0.5  ~ "Best",
      sd == 0.75 ~ "Better",
      sd == 1   ~ "Good"
    ))
}
df <- map(c(0.5, 0.75, 1), create_data)

# Compute and add cluster info to data
k2 <- map(df, ~ kmeans(.x[, 1:2], 2, nstart = 20))
df <- map2(df, k2, ~ mutate(.x, cluster = .y$cluster)) %>%
  map2_dfr(k2, ~ inner_join(.x, .y$centers %>% 
                              as.data.frame() %>% 
                              mutate(cluster = row_number()), by = "cluster")
  ) %>%
  rename(x1 = x1.x, x2 = x2.x, x_center = x1.y, y_center = x2.y) %>%
  mutate(`W(Ck)` = factor(`W(Ck)`, levels = c("Good", "Better", "Best")))

# Plot results
df %>%
  ggplot(aes(colour = factor(cluster))) +
  facet_wrap(~ `W(Ck)`) +
  geom_segment(aes(x = x1, xend = x_center, y = x2, yend = y_center), lty = "dashed", alpha = .5) +
  geom_point(aes(x_center, y_center), size = 4) +
  geom_point(aes(x1, x2), show.legend = FALSE, alpha = .5) +
  scale_x_continuous(bquote(X[1]), breaks = NULL, labels = NULL) +
  scale_y_continuous(bquote(X[2]), breaks = NULL, labels = NULL) +
  theme(legend.position = "none")



# Generate data
set.seed(111)
obj <- mlbench::mlbench.spirals(200, 1, 0.025)
df <- data.frame(
  x = obj$x[, 1],
  y = obj$x[, 2],
  class = obj$classes
)

# Plot data
p1 <- ggplot(df, aes(x, y)) +
  geom_point() +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle('(A) Original spiral data')

# Run k-means
kmeans_on_spiral <- kmeans(df[, 1:2], 2)
df$kmeans_clusters <- kmeans_on_spiral$cluster
p2 <- ggplot(df, aes(x, y, color = kmeans_clusters)) +
  geom_point(show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle('(B) k-means clusters')

# Plot results
sc <- kernlab::specc(as.matrix(df[, 1:2]), centers = 2)
df$spec_clusters <- sc@.Data
p3 <- ggplot(df, aes(x, y, color = spec_clusters)) +
  geom_point(show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle('(C) Spectral clusters')

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


