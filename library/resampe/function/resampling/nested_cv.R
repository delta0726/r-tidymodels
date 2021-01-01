# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/04



# ＜構文＞
# nested_cv(data, outside, inside)



library(tidyverse)
library(rsample)



# データ確認
mtcars %>%
  rownames_to_column() %>%
  as_tibble()


#%% ネストされたリサンプリング -------------------------------------------------

# 関数による実行
mtcars %>%
  nested_cv(outside = vfold_cv(v = 3),
            inside  = bootstraps(times = 5))


# 手動で実行
folds <- mtcars %>% vfold_cv((v = 3))
mtcars %>% nested_cv(folds, inside = bootstraps(times = 5))


# 関数による実行
resample1 <-
  mtcars %>%
    nested_cv(outside = vfold_cv(v = 3),
              inside  = bootstraps(times = 5))



resample1$splits[[1]]


set.seed(2222)
bad_idea <- nested_cv(mtcars,
                      outside = bootstraps(times = 5),
                      inside = vfold_cv(v = 3))#> Warning: Using bootstrapping as the outer resample is dangerous since the inner resample might have the same data point in both the analysis and assessment set.
first_outer_split <- bad_idea$splits[[1]]
outer_analysis <- as.data.frame(first_outer_split)
sum(grepl("Volvo 142E", rownames(outer_analysis)))#> [1] 0
## For the 3-fold CV used inside of each bootstrap, how are the replicated
## `Volvo 142E` data partitioned?
first_inner_split <- bad_idea$inner_resamples[[1]]$splits[[1]]
inner_analysis <- as.data.frame(first_inner_split)
inner_assess   <- as.data.frame(first_inner_split, data = "assessment")

sum(grepl("Volvo 142E", rownames(inner_analysis)))#> [1] 0sum(grepl("Volvo 142E", rownames(inner_assess)))#> [1] 0