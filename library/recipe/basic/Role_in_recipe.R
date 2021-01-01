# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/01

# Roles in Recipes
# https://cran.r-project.org/web/packages/recipes/vignettes/Roles.html


# ＜roleについて＞
# ・recipeではデータの各列に1つ以上の役割を割り当てることができる
# ・典型的なroleは“predictor”と“outcome”でフォーミュラを定義すると設定される
# ・roleを割り当てることで"step_**"で一括処理ができるようになる


library(recipes)
library(modeldata)


# データ準備
data(biomass)



#%% フォーミュラ定義時に自動設定 ---------------------------------------

# 典型的なroll
# --- フォーミュラ定義時に“predictor”と“outcome”が定義される
recipe(Species ~ ., data = iris) %>% summary()


# Outcomeは1つに制限されない
# ---
recipe(Sepal.Length + Sepal.Width ~ ., data = iris) %>% summary()



#%% rollのアップデート ---------------------------------------

# データ概要
biomass %>% glimpse()

# レシピ作成
recipe(HHV ~ ., data = biomass) %>%
  summary()

# roleのアップデート
# --- 列を指定してroleを更新
recipe(HHV ~ ., data = biomass) %>%
  update_role(dataset, new_role = "dataset split variable") %>%
  update_role(sample, new_role = "sample ID") %>%
  summary()


#%% rollの削除 ---------------------------------------

# データ概要
biomass %>% glimpse()

# レシピ作成
recipe(HHV ~ ., data = biomass) %>%
  summary()

# roleの削除
# --- 手動でのroleの削除はできない
recipe(HHV ~ ., data = biomass) %>%
  remove_role(sample, old_role = "predictor") %>%
  summary()


#%% rollの追加 ---------------------------------------

# データ概要
biomass %>% glimpse()

# レシピ作成
recipe(HHV ~ ., data = biomass) %>%
  summary()

# 1つの列に2つのrollを割り当てることも可能
recipe(HHV ~ ., data = biomass) %>%
  update_role(sample, new_role = "sample ID") %>%
  add_role(sample, new_role = "jellyfish") %>%
  summary()

# rollが複数の場合のrollの更新
recipe(HHV ~ ., data = biomass) %>%
  update_role(sample, new_role = "sample ID") %>%
  add_role(sample, new_role = "jellyfish") %>%
  update_role(sample, new_role = "flounder", old_role = "jellyfish") %>%
  summary()



#%% rollを活用した前処理 ---------------------------------------

# step関数の適用対象をroleで指定できる
recipe(HHV ~ ., data = biomass) %>%
  update_role(dataset, new_role = "dataset split variable") %>%
  update_role(sample, new_role = "sample ID") %>%
  add_role(sample, new_role = "jellyfish") %>%
  add_role(HHV, new_role = "nocenter") %>%
  step_center(all_predictors(), -has_role("nocenter")) %>%
  prep() %>%
  juice() %>%
  head()



#%% rollなしでレシピを開始 ---------------------------------------

# rollなしのレシピ
recipe(biomass) %>% summary()

# rollの更新
recipe(biomass) %>%
  update_role(contains("gen"), new_role = "lunchroom") %>%
  update_role(sample, HHV, new_role = "snail") %>%
  summary()


