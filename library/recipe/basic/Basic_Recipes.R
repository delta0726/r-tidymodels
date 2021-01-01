# Title     : TODO
# Objective : TODO
# Created by: Owner
# Created on: 2020/06/01


library(recipes)
library(rsample)
library(modeldata)
library(Hmisc)

data("credit_data")

set.seed(55)


#%% Step1: 準備 ------------------------------------------

# データ確認
credit_data %>% glimpse()

# データ分割
train_test_split <- credit_data %>% initial_split()
credit_train     <- train_test_split %>% training()
credit_test      <- train_test_split %>% testing()

# 欠損値確認
credit_train %>% vapply(function(x) mean(!is.na(x)), numeric(1))


# 最初のレシピ
# --- フォーミュラとデータを定義
# --- データは訓練データである必要はない（ヘッダーがなどが同一であればよい）
rec_obj <- recipe(Status ~ ., data = credit_train)
rec_obj



#%% Step2: k近傍法 ------------------------------------------

# レシピ追加
# --- k近傍法(デフォルトはk=5)
# --- すべてのpredictorに適用
imputed <-
  rec_obj %>%
    step_knnimpute(all_predictors())

# 確認
imputed %>% print()


#%% Step3: ダミー変換 ------------------------------------------

# レシピ追加
# --- 因子データのダミー変換
ind_vars <-
  imputed %>%
    step_dummy(all_predictors(), -all_numeric())

# 確認
ind_vars %>% print()



#%% Step4: ダミー変換 ------------------------------------------

# レシピ追加
standardized <-
  ind_vars %>%
    step_center(all_predictors())  %>%
    step_scale(all_predictors())

# 確認
standardized %>% print()



#%% レシピ適用の準備 ------------------------------------------

# レシピを新たなデータセットに適用
trained_rec <- standardized %>% prep(training = credit_train)
trained_rec %>% print()

# データ比較
standardized %>% names()
trained_rec %>% names()

# 追加されたデータ
trained_rec$tr_info
trained_rec$orig_lvls
trained_rec$last_term_info



#%% 実際のデータへの適用 ------------------------------------------

# データへの適用
train_data <- trained_rec %>% bake(new_data = credit_train)
test_data  <- trained_rec %>% bake(new_data = credit_test)

