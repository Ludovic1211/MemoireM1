# =========================================
# 01_adult_prep.R
# Nettoyage & split – Adult Income
# =========================================
library(tidyverse)
library(tidymodels)

# 1. Lecture ---------------------------------------------------------
cols <- c("age","workclass","fnlwgt","education","education_num",
          "marital_status","occupation","relationship","race","sex",
          "capital_gain","capital_loss","hours_per_week","native_country",
          "income")

adult_train <- read_csv("data/adult_raw.csv",  col_names = cols, na = c("?", ""))
adult_test  <- read_csv("data/adult_test_raw.csv", col_names = cols,
                        skip = 1, na = c("?", ""))

adult_raw <- bind_rows(adult_train, adult_test)

# 2. Recodage cible --------------------------------------------------
adult_raw <- adult_raw %>%
  mutate(income_bin = income %>% str_trim() %>% str_remove("\\.") %>%
           factor(levels = c("<=50K", ">50K"),
                  labels = c("no", "yes")))

# 3. Recette (avec Yeo-Johnson) --------------------------------------
adult_rec <- recipe(income_bin ~ ., data = adult_raw,
                    strings_as_factors = FALSE) %>%
  step_rm(income) %>%                         # retire ancienne cible
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# 4. Split 70/30 stratifié ------------------------------------------
set.seed(42)
adult_split <- initial_split(adult_raw, prop = 0.7, strata = income_bin)
adult_train <- training(adult_split)
adult_test  <- testing(adult_split)

adult_prep <- adult_rec %>% prep(verbose = TRUE)
train_processed <- juice(adult_prep)
test_processed  <- bake(adult_prep, new_data = adult_test)

# 5. Sauvegarde ------------------------------------------------------
write_rds(list(train = train_processed,
               test  = test_processed,
               prep  = adult_prep),
          "results/adult_clean.rds")
