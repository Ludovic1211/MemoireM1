# =============================================
# 01_diabetes_prep.R
# Nettoyage & split – Diabetes
# =============================================
library(tidyverse)
library(tidymodels)

# 1. Lecture ---------------------------------------------------------
diab_raw <- read_csv("data/diabetes_raw.csv", na = c("?", "NA", ""))

# 2. Recodage cible --------------------------------------------------
diab_raw <- diab_raw %>%
  mutate(readmitted_bin = factor(if_else(readmitted == "<30", "yes", "no"),
                                 levels = c("no", "yes")))


# 3. Recette ---------------------------------------------------------
diab_rec <- recipe(readmitted_bin ~ ., data = diab_raw,
                   strings_as_factors = FALSE) %>%
  step_rm(readmitted) %>%                                  # retire l’ancienne cible
  update_role(encounter_id, patient_nbr, new_role = "ID") %>%
  step_rm(encounter_id, patient_nbr) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# 4. Split stratifié 70/30 ------------------------------------------
set.seed(42)
diab_split <- initial_split(diab_raw, prop = 0.7, strata = readmitted_bin)
diab_train <- training(diab_split)
diab_test  <- testing(diab_split)

# 5. Préparation finale ---------------------------------------------
diab_prep <- diab_rec %>% prep(verbose = TRUE)
train_processed <- juice(diab_prep)
test_processed  <- bake(diab_prep, new_data = diab_test)

# 6. Sauvegarde ------------------------------------------------------
write_rds(list(train = train_processed,
               test  = test_processed,
               prep  = diab_prep),
          "results/diabetes_clean.rds")

