# ==========================================================
# 02c_adult_rfe.R   –   RFE + Logit (nested CV)
# ==========================================================
library(tidyverse)
library(tidymodels)
library(caret)
source("R/utils_models.R")
source("R/nested_helpers.R")

# ---------- 1. Charger les données ------------------------
data_lst <- read_rds("results/adult_clean.rds")
train    <- data_lst$train
outcome  <- "income_bin"

# ---------- 2. Nested CV avec build_rfe_logit ------------
metrics_tbl <- run_nested(
  data           = train,
  outcome        = outcome,
  build_inner_fun= build_rfe_logit,
  outer_v        = 3,
  inner_v        = 5
) %>%
  mutate(model = "RFE + Logit")

# ---------- 3. Sauvegarde -------------------------------
write_csv(metrics_tbl, "results/adult_metrics_rfe.csv")
