# ==========================================================
# 02d_adult_ga.R –  GA + Logit (wrapper non-greedy)
# ==========================================================
library(tidyverse)
library(tidymodels)
library(future)
source("R/utils_models.R")
source("R/nested_helpers.R")

# ---------- 1. Données ------------------------------------
data_lst <- read_rds("results/adult_clean.rds")
train    <- data_lst$train
outcome  <- "income_bin"

# ---------- 2. Plan de parallélisme ------------------------
plan(multisession, workers = 8)
options(future.globals.maxSize = 8 * 1024^3)

# ---------- 3. Nested CV ----------------------------------
metrics_tbl <- run_nested(
  data           = train,
  outcome        = outcome,
  build_inner_fun= build_ga_logit,
  outer_v        = 3,
  inner_v        = 1
) %>% mutate(model = "GA + Logit")

# ---------- 4. Sauvegarde ---------------------------------
write_csv(metrics_tbl, "results/adult_metrics_ga.csv")