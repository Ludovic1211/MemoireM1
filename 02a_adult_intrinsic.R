# ==========================================================
# 02a_adult_intrinsic.R   –   Logit (baseline) & CART
# ==========================================================
library(tidyverse)
library(tidymodels)
library(future)
source("R/utils_models.R")
source("R/nested_helpers.R")

data_lst <- read_rds("results/adult_clean.rds")
train    <- data_lst$train
outcome  <- "income_bin"

## ---------- 1. Logit baseline ---------------
build_logit <- function(analysis, outcome, inner_v = 1) {
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = analysis) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  wf  <- workflow() %>%
    add_recipe(rec) %>%
    add_model(logistic_reg() %>% set_engine("glm"))
  
  fit_wf <- time_it(fit(wf, data = analysis))
  list(wf = fit_wf, n_vars = NA, time = attr(fit_wf, "time"))
}

## ---------- 2′. CART sans tuning ----------------------------
build_cart <- function(analysis, outcome, inner_v = 1) {
  spec <- decision_tree() %>%              # pas de tune()
    set_engine("rpart") %>% 
    set_mode("classification")
  
  wf   <- workflow() %>% add_model(spec) %>% 
    add_formula(as.formula(paste(outcome, "~ .")))
  
  fit_wf <- time_it(fit(wf, data = analysis))
  
  nvar <- length(unique(fit_wf$fit$fit$frame$var)) - 1
  list(wf = fit_wf, n_vars = nvar, time = attr(fit_wf, "time"))
}


## ---------- 3. Exécution nested ----------
plan(sequential)

logit_tbl <- run_nested(train, outcome, build_logit, outer_v = 3, inner_v = 5) %>%
  mutate(model = "Logit (baseline)")

cart_tbl  <- run_nested(train, outcome, build_cart , outer_v = 3, inner_v = 5) %>% 
  mutate(model = "CART (default)")


metrics_tbl <- bind_rows(logit_tbl, cart_tbl)
write_csv(metrics_tbl, "results/adult_metrics_intrinsic.csv")