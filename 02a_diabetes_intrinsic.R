# ==========================================================
# 02a_diabetes_intrinsic.R   –   Logit (baseline) & CART
# ==========================================================
library(tidyverse)
library(tidymodels)
library(future)
source("R/utils_models.R")
source("R/nested_helpers.R")

# 02a_diabetes_intrinsic.R – Logit baseline
library(tidyverse)
library(tidymodels)
library(future)
source("R/utils_models.R")
source("R/nested_helpers.R")

data_lst <- read_rds("results/diabetes_clean.rds")
train    <- data_lst$train
outcome  <- "readmitted_bin"

build_logit <- function(analysis, outcome, inner_v = 1) {
  wf <- workflow() %>% 
    add_formula(as.formula(paste(outcome, "~ ."))) %>%   # plus de recipe
    add_model(logistic_reg() %>% set_engine("glm"))
  
  fit_wf <- time_it(fit(wf, data = analysis))
  
  list(
    wf     = fit_wf,
    n_vars = NA,
    time   = attr(fit_wf, "time")
  )
}


plan(sequential)
logit_tbl <- run_nested(train, outcome, build_logit, outer_v = 3, inner_v = 1) %>%
  mutate(model = "Logit (baseline)")

write_csv(logit_tbl, "results/diabetes_metrics_intrinsic.csv")