# 02f_diabetes_cart.R  -------------------------------------------------
library(tidyverse)
library(tidymodels)
library(future)
source("R/utils_models.R")
source("R/nested_helpers.R")

data_lst <- read_rds("results/diabetes_clean.rds")
train    <- data_lst$train
outcome  <- "readmitted_bin"

# -------- builder CART -----------------------------------------------
build_cart <- function(analysis, outcome,
                       inner_v = 5,
                       grid_size = 15) {
  
  ## 1. recipe pour sécuriser les noms de variables
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = analysis) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  ## 2. spécification du modèle
  spec <- decision_tree(
    cost_complexity = tune(),
    min_n           = tune()
  ) %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
  
  ## 3. CV interne + grille
  inner_cv <- vfold_cv(analysis, v = inner_v, strata = !!sym(outcome))
  
  grid <- grid_latin_hypercube(
    cost_complexity(),
    min_n(),
    size = grid_size
  )
  
  tuned <- time_it(
    tune_grid(
      wf, resamples = inner_cv, grid = grid,
      metrics  = metric_set(roc_auc),
      control  = control_grid(save_pred = FALSE, verbose = FALSE)
    )
  )
  
  best_params <- select_best(tuned, metric = "roc_auc")
  final_wf    <- finalize_workflow(wf, best_params) %>% fit(analysis)
  
  # nb de variables effectivement utilisées
  vars_used <- final_wf$fit$fit$fit$frame$var
  nvar      <- length(unique(vars_used)) - 1
  
  list(
    wf     = final_wf,
    n_vars = nvar,
    time   = attr(tuned, "time")
  )
}

# -------- nested CV ---------------------------------------------------
plan(sequential)

metrics_tbl <- run_nested(
  data            = train,
  outcome         = outcome,
  build_inner_fun = build_cart,
  outer_v         = 3,
  inner_v         = 5
) %>%
  mutate(model = "CART", dataset = "Diabetes")

write_csv(metrics_tbl, "results/diabetes_metrics_cart.csv")
