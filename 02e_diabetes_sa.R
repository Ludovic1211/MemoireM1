# ==========================================================
# 02e_diabetes_sa.R – SA + Logit (wrapper non-greedy)
# ==========================================================
library(tidyverse)
library(tidymodels)
library(caret)
library(future)
library(themis)
source("R/utils_models.R")
source("R/nested_helpers.R")

# ---------- données -----------------
data_lst <- read_rds("results/diabetes_clean.rds")
train    <- data_lst$train
outcome  <- "readmitted_bin"

# ------------------------------------------------------------------
# SA + Logit
# ------------------------------------------------------------------
build_sa_logit <- function(analysis, outcome,
                           iters   = 200,
                           inner_v = 5,
                           start_p = 0.90) {
  
  ## 1. Recette commune
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = analysis) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors())
  
  ## 2. Métriques
  many_stats <- function(dat, lev = levels(dat$obs), model = NULL) {
    c(twoClassSummary(dat, lev, model), prSummary(dat, lev, model))
  }
  
  ## 3. Fonctions SA
  sa_fun <- caretSA
  sa_fun$fitness_extern <- many_stats
  sa_fun$initial <- function(vars, prob = start_p, ...)
    sort(sample.int(vars, size = floor(vars * prob) + 1))
  
  ## 4. Contrôles
  ctrl_inner <- trainControl(
    method = "LGOCV", p = 0.90, number = 1,
    classProbs = TRUE, summaryFunction = many_stats,
    allowParallel = FALSE
  )
  
  ctrl_sa <- safsControl(
    functions = sa_fun,
    method    = "cv",
    number    = inner_v,
    metric    = c(internal = "ROC", external = "ROC"),
    maximize  = c(internal = TRUE,  external = TRUE),
    improve   = 10,
    verbose   = FALSE
  )
  
  ## 5. Recherche SA
  sa_fit <- time_it(
    safs(
      rec,
      data        = analysis,
      iters       = iters,
      method      = "glm",
      family      = binomial,
      metric      = "ROC",
      trControl   = ctrl_inner,
      safsControl = ctrl_sa
    )
  )
  
  
  ## 6. Refit final
  sel_vars  <- sa_fit$optVariables
  rec_final <- rec %>%
    step_select(all_outcomes(), all_of(sel_vars), skip = TRUE)  # 👈
  
  wf <- workflow() %>%
    add_recipe(rec_final) %>%
    add_model(logistic_reg() %>% set_engine("glm")) %>%
    fit(analysis)
  
  
  list(wf     = wf,
       n_vars = length(sel_vars),
       time   = attr(sa_fit, "time"))
}








# ---------- plan / limites mémoire ----
plan(multisession, workers = 8)
options(future.globals.maxSize = 8 * 1024^3)   # 8 Go

# ---------- nested CV ------------------
plan(multisession, workers = 8)

metrics_sa <- run_nested(
  data            = train,
  outcome         = "readmitted_bin",
  build_inner_fun = build_sa_logit,
  outer_v         = 3,
  inner_v         = 5
) 
metrics_sa %>%
  mutate(
    model   = "SA + Logit",
    dataset = "Diabetes"
  )

print(metrics_sa)
write_csv(metrics_sa, "results/diabetes_metrics_sa.csv")

