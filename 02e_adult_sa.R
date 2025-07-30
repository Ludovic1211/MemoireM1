# ===============================================================
# 02e_adult_sa.R  –  Simulated-Annealing Feature Selection + Logit
# ===============================================================

library(tidyverse)
library(tidymodels)
library(caret)
library(future)
source("R/utils_models.R")
source("R/nested_helpers.R")

# ---------- 1. Jeu de données ----------------------------------
data_lst <- read_rds("results/adult_clean.rds")
train    <- data_lst$train
outcome  <- "income_bin"

# ---------- 2. Fonction interne : SA + Logit -------------------
build_sa_logit_adult <- function(analysis, outcome,
                                 iters = 200) {
  
  # 1. recipe complet ------------------------------
  rec <- recipe(as.formula(paste(outcome, "~ .")), data = analysis) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
  
  # 2. métrique = ROC binomiale --------------------
  two_stats <- function(dat, lev = levels(dat$obs), model = NULL) {
    twoClassSummary(dat, lev, model)
  }
  
  sa_fun <- caretSA          # liste par défaut
  sa_fun$fitness_extern <- two_stats   # juste AUC
  
  # 3. contrôles -----------------------------------
  ctrl_inner <- trainControl(
    method      = "LGOCV",   # 90/10 split interne
    p           = 0.90,
    classProbs  = TRUE,
    summaryFunction = two_stats,
    allowParallel  = FALSE
  )
  
  ctrl_sa <- safsControl(
    functions = sa_fun,
    method    = "cv",
    verbose   = FALSE,
    metric    = c(internal = "ROC", external = "ROC"),
    maximize  = c(internal = TRUE,  external = TRUE),
    improve   = 10
  )
  
  
  # 4. appel SA ------------------------------------
  sa_fit <- time_it(
    safs(
      rec,
      data         = analysis,
      iters        = iters,
      method       = "glm",
      family       = binomial,
      metric       = "ROC",
      trControl    = ctrl_inner,
      safsControl  = ctrl_sa
    )
  )
  
  # 5. variables retenues + refit workflow ---------
  sel_vars <- sa_fit$optVariables
  
  rec_final <- recipe(
    as.formula(paste(outcome, "~", paste(sel_vars, collapse = "+"))),
    data = analysis
  ) %>%                       # on repart de zéro
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  
  wf <- workflow() %>% 
    add_recipe(rec_final) %>% 
    add_model(logistic_reg() %>% set_engine("glm")) %>% 
    fit(analysis)
  
  list(
    wf     = wf,
    n_vars = length(sel_vars),
    time   = attr(sa_fit, "time")
  )
}



plan(multisession, workers = 8)

metrics_tbl <- run_nested(
  data            = train,
  outcome         = outcome,
  build_inner_fun = build_sa_logit_adult,
  outer_v         = 3,
  inner_v         = 1
) %>% 
  mutate(model = "SA + Logit")

write_csv(metrics_tbl, "results/adult_metrics_sa.csv")


# ---------- 5. Sauvegarde --------------------------------------
write_csv(metrics_tbl, "results/adult_metrics_sa.csv")