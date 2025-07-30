# ==========================================================
# 02b_diabetes_filter.R   –   Filter IG + Logit (nested CV)
#   (logistique classique, sans tuning interne)
# ==========================================================

library(tidyverse)
library(tidymodels)
library(FSelectorRcpp)
source("R/utils_models.R")
source("R/nested_helpers.R")

# ---------- 1. Charger le jeu nettoyé ---------------------
data_lst <- read_rds("results/diabetes_clean.rds")
train    <- data_lst$train
outcome  <- "readmitted_bin"

# ---------- 2. Fonction inner (Information Gain + Logit) --
build_filter_logit <- function(analysis, outcome,
                               top_k = 20, apply_fdr = FALSE) {
  
  ig_tbl <- FSelectorRcpp::information_gain(
    as.formula(paste(outcome, "~ .")), data = analysis)
  
  ig_tbl <- if (apply_fdr) {
    ig_tbl %>%
      mutate(p_adj = p.adjust(p.value, method = "BH")) %>%
      filter(p_adj < 0.05)
  } else {
    ig_tbl %>%
      arrange(desc(importance)) %>%
      slice_head(n = top_k)
  }
  
  sel_vars <- ig_tbl$attributes
  
  rec <- recipe(as.formula(
    paste(outcome, "~", paste(sel_vars, collapse = "+"))),
    data = analysis) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  
  wf <- workflow() %>%
    add_model(logistic_reg() %>% set_engine("glm")) %>%
    add_recipe(rec)
  
  fit_wf <- time_it(fit(wf, data = analysis))
  
  list(
    wf     = fit_wf,
    n_vars = length(sel_vars),
    time   = attr(fit_wf, "time")
  )
}

# ---------- 3. Lancer le nested CV ------------------------
metrics_tbl <- run_nested(
  data           = train,
  outcome        = outcome,
  build_inner_fun= build_filter_logit,
  outer_v        = 3,
  inner_v        = 5
) %>%
  mutate(model = "Filter IG + Logit")

# ---------- 4. Sauvegarde ---------------------------------
write_csv(metrics_tbl, "results/diabetes_metrics_filter.csv")