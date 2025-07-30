# ==========================================================
# utils_models.R
# ==========================================================

library(tidyverse)
library(tidymodels)
library(vip)

# ---------- Chronometre ------------------
time_it <- function(expr) {
  start <- Sys.time()
  res   <- eval(expr)
  attr(res, "time") <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  res
}

# ---------- Évaluer un workflow sur un jeu test -----------
score_model <- function(fitted_wf, test_data, outcome) {
  probs <- predict(fitted_wf, test_data, type = "prob")[[".pred_yes"]]
  preds <- predict(fitted_wf, test_data)[[".pred_class"]]
  tibble(
    roc_auc  = roc_auc_vec(test_data[[outcome]], probs),
    accuracy = accuracy_vec(test_data[[outcome]], preds)
  )
}

# ---------- Enregistrer / accumuler les métriques ---------
append_metrics <- function(tbl, path) {
  if (file.exists(path)) {
    old <- read_csv(path, show_col_types = FALSE)
    write_csv(bind_rows(old, tbl), path)
  } else {
    write_csv(tbl, path)
  }
}
