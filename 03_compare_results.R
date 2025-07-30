# ==============================================================
# 03_compare_results.R – Fusion et comparaisons finales
# ==============================================================

library(tidyverse)

read_metrics <- function(path, dataset) {
  
  tbl <- read_csv(path, show_col_types = FALSE) |>
    mutate(dataset = dataset)
  
  
  if (!"model" %in% names(tbl)) {
    mdl <- basename(path) |>
      str_remove("_metrics_.*") |>
      str_remove("_caret")
    tbl <- mutate(tbl, model = mdl)
  }
  
  tbl
}

all_files <- list.files("results", pattern = "_metrics_.*\\.csv$", full.names = TRUE)

metrics_tbl <- map_dfr(all_files, function(f) {
  if (str_starts(basename(f), "adult")) {
    read_metrics(f, "Adult")
  } else {
    read_metrics(f, "Diabetes")
  }
})

write_csv(metrics_tbl, "results/metrics_all.csv")
print(metrics_tbl)
