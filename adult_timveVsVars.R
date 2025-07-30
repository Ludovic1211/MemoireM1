library(tidyverse)
library(glue)
library(scales)

# 1. préparation des métriques -----------------------------------------------
tbl <- read_csv("results/metrics_all.csv", show_col_types = FALSE) |>
  arrange(dataset, desc(roc_auc_mean)) |>
  rename(methode = model) |>
  mutate(
    auc  = sprintf("%.3f $\\pm$ %.3f", roc_auc_mean, roc_auc_sd),
    acc  = sprintf("%.3f", accuracy_mean),
    nvar = ifelse(is.na(n_vars_mean) | n_vars_mean < 0, "-",
                  sprintf("%d", round(n_vars_mean))),
    time = comma(time_total_sec, accuracy = 1)
  ) |>
  select(dataset, methode, auc, acc, nvar, time) |>
  mutate(across(everything(), \(x) gsub("_", "\\\\_", x)))

# 2. génération du code LaTeX -----------------------------------------------
latex_lines <- c(
  "\\begin{tabularx}{\\textwidth}{l l c c c r}",
  "\\toprule",
  "Jeu & Méthode & AUC & Acc. & \\#var & Temps (s)\\\\",
  "\\midrule",
  glue_data(tbl, "{dataset} & {methode} & {auc} & {acc} & {nvar} & {time}\\\\"),
  "\\bottomrule",
  "\\end{tabularx}"
)

write_lines(latex_lines, "figs/table_metrics_tex.tex")
