# -------- hist_target_plots.R  -----------------------------------
library(tidyverse)

# 1. Lire les jeux déjà pré-traités
adult   <- read_rds("results/adult_clean.rds")$train
diab    <- read_rds("results/diabetes_clean.rds")$train

# 2. Dossier figs/ 
if (!dir.exists("figs")) dir.create("figs")

# 3. Fonction utilitaire ------------------------------------------------
plot_target <- function(data, target, outfile, xlab = "Classe cible") {
  p <- data %>%
    count(.data[[target]]) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(x = .data[[target]], y = pct)) +
    geom_col(fill = "grey60") +
    geom_text(aes(label = scales::percent(pct, accuracy = .1)),
              vjust = -0.3, size = 3.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = xlab, y = "Proportion") +
    theme_minimal(base_size = 11)
  
  ggsave(outfile, p, width = 7, height = 5, device = cairo_pdf)
}

# 4. Génération des deux figures ---------------------------------------
plot_target(adult, "income_bin",  "figs/hist_adult.pdf")
plot_target(diab,  "readmitted_bin", "figs/hist_diabetes.pdf")
