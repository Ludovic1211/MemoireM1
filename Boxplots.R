# ────────────────────────────────────────────────────────────────
#  AUC – boxplots horizontaux (Adult vs Diabetes)
# ────────────────────────────────────────────────────────────────
library(tidyverse)

# 1. Lecture -----------------------------------------------------
plot_df <- read_csv("figs/table_metrics.csv", show_col_types = FALSE) %>% 
  transmute(dataset = dataset,
            auc      = roc_auc_mean)

# 2. Ordre = médiane décroissante -------------------------------
plot_df <- plot_df %>% 
  mutate(dataset = fct_reorder(dataset, auc, .desc = TRUE))

# 3. Box-plot horizontal ----------------------------------------
p <- ggplot(plot_df, aes(y = dataset, x = auc, fill = dataset)) +
  geom_boxplot(height = .55, alpha = .75, colour = "grey25",
               outlier.shape = 21, outlier.fill = "white", outlier.size = 2) +
  stat_summary(fun = median, geom = "point",
               shape = 23, size = 3, fill = "white", colour = "black") +
  scale_x_continuous(limits = c(0.60, 1),
                     breaks  = seq(0.60, 1.00, .05),
                     expand  = expansion(mult = c(0, .015))) +
  labs(x = "AUC moyenne (folds externes)", y = NULL,
       title = "Performance globale par jeu de données") +
  guides(fill = "none") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = .5)
  )

# 4. Aperçu & export --------------------------------------------
print(p)
ggsave("figs/auc_boxplot.pdf", plot = p,
       width = 5.5, height = 2.6, device = cairo_pdf)
