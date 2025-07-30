library(tidyverse)
metrics <- read_csv("results/metrics_all.csv", show_col_types = FALSE)

ggplot(metrics,
       aes(x = n_vars_mean, y = time_total_sec/60, colour = dataset, label = model)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.6, size = 3) +
  scale_y_log10() +
  scale_x_continuous(breaks = pretty) +
  labs(x = "# variables retenues",
       y = "Temps d’entraînement (minutes, échelle log)",
       colour = NULL) +
  theme_minimal(base_size = 10)

ggsave("figs/time_vs_vars.pdf", width = 5.5, height = 3)
