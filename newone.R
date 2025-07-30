# ─────────────────────────────────────────────────────────────
#  Temps d’entraînement  VS  nombre de variables retenues
# ─────────────────────────────────────────────────────────────
library(tidyverse)
library(scales)

df <- read_csv("figs/table_metrics.csv", show_col_types = FALSE) %>% 
  filter(dataset == "Adult") %>%
  mutate(method = str_replace_all(model, "_", " "))

p <- ggplot(df, aes(x = n_vars_mean, y = time_total_sec,
                    label = method)) +
  geom_point(size = 3, colour = "#1f77b4") +
  geom_text(nudge_y = 100, size = 3) +
  scale_x_log10(name = "Nombre moyen de variables (log10)",
                breaks = c(1,5,10,20,50,100)) +
  scale_y_log10(name = "Temps total (secondes, log10)",
                labels = comma) +
  theme_minimal(base_size = 11)

ggsave("figs/time_vs_vars.pdf", p, width = 5.5, height = 3)
