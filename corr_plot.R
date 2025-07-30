# ================================================
# 99_make_corrplots.R – Heatmaps de corrélations
# ================================================
library(tidyverse)
library(corrr)
library(GGally)


dir.create("figs", showWarnings = FALSE)

# -------- helper -------------------------------------------------
make_corrplot <- function(data_rds, outfile, top_n = 30) {
  data <- read_rds(data_rds)$train
  
  
  numerics <- data %>% 
    select(where(is.numeric)) %>% 
    select(-contains("_dummy"), -contains("_bin"))
  
 
  vars <- numerics %>% 
    summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>% 
    pivot_longer(everything()) %>% 
    arrange(desc(value)) %>% 
    slice_head(n = top_n) %>% 
    pull(name)
  
  mat <- numerics %>% select(all_of(vars)) %>% correlate()
  
  # ---------- plot ----------
  p <- mat %>% 
    shave() %>% 
    stretch() %>% 
    ggplot(aes(x = x, y = y, fill = r)) +
    geom_tile() +
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1)) +
    labs(fill = "r") +
    theme_minimal(base_size = 8) +
    theme(axis.text.x  = element_text(angle = 60, hjust = 1),
          panel.grid   = element_blank())
  
  ggsave(outfile, p, width = 6, height = 6)
}

# --------- exécution --------------------------------------------
make_corrplot("results/diabetes_clean.rds",
              "figs/corr_diabetes.pdf",  top_n = 40)

make_corrplot("results/adult_clean.rds",
              "figs/corr_adult.pdf",     top_n = 40)
