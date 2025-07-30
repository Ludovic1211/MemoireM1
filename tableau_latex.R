library(dplyr)
library(readr)
library(knitr)
library(kableExtra)

overview <- tribble(
  ~`Jeu de données`, ~`# obs.`, ~`# prédicteurs bruts`, ~`# dummies`, ~`% classe positive`,
  "Adult Income", 48842, 14, 108, 24.1,
  "Hospital Readmission (Diabetes)", 101766, 55, 150, 11.2
)

overview %>%
  kable(booktabs = TRUE, format = "latex",
        caption = "Effectifs et dimensions après pré-traitement",
        label = "overview") %>%
  kable_styling(latex_options = c("striped","hold_position"))
