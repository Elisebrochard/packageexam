## code to prepare `packageexam` dataset goes here

library(tidyverse)

url_jeu_de_donnees <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

noms <- c("age","sex","cp","trestbps","chol","fbs",
          "restecg","thalach","exang","oldpeak","slope","ca","thal","num")

data <- read.csv(url_jeu_de_donnees, header = FALSE, col.names = noms, na.strings = "?")

packageexam <- data |>
  select(age, sexe = sex, douleur = cp, tension = trestbps, 
         cholesterol = chol, max_bpm = thalach, 
         angine_effort = exang, stress_cardiaque = oldpeak, diagnostic = num) |>
  mutate(
    sexe = factor(sexe, levels = c(0, 1), labels = c("Femme", "Homme")),
    douleur = factor(douleur, levels = c(1, 2, 3, 4),
                     labels = c("Angine Typique", "Angine Atypique",
                                "Douleur Non-Ang", "Asymptomatique")),
    angine_effort = factor(angine_effort, levels = c(0, 1), labels = c("Non", "Oui")),
    diagnostic = factor(ifelse(diagnostic > 0, "Malade", "Sain"))
  ) |>
  drop_na(tension, cholesterol, max_bpm)

usethis::use_data(packageexam, overwrite = TRUE)
