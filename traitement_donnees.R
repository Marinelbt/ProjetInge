install.packages("pdftools")    # Pour extraire le texte des PDF
install.packages("tidyverse")  # Pour la manipulation et visualisation des données
install.packages("stringr")    # Pour traiter les chaînes de caractères

library(tidyverse)
library(stringr)
library(pdftools)

# Lire le fichier PDF
pdf_file <- "UAFJKMF.pdf"
text <- pdf_text(pdf_file)

# Séparer le texte ligne par ligne
lines <- unlist(strsplit(text, "\n"))

# Filtrer les lignes contenant "Temps Mort"
temps_morts <- lines[str_detect(lines, "Temps Mort|But")]

# Aperçu des temps morts
head(temps_morts)

# Extraire le temps
temps <- str_extract(temps_morts, "\\d{1,2}:\\d{2}")

# Extraire le score
score <- str_extract(temps_morts, "\\d{1,2} - \\d{1,2}")

# Identifier l'équipe
equipe <- ifelse(str_detect(temps_morts, "Recevant"), "Recevant", "Visiteur")

# Créer un tableau structuré
df_tm <- data.frame(Temps = temps, Score = score, Equipe = equipe)

# Définition score recevant et visiteur
df_tm <- df_tm %>%
  mutate(
    Score_Recevant = as.numeric(str_extract(Score, "^\\d+")),
    Score_Visiteur = as.numeric(str_extract(Score, "\\d+$"))
  )











# Charger le fichier PDF
pdf_file <- "UAFJKMF.pdf"
text <- pdf_text(pdf_file)

# Diviser le contenu en lignes
lines <- unlist(strsplit(text, "\n"))

# Extraire les lignes contenant un temps suivi d'un score
table_lines <- lines[str_detect(lines, "^\\d{1,2}:\\d{2} \\d{1,2} - \\d{1,2}")]

# Structurer les données
df <- data.frame(
  Temps = str_extract(table_lines, "^\\d{1,2}:\\d{2}"),                     # Extraire le temps
  Score = str_extract(table_lines, "\\d{1,2} - \\d{1,2}"),                 # Extraire le score
  Action = str_trim(str_remove(table_lines, "^\\d{1,2}:\\d{2} \\d{1,2} - \\d{1,2}"))  # Extraire l'action
)

# Aperçu des données structurées
head(df)


