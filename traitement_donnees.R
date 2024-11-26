library(tidyverse) # Pour la manipulation et visualisation des données
library(stringr)   # Pour traiter les chaînes de caractères
library(pdftools)  # Pour extraire le texte des PDF


# Charger le fichier PDF
pdf_file <- "UAFJKJI.pdf"
text <- pdf_text(pdf_file)

# Diviser le contenu en lignes
lines <- unlist(strsplit(text, "\n"))


##### Déroulé du match #####

# Extraire les données de chaque ligne contenant un motif de temps et de score
table_lines <- lines[str_detect(lines, "\\d{2}:\\d{2}\\s+\\d{2} - \\d{2}")]

# Appliquer l'extraction de la ligne à toutes les lignes pertinentes
extracted_data <- lapply(table_lines, function(line) {
  matches <- str_match_all(line, "(\\d{2}:\\d{2})\\s+(\\d{2} - \\d{2})\\s+(.*?)(?=(\\d{2}:\\d{2}|$))")
  data.frame(
    Temps = matches[[1]][, 2],
    Score = matches[[1]][, 3],
    Action = matches[[1]][, 4]
  )
})

# Combiner toutes les lignes extraites en un seul data frame
df_match <- do.call(rbind, extracted_data)

# Convertir le temps en secondes pour faciliter le tri
df_match <- df_match %>%
  mutate(
    Temps_Sec = as.numeric(str_extract(Temps, "^\\d{1,2}")) * 60 + as.numeric(str_extract(Temps, "\\d{2}$"))
  )

# Trier les données par Temps_Sec
df_match <- df_match %>%
  arrange(Temps_Sec)
df_match <- df_match %>%
  select(-Temps_Sec)

# Afficher le résultat final
head(df_match)



##### Extraire code rencontre #####

code_renc <- lines[str_detect(lines, "Code Renc")]
code_renc <- str_extract(code_renc, "Code Renc\\s+(.+)")
code_renc <- str_trim(str_extract(code_renc, "\\S+$"))[1]
code_renc


##### Nom équipes #####

equipe <- lines[str_detect(lines, "/")][1]
equipe <- str_split(equipe, "/")
recevant <- str_trim(equipe[[1]][1])
visiteur <- str_trim(str_extract(equipe[[1]][2], "^[^\\d]+"))


##### Noms coachs #####

index_coach <- which(str_detect(lines, "Officiel Resp"))

# Extraire le nom et prénom de la ligne suivante pour chaque occurrence de "Officiel Resp"
coach <- sapply(index_coach, function(index) {
  # Extraire la ligne suivante
  line_next <- lines[index + 1]
  name_and_surname <- str_extract(line_next, "^[^\\d]+")  # Capture tout avant un chiffre
  str_trim(name_and_surname)  # Supprimer les espaces superflus
})
coach_recevant <- coach[1]
coach_visiteur <- coach[2]


df_info_equipe <- data.frame(
  code_renc = code_renc,
  equipe_recevant = recevant,
  coach_recevant = coach_recevant,
  equipe_visiteur = visiteur,
  coach_visiteur = coach_visiteur
)
