library(tidyverse) # Pour la manipulation et visualisation des données
library(stringr)   # Pour traiter les chaînes de caractères
library(pdftools)  # Pour extraire le texte des PDF



# Charger le fichier PDF
pdf_file <- "pdf/rencontre_D2H_2425_854.pdf"
text <- pdf_text(pdf_file)

# Diviser le contenu en lignes
lines <- unlist(strsplit(text, "\n"))
lines <- str_squish(lines)


##### Déroulé du match #####

# Extraire les données de chaque ligne contenant un motif de temps et de score
table_lines <- lines[str_detect(lines, "\\d{2}:\\d{2}\\s+\\d{2} - \\d{2}")]

# Appliquer l'extraction de la ligne à toutes les lignes pertinentes
extracted_data <- lapply(table_lines, function(line) {
  matches <- str_match_all(line, "(\\d{2}:\\d{2})\\s+(\\d{2} - \\d{2})\\s+(.*?)(?=(\\d{2}:\\d{2}|$))")
  data.frame(
    temps = matches[[1]][, 2],
    score = matches[[1]][, 3],
    action = matches[[1]][, 4]
  )
})

extracted_data$score <- as.numeric(extracted_data$score)

# Combiner toutes les lignes extraites en un seul data frame
df_match <- do.call(rbind, extracted_data)

# Convertir le temps en secondes pour faciliter le tri
df_match <- df_match %>%
  mutate(
    Temps_Sec = as.numeric(str_extract(temps, "^\\d{1,2}")) * 60 + as.numeric(str_extract(temps, "\\d{2}$"))
  )

# Trier les données par Temps_Sec
df_match <- df_match %>%
  arrange(Temps_Sec)


# Afficher le résultat final
head(df_match)



##### Extraire code rencontre #####

code_renc <- lines[str_detect(lines, "Code Renc")]
code_renc <- str_extract(code_renc, "(?<=Renc ?)[A-Z]+")
code_renc <- str_trim(str_extract(code_renc, "\\S+$"))[1]
code_renc


##### Nom équipes #####

equipe <- lines[str_detect(lines, " / ")][1]
equipe <- str_split(equipe, "/")
recevant <- str_trim(equipe[[1]][1])
visiteur <- str_trim(str_extract(equipe[[1]][2], "^[^\\d]+"))


##### Noms coachs #####

index_coach <- which(str_detect(lines, "Officiel Resp\\."))
if (length(index_coach) == 0){
  index_coach <- which(str_detect(lines, "Officiel Resp"))
  
  # Extraire le nom et prénom de la ligne suivante pour chaque occurrence de "Officiel Resp"
  coach <- sapply(index_coach, function(index) {
    line_next <- lines[index + 1]
    name_and_surname <- str_extract(line_next, "^[^\\d]+")  # Capture tout avant un chiffre
    str_trim(name_and_surname)  # Supprimer les espaces superflus
  })
} else {
  coach <- sapply(index_coach, function(index) {
    line_coach <- lines[index]
    name_and_surname <- str_extract(line_coach, "[A-ZÉÈÊÎÔÛÄËÏÖÜŸÇ]+\\s+[a-zéèêîôûäëïöüÿç]+")  # Capture un mot en majuscule suivi d'un mot en minuscule
  })
}

coach_recevant <- coach[1]
coach_visiteur <- coach[2]


df_info_equipe <- data.frame(
  code_rencontre = code_renc,
  club_recevant = recevant,
  coach_recevant = coach_recevant,
  club_visiteur = visiteur,
  coach_visiteur = coach_visiteur
)

df_info_repeat <- df_info_equipe[rep(1,nrow(df_match)),]
df_info_match <- cbind(df_info_repeat, df_match)
rownames(df_info_match) <- NULL

df_info_match$score_final <- df_info_match$score[nrow(df_info_match)]
df_info_match <- df_info_match %>% separate(col = score,
                           into = c("score_recevant", "score_visiteur"),
                           sep = '-')
df_info_match$score_recevant <- as.numeric(df_info_match$score_recevant)
df_info_match$score_visiteur <- as.numeric(df_info_match$score_visiteur)

##### Dataframe temps-morts #####

# Extraire les index où chaque équipe à pris un temps mort
index_tm_visiteur <- which(is.na(str_extract(df_info_match$action, "^Temps Mort.+Visiteur$")) == FALSE)
index_tm_recevant <- which(is.na(str_extract(df_info_match$action, "^Temps Mort.+Recevant$")) == FALSE)

# Créer une colonne "qui prend le temps mort"

df_info_match$temps_mort_equipe <- ""
df_info_match$temps_mort_equipe[index_tm_recevant] <- "r"
df_info_match$temps_mort_equipe[index_tm_visiteur] <- "v"

# But 1 min après temps-mort

df_info_match$but_1min_apres_temps_mort <- ""

for (i in which(df_info_match$temps_mort_equipe != "")){
  ind_1min_apres_tm <- which(df_info_match$Temps_Sec > df_info_match$Temps_Sec[i] & 
                               df_info_match$Temps_Sec <= df_info_match$Temps_Sec[i] + 60)
  
  for (j in ind_1min_apres_tm){
    if (df_info_match$temps_mort_equipe[i] == "r" & 
        df_info_match$score_recevant[j] > df_info_match$score_recevant[i]){
      df_info_match$but_1min_apres_temps_mort[i] <- "oui"
    }
    else if (df_info_match$temps_mort_equipe[i] == "v" & 
               df_info_match$score_visiteur[j] > df_info_match$score_visiteur[i]){
      df_info_match$but_1min_apres_temps_mort[i] <- "oui"
    }
    
    else {
      df_info_match$but_1min_apres_temps_mort[i] <- "non"
    }
  }
  
}


df_final <- df_info_match[grepl("^(Temps Mort|But|Tir)", df_info_match$action), ] %>% 
  select(-"action", -"Temps_Sec")

# Ajouter une colonne si l'équipe est en train de perdre, gagner, égalité

df_final$statut_recevant <- rep(0, nrow(df_final))
for (i in 1:nrow(df_final)){
  if (df_final$score_recevant[i] > df_final$score_visiteur[i]){
    df_final$statut_recevant[i] <- 1
  }
  else if (df_final$score_recevant[i] < df_final$score_visiteur[i]){
    df_final$statut_recevant[i] <- -1
  }
}

df_final$ecart_recevant <- df_final$score_recevant - df_final$score_visiteur

###### Récupérer la journée ######

ind_journee <- which(str_detect(lines, "J[0-9]")==T)
ligne_J <- lines[ind_journee]
journee <- regmatches(ligne_J, regexpr("J\\d+", ligne_J))

df_final$journee <- journee

###### Classement par journée ######

# Victoire : +2
# Egalité : +1
# Défaite : +0

df <- read.csv("df_pdfs_2.csv")
df <- df %>%
  separate(score_final, into = c("score_final_r", "score_final_v"), sep = "-") %>%
  mutate(
    score_final_r = as.integer(score_final_r),
    score_final_v = as.integer(score_final_v)
  )

df <- df %>%
  mutate(
    saison = str_extract(fichier, "\\d{4}"),           
    HF = str_extract(fichier, "[FH]"),              
    division = str_extract(fichier, "D\\d"),          
    match_num = str_extract(fichier, "_\\d+_\\d{4}") %>%
      str_extract("^\\d+")
  ) %>% 
  select(-match_num)

df <- df %>% select("code_rencontre","club_recevant","club_visiteur", "score_final_r", "score_final_v",
              "division","HF","saison", "journee", "fichier")

df_unique <- df %>%
  distinct(code_rencontre, .keep_all = TRUE)



for (i in 1:nrow(df)){
  if (df$score_final_r[i] > df$score_final_v[i]) {
    df$point_r[i] <- 2
    df$point_v[i] <- 0
  } else if (df$score_final_r[i] == df$score_final_v[i]){
    df$point_r[i] <- 1
    df$point_v[i] <- 1
  } else if (df$score_final_r[i] < df$score_final_v[i]){
    df$point_r[i] <- 0
    df$point_v[i] <- 2
  }
}

pdf <- read.csv("df_pdfs.csv")

pdf <- pdf %>% group_by("fichier") %>% 
  summarize(fichier = unique(fichier))

length(df$fichier)
length(pdf$fichier)

points_par_club <- df %>%
  select(code_rencontre,journee, division, saison, club = club_recevant, points = point_r, HF) %>%
  bind_rows(
    df %>% select(code_rencontre,journee, division, saison, club = club_visiteur, points = point_v, HF)
  )

print(points_par_club)

points_par_club %>%  group_by(club)

