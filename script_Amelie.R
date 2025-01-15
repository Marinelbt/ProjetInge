library(tidyverse) # Pour la manipulation et visualisation des données
library(stringr)   # Pour traiter les chaînes de caractères
library(pdftools)  # Pour extraire le texte des PDF
library(dplyr)


# Charger le fichier PDF
pdf_file <- "N1/rencontre_N1H1_2122_4014.pdf"
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
equipe <- str_split(equipe, " / ")
recevant <- str_trim(equipe[[1]][1])
visiteur <- str_trim(str_extract(equipe[[1]][3], "^[^\\d]+"))


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

# Victoire : +3
# Egalité : +2
# Défaite : +1

df <- read.csv("Data/df_pdfs.csv")
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



for (i in 1:nrow(df_unique)){
  if (df_unique$score_final_r[i] > df_unique$score_final_v[i]) {
    df_unique$point_r[i] <- 3
    df_unique$point_v[i] <- 1
  } else if (df_unique$score_final_r[i] == df_unique$score_final_v[i]){
    df_unique$point_r[i] <- 2
    df_unique$point_v[i] <- 2
  } else if (df_unique$score_final_r[i] < df_unique$score_final_v[i]){
    df_unique$point_r[i] <- 1
    df_unique$point_v[i] <- 3
  }
}

points_par_club <- df_unique %>%
  select(code_rencontre,journee, division, saison, club = club_recevant, points = point_r, HF) %>%
  bind_rows(
    df_unique %>% select(code_rencontre,journee, division, saison, club = club_visiteur, points = point_v, HF)
  )

print(points_par_club)

points_par_club$journee <- as.factor(points_par_club$journee)
points_par_club$HF <- as.factor(points_par_club$HF)
points_par_club$division <- as.factor(points_par_club$division)
points_par_club$saison <- as.factor(points_par_club$saison)
points_par_club$points <- as.numeric(points_par_club$points)

summary(points_par_club$journee)

points_par_club <- points_par_club %>% 
  mutate(club = case_when(
    club == "PARIS 92" ~ "PARIS",
    club == "HANDBALL CLERMONT AUVERGNE METROPOLE 63" ~ "HANDBALL CLERMONT AUVERGNE METROPOLE",
    club == "BOUILLARGUES HANDBALL NIMES MEDITERRANEE" ~ "BOUILLARGUES HANDBALL NIMES METROPOLE",
    club == "LE POUZIN HB 07" ~ "LE POUZIN HB",
    club == "C'CHARTRES HANDBALL" ~ "C'CHARTRES METROPOLE HANDBALL",
    club == "LIMOGES HAND 87" ~ "LIMOGES HAND",
    club == "NANCY METROPOLE HB" ~ "NANCY METROPOLE",
    club == "NANCY HANDBALL" ~ "NANCY METROPOLE",
    club == "PAYS AIX UNIVERSITE CLUB HANDBALL" ~ "PROVENCE AIX UNIVERSITE CLUB HANDBALL",
    club == "VILLEURBANNE HANDBALL ASSOCIATION" ~ "VILLEURBANNE HANDBALL",
    club == "TREMBLAY EN FRANCE HANDBALL" ~ "TREMBLAY HANDBALL",
    club == "TREMBLAY-EN-FRANCE HANDBALL" ~ "TREMBLAY HANDBALL",
    club == "TOULON METROPOLE VAR HB" ~ "TOULON METROPOLE VAR HANDBALL",
    club == "BILLERE HANDBALL" ~ "BILLERE HANDBALL PAU PYRENEES",
    club == "CHARTRES HANDBALL" ~ "C'CHARTRES METROPOLE HANDBALL",
    club == "CAVIGAL NICE SPORTS HANDBALL" ~ "CAVIGAL NICE HANDBALL",
    club == "FRONTIGNAN HANDBALL" ~ "FRONTIGNAN THAU HANDBALL",
    club == "HBC NANTAIS" ~ "HBC NANTES",
    club == "MASSY ESSONNE HB" ~ "MASSY ESSONNE HANDBALL",
    club == "NOISY-LE-GRAND HANDBALL" ~ "NOISY LE GRAND HANDBALL",
    club == "ROCHECHOUART-ST-JUNIEN HANDBALL 87" ~ "ROCHECHOUART-ST-JUNIEN HANDBALL",
    club == "HBC ST AMAND LES EAUX PORTE DU HAINAUT" ~ "ST AMAND HANDBALL - PORTE DU HAINAUT",
    club == "CHAMBERY SAVOIE MT-BLANC HB" ~ "CHAMBERY SAVOIE HB",
    club == "GRAND NANCY METROPOLE HB" ~ "GRAND NANCY METROPOLE HANDBALL",
    club == "LIMOGES HAND" ~ "LIMOGES HANDBALL",
    club == "SAINT RAPHAEL VHB" ~ "SAINT RAPHAEL VAR HANDBALL",
    club == "SARAN LOIRET HB" ~ "SARAN LOIRET HANDBALL",
    club == "DIJON METROPOLE HB" ~ "DIJON METROPOLE HANDBALL",
    club == "JS CHERBOURGEOISE MANCHE HB" ~ "JS CHERBOURGEOISE MANCHE HANDBALL",
    club == "E. STRASBOURG SCHILTIGHEIM ALSACE HANDBALL" ~ "STRASBOURG EUROMETROPOLE HB",
    club == "LIMOGES HANDBALL" ~ "LIMOGES",
    club == "LIMOGES HAND" ~ "LIMOGES",
    TRUE ~ club
  )) %>% 
  filter(club != "SAINT-CYR VAR HANDBALL")

result <- points_par_club %>%
  select(club, division, saison, HF, journee, points) %>%
  pivot_wider(names_from = journee, values_from = points) %>% 
  arrange(HF, saison, division, club)

result$match_joue <- ""
for (i in 1:nrow(result)){
  result$match_joue[i] <- sum(is.na(result[i,]) == FALSE) -5
}

# ne pas exécuter car fichier compléter à la main
#write.csv(result, "point_jour2.csv")

point_jour <- read.csv("Data/point_jour.csv", row.names = 1, sep=";")

# Point par jour
for (i in 1:nrow(point_jour)){
  if (is.na(point_jour$J1[i]) == T){
    point_jour$J1[i] <- 0
  }
  for (j in 8:36){  # j = [J2:J6]
    if (is.na(point_jour[i,j-1])==T){
      point_jour[i,j-1] <- point_jour[i,j-2]
      point_jour[i,j] <- point_jour[i,j] + point_jour[i,j-1]
    } else {
      point_jour[i,j] <- point_jour[i,j] + point_jour[i,j-1]
    }
  }
  if(is.na(point_jour$J30[i])==T){
    point_jour$J30[i] <- point_jour$J25[i]
  }
  
}


point_jour$saison <- factor(point_jour$saison)
point_jour$division <- factor(point_jour$division)


##### Evolution difference

summary_df <- point_jour %>% 
  filter(HF == "F") %>% 
  select(-c("match_joue", "total_point")) %>% 
  group_by(saison, division) %>%
  summarise(across(where(is.numeric), list(Max = max, 
                                           Min = min), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -c(saison, division), 
               names_to = c("Jour", ".value"), 
               names_pattern = "(.*)_(.*)") %>%  
  mutate (Diff = Max - Min)

summary_df$Jour <- factor(summary_df$Jour, 
                          levels = unique(summary_df$Jour[order(as.numeric(str_extract(summary_df$Jour, "\\d+")))]))

ggplot(summary_df, aes(x = Jour, group = interaction(saison, division))) +
  geom_line(aes(y = Diff, color = interaction(saison, division))) +
  labs(title = "Évolution dfférence de points entre premier et dernier",
       x = "Journées", y = "Ecarts", color = "Saison-Division") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###### Variable diff-classement ######

point_jour$J0 <- 0
point_jour <- point_jour %>% relocate('J0', .after = 'total_point')

for (j in 37:8){
  point_jour[,j] <- point_jour[,j-1]
}

point_jour <- point_jour %>%
  select(-J0) %>% 
  pivot_longer(cols = starts_with("J"), names_to = "journee", values_to = "points") %>%
  mutate(journee = toupper(journee))

#write.csv(point_jour_final, "point_jour.csv")

####### normaliser diff point par jour #######

df <- read.table("df_pdfs.csv", header = TRUE, sep = ",")
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

df <- df %>% 
  select("code_rencontre","club_recevant","club_visiteur", "score_final_r", "score_final_v",
                    "division","HF","saison", "journee", "fichier") %>%
  distinct(code_rencontre, .keep_all = TRUE)

df <- df %>% 
  mutate(club_recevant = case_when(
    club_recevant == "PARIS 92" ~ "PARIS",
    club_recevant == "HANDBALL CLERMONT AUVERGNE METROPOLE 63" ~ "HANDBALL CLERMONT AUVERGNE METROPOLE",
    club_recevant == "BOUILLARGUES HANDBALL NIMES MEDITERRANEE" ~ "BOUILLARGUES HANDBALL NIMES METROPOLE",
    club_recevant == "LE POUZIN HB 07" ~ "LE POUZIN HB",
    club_recevant == "C'CHARTRES HANDBALL" ~ "C'CHARTRES METROPOLE HANDBALL",
    club_recevant == "LIMOGES HAND 87" ~ "LIMOGES HAND",
    club_recevant == "NANCY METROPOLE HB" ~ "NANCY METROPOLE",
    club_recevant == "NANCY HANDBALL" ~ "NANCY METROPOLE",
    club_recevant == "PAYS AIX UNIVERSITE CLUB HANDBALL" ~ "PROVENCE AIX UNIVERSITE CLUB HANDBALL",
    club_recevant == "VILLEURBANNE HANDBALL ASSOCIATION" ~ "VILLEURBANNE HANDBALL",
    club_recevant == "TREMBLAY EN FRANCE HANDBALL" ~ "TREMBLAY HANDBALL",
    club_recevant == "TREMBLAY-EN-FRANCE HANDBALL" ~ "TREMBLAY HANDBALL",
    club_recevant == "TOULON METROPOLE VAR HB" ~ "TOULON METROPOLE VAR HANDBALL",
    club_recevant == "BILLERE HANDBALL" ~ "BILLERE HANDBALL PAU PYRENEES",
    club_recevant == "CHARTRES HANDBALL" ~ "C'CHARTRES METROPOLE HANDBALL",
    club_recevant == "CAVIGAL NICE SPORTS HANDBALL" ~ "CAVIGAL NICE HANDBALL",
    club_recevant == "FRONTIGNAN HANDBALL" ~ "FRONTIGNAN THAU HANDBALL",
    club_recevant == "HBC NANTAIS" ~ "HBC NANTES",
    club_recevant == "MASSY ESSONNE HB" ~ "MASSY ESSONNE HANDBALL",
    club_recevant == "NOISY-LE-GRAND HANDBALL" ~ "NOISY LE GRAND HANDBALL",
    TRUE ~ club_recevant
  )) %>%
  filter (club_recevant != "HBC ST AMAND LES EAUX PORTE DU HAINAUT",club_visiteur != "SAINT-CYR VAR HANDBALL")

df <- df %>% 
  mutate(club_visiteur = case_when(
    club_visiteur == "PARIS 92" ~ "PARIS",
    club_visiteur == "HANDBALL CLERMONT AUVERGNE METROPOLE 63" ~ "HANDBALL CLERMONT AUVERGNE METROPOLE",
    club_visiteur == "BOUILLARGUES HANDBALL NIMES MEDITERRANEE" ~ "BOUILLARGUES HANDBALL NIMES METROPOLE",
    club_visiteur == "LE POUZIN HB 07" ~ "LE POUZIN HB",
    club_visiteur == "C'CHARTRES HANDBALL" ~ "C'CHARTRES METROPOLE HANDBALL",
    club_visiteur == "LIMOGES HAND 87" ~ "LIMOGES HAND",
    club_visiteur == "NANCY METROPOLE HB" ~ "NANCY METROPOLE",
    club_visiteur == "NANCY HANDBALL" ~ "NANCY METROPOLE",
    club_visiteur == "PAYS AIX UNIVERSITE CLUB HANDBALL" ~ "PROVENCE AIX UNIVERSITE CLUB HANDBALL",
    club_visiteur == "VILLEURBANNE HANDBALL ASSOCIATION" ~ "VILLEURBANNE HANDBALL",
    club_visiteur == "TREMBLAY EN FRANCE HANDBALL" ~ "TREMBLAY HANDBALL",
    club_visiteur == "TREMBLAY-EN-FRANCE HANDBALL" ~ "TREMBLAY HANDBALL",
    club_visiteur == "TOULON METROPOLE VAR HB" ~ "TOULON METROPOLE VAR HANDBALL",
    club_visiteur == "BILLERE HANDBALL" ~ "BILLERE HANDBALL PAU PYRENEES",
    club_visiteur == "CHARTRES HANDBALL" ~ "C'CHARTRES METROPOLE HANDBALL",
    club_visiteur == "CAVIGAL NICE SPORTS HANDBALL" ~ "CAVIGAL NICE HANDBALL",
    club_visiteur == "FRONTIGNAN HANDBALL" ~ "FRONTIGNAN THAU HANDBALL",
    club_visiteur == "HBC NANTAIS" ~ "HBC NANTES",
    club_visiteur == "MASSY ESSONNE HB" ~ "MASSY ESSONNE HANDBALL",
    club_visiteur == "NOISY-LE-GRAND HANDBALL" ~ "NOISY LE GRAND HANDBALL",
    TRUE ~ club_visiteur
  )) %>%
  filter (club_visiteur != "HBC ST AMAND LES EAUX PORTE DU HAINAUT", club_visiteur != "SAINT-CYR VAR HANDBALL" )


df$saison <- factor(df$saison)
point_jour_final$saison <- factor(point_jour_final$saison)


df <- df %>% 
  left_join(point_jour_final, by = c("HF","division","saison", "journee","club_recevant"="club")) %>% 
  mutate(points_recevant = points) %>% 
  select(-c(match_joue, total_point, points))

df <- df %>% 
  left_join(point_jour_final, by = c("HF","division","saison", "journee","club_visiteur"="club")) %>% 
  mutate(points_visiteur = points) %>% 
  select(-c(match_joue, total_point, points))

df <- df %>% 
  mutate (diff_point = points_recevant - points_visiteur)

data_filtre <- df %>%
  filter(saison == 2223, division == "D1", HF == "F")

data_filtre$journee <- factor(data_filtre$journee, 
                                levels = unique(data_filtre$journee))

ggplot(data_filtre, aes(x = journee, y = diff_point_norm)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +  # Points bien alignés
  labs(title = "Nuage de points des différences de points par journée",
       x = "Journée",
       y = "Différence de points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df <- df %>% 
  group_by(saison, division, HF, journee) %>% 
  mutate(diff_point_norm = scale(diff_point))

for (i in 1:nrow(df)){
  df$diff_point_norm[i] <- ifelse (df$diff_point[i] == 0, 0, df$diff_point_norm[i])
}

df <- df %>% select(code_rencontre, diff_point_norm)

#write.csv(df, "data_diff_point_norme.csv")








