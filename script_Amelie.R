library(tidyverse) # Pour la manipulation et visualisation des données
library(stringr)   # Pour traiter les chaînes de caractères
library(pdftools)  # Pour extraire le texte des PDF
library(dplyr)


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
    TRUE ~ club
  ))

result <- points_par_club %>%
  select(club, division, saison, HF, journee, points) %>%
  pivot_wider(names_from = journee, values_from = points) %>% 
  arrange(HF, saison, division, club)

result$match_joue <- ""
for (i in 1:nrow(result)){
  result$match_joue[i] <- sum(is.na(result[i,]) == FALSE) -5
}

# ne pas exécuter car fichier compléter à la main
#write.csv(result, "point_jour.csv")

point_jour <- read.csv("Data/point_jour.csv", header = T, sep = ";", row.names = 1)

point_jour_F <- point_jour %>% filter(HF == 'F') %>% 
  select(-c(J27,J28,J29,J30))
point_jour_H <- point_jour %>% filter(HF == 'H')


# Point par jour de la compétition masculine D1 et D2, 2223 et 2324
for (i in 1:nrow(point_jour_H)){
  if (is.na(point_jour_H$J1[i]) == T){
    point_jour_H$J1[i] <- 0
  }
  for (j in 6:34){  # j = [J2:J6]
    if (is.na(point_jour_H[i,j-1])==T){
      point_jour_H[i,j-1] <- point_jour_H[i,j-2]
      point_jour_H[i,j] <- point_jour_H[i,j] + point_jour_H[i,j-1]
    } else {
      point_jour_H[i,j] <- point_jour_H[i,j] + point_jour_H[i,j-1]
    }
  }
  if(is.na(point_jour_H$J30[i])==T){
    point_jour_H$J30[i] <- point_jour_H$J25[i]
  }
  
}

# Point par jour de la compétition féminine D1 et D2, 2223 et 2324
for (i in 1:nrow(point_jour_F)){
  if (is.na(point_jour_F$J1[i]) == T){
    point_jour_F$J1[i] <- 0
  }
  for (j in 6:30){  # j = [J2:J26]
    if (is.na(point_jour_F[i,j-1])==T){
      point_jour_F[i,j-1] <- point_jour_F[i,j-2]
      point_jour_F[i,j] <- point_jour_F[i,j] + point_jour_F[i,j-1]
    } else {
      point_jour_F[i,j] <- point_jour_F[i,j] + point_jour_F[i,j-1]
    }
  }
  if(is.na(point_jour_F$J26[i])==T){
    point_jour_F$J26[i] <- point_jour_F$J25[i]
  }
  
}


point_jour_F$saison <- factor(point_jour_F$saison)
point_jour_F$division <- factor(point_jour_F$division)

point_jour_H$saison <- factor(point_jour_H$saison)
point_jour_H$division <- factor(point_jour_H$division)

##### Evolution difference Femme

summary_df_F <- point_jour_F %>% 
  select(-c("match_joue", "total_point")) %>% 
  group_by(saison, division) %>%
  summarise(across(where(is.numeric), list(Max = max, 
                                           Min = min), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -c(saison, division), 
               names_to = c("Jour", ".value"), 
               names_pattern = "(.*)_(.*)") %>%  
  mutate (Diff = Max - Min)

summary_df_F$Jour <- factor(summary_df_F$Jour, 
                          levels = unique(summary_df_F$Jour[order(as.numeric(str_extract(summary_df_F$Jour, "\\d+")))]))

ggplot(summary_df_F, aes(x = Jour, group = interaction(saison, division))) +
  geom_line(aes(y = Diff, color = interaction(saison, division))) +
  labs(title = "Évolution dfférence de points entre premier et dernier",
       x = "Journées", y = "Ecarts", color = "Saison-Division") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Evolution difference Homme

summary_df_H <- point_jour_H %>% 
  select(-c("match_joue", "total_point")) %>% 
  group_by(saison, division) %>%
  summarise(across(where(is.numeric), list(Max = max, 
                                           Min = min), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -c(saison, division), 
               names_to = c("Jour", ".value"), 
               names_pattern = "(.*)_(.*)") %>%  
  mutate (Diff = Max - Min)

summary_df_H$Jour <- factor(summary_df_H$Jour, 
                          levels = unique(summary_df_H$Jour[order(as.numeric(str_extract(summary_df_H$Jour, "\\d+")))]))

ggplot(summary_df_H, aes(x = Jour, group = interaction(saison, division))) +
  geom_line(aes(y = Diff, color = interaction(saison, division))) +
  labs(title = "Évolution dfférence de points entre premier et dernier",
       x = "Journées", y = "Ecarts", color = "Saison-Division") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###### Ajout variable diff-classement ######

data_mod1 <- read.csv("Data/data_mod1_j.csv")
point_jour_F <- read.csv("Data/point_jour_F.csv", row.names = 1)
point_jour_F$HF <- 'F'
point_jour_F$J0 <- 0
point_jour_F <- point_jour_F %>% relocate('J0', .after = 'HF')
point_jour_H <- read.csv("Data/point_jour_H.csv", row.names = 1)
point_jour_H$J0 <- 0
point_jour_H <- point_jour_H %>% relocate('J0', .after = 'HF')

for (j in 31:6){
  point_jour_F[,j] <- point_jour_F[,j-1]
}

for (j in 35:6){
  point_jour_H[,j] <- point_jour_H[,j-1]
}

point_jour_F <- point_jour_F %>%
  select(-J0) %>% 
  pivot_longer(cols = starts_with("J"), names_to = "journee", values_to = "points") %>%
  mutate(journee = toupper(journee))

point_jour_H <- point_jour_H %>%
  select(-J0) %>% 
  pivot_longer(cols = starts_with("J"), names_to = "journee", values_to = "points") %>%
  mutate(journee = toupper(journee))


data_mod1 <- data_mod1 %>% 
  select(-c(points_classement, points_opp, diff_classement))

data_mod1 <- data_mod1 %>% 
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
    TRUE ~ club_recevant
  ))

data_mod1 <- data_mod1 %>% 
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
    TRUE ~ club_visiteur
  ))

point_jour_final <- bind_rows(point_jour_F, point_jour_H)
#write.csv(point_jour_final, "point_jour_final.csv")

data_mod1 <- data_mod1 %>% 
  left_join(point_jour_final, by = c("HF","division","saison", "journee","club_recevant"="club")) %>% 
  mutate(points_recevant = points) %>% 
  select(-c(match_joue, total_point, points))

data_mod1 <- data_mod1 %>% 
  left_join(point_jour_final, by = c("HF","division","saison", "journee","club_visiteur"="club")) %>% 
  mutate(points_visiteur = points) %>% 
  select(-c(match_joue, total_point, points))

data_mod1 <- data_mod1 %>% 
  mutate (diff_point = points_recevant - points_visiteur)

data_mod1$diff_point_norm <- scale(data_mod1$diff_point)

data_mod1$diff_point_norm[,1]

#write.csv(data_mod1, "data_mod1_jour_point.csv")
