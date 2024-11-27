library(pdftools)  # Pour extraire le texte des PDF
library(tidyverse) # Pour la manipulation et visualisation des données
library(stringr)   # Pour traiter les chaînes de caractères


##### Fonction traitement document pdf #####

traitement_pdf <- function(pdf_file){   # format "nom_doc.pdf"
  text <- pdf_text(pdf_file)
  
  # Diviser le contenu en lignes
  lines <- unlist(strsplit(text, "\n"))
  
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

  # Extraire code rencontre
  code_renc <- lines[str_detect(lines, "Code Renc")]
  code_renc <- str_extract(code_renc, "Code Renc\\s+(.+)")
  code_renc <- str_trim(str_extract(code_renc, "\\S+$"))[1]
  
  # Nom équipes
  equipe <- lines[str_detect(lines, "/")][1]
  equipe <- str_split(equipe, "/")
  recevant <- str_trim(equipe[[1]][1])
  visiteur <- str_trim(str_extract(equipe[[1]][2], "^[^\\d]+"))
  
  # Noms coach
  index_coach <- which(str_detect(lines, "Officiel Resp"))
  
  # Extraire le nom et prénom de la ligne suivante pour chaque occurrence de "Officiel Resp"
  coach <- sapply(index_coach, function(index) {
    line_next <- lines[index + 1]
    name_and_surname <- str_extract(line_next, "^[^\\d]+")  # Capture tout avant un chiffre
    str_trim(name_and_surname)  # Supprimer les espaces superflus
  })
  coach_recevant <- coach[1]
  coach_visiteur <- coach[2]
  
  # tableau avec les infos des équipes
  df_info_equipe <- data.frame(
    code_rencontre = code_renc,
    club_recevant = recevant,
    coach_recevant = coach_recevant,
    club_visiteur = visiteur,
    coach_visiteur = coach_visiteur
  )
  
  # Assembler les deux dataframe
  df_info_repeat <- df_info_equipe[rep(1,nrow(df_match)),]
  df_info_match <- cbind(df_info_repeat, df_match)
  rownames(df_info_match) <- NULL
  
  # Séparer score en score recevant et visiteur
  df_info_match$score_final <- df_info_match$score[nrow(df_info_match)]
  df_info_match <- df_info_match %>% separate(col = score,
                                              into = c("score_recevant", "score_visiteur"),
                                              sep = '-')
  df_info_match$score_recevant <- as.numeric(df_info_match$score_recevant)
  df_info_match$score_visiteur <- as.numeric(df_info_match$score_visiteur)
  
  return (df_info_match)
}


ajout_variables <- function(data_info_match){   # format data.frame
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
  
  # Garder les variables qui nous intéressent
  df_final <- df_info_match[grepl("^(Temps Mort|But)", df_info_match$action), ] %>% 
    select(-"action", -"Temps_Sec")
  
  return (df_final)
}  


ajout_variables(traitement_pdf("UAFJKJI.pdf"))
