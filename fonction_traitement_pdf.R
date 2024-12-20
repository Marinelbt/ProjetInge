library(pdftools)  # Pour extraire le texte des PDF
library(tidyverse) # Pour la manipulation et visualisation des données
library(stringr)   # Pour traiter les chaînes de caractères


##### Fonction traitement document pdf #####

traitement_pdf <- function(pdf_file) {   
  text <- pdf_text(pdf_file)
  
  # Diviser le contenu en lignes
  lines <- unlist(strsplit(text, "\n"))
  lines <- str_squish(lines)
  
  # Extraire les données de chaque ligne contenant un motif de temps et de score
  table_lines <- lines[str_detect(lines, "\\d{2}:\\d{2}\\s+\\d{2} - \\d{2}")]
  
  # Appliquer l'extraction de la ligne à toutes les lignes pertinentes
  extracted_data <- lapply(table_lines, function(line) {
    matches <- str_match_all(line, "(\\d{2}:\\d{2})\\s+(\\d{2} - \\d{2})\\s+(.*?)(?=(\\d{2}:\\d{2}|$))")
    
    # Convertir en data.frame
    data.frame(
      temps = matches[[1]][, 2],
      score = matches[[1]][, 3],
      action = matches[[1]][, 4],
      stringsAsFactors = FALSE
    )
  })
  
  # Combiner toutes les lignes extraites en un seul data frame
  df_match <- do.call(rbind, extracted_data)
  
  df_match <- as.data.frame(df_match)
  
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
  code_renc <- str_extract(code_renc, "(?<=Renc ?)[A-Z]+")
  code_renc <- str_trim(str_extract(code_renc, "\\S+$"))[1]
  
  # Nom équipes
  equipe <- lines[str_detect(lines, " / ")][1]
  equipe <- str_split(equipe, "/")
  recevant <- str_trim(equipe[[1]][1])
  visiteur <- str_trim(str_extract(equipe[[1]][2], "^[^\\d]+"))
  
  # Noms coach
  index_coach <- which(str_detect(lines, "Officiel Resp\\."))
  if (length(index_coach) == 0){
    index_coach <- which(str_detect(lines, "Officiel Resp"))
    
    # Extraire le nom et prénom de la ligne suivante pour chaque occurrence de "Officiel Resp"
    coach <- sapply(index_coach, function(index) {
      if (length(which(str_detect(lines, "Officiel Resp A"))) != 0){
        line_next <- lines[index]
      } else {
        # Extraire la ligne suivante
        line_next <- lines[index +1]
      }
      name_and_surname <- str_extract(line_next, "[A-ZÉÈÊÎÔÛÄËÏÖÜŸÇ]+\\s+[a-zéèêîôûäëïöüÿç]+")
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
  
  # Tableau avec les infos des équipes
  df_info_equipe <- data.frame(
    code_rencontre = code_renc,
    club_recevant = recevant,
    coach_recevant = coach_recevant,
    club_visiteur = visiteur,
    coach_visiteur = coach_visiteur,
    stringsAsFactors = FALSE
  )
  
  # Assembler les deux dataframes
  df_info_repeat <- df_info_equipe[rep(1, nrow(df_match)), ]
  df_info_match <- cbind(df_info_repeat, df_match)
  rownames(df_info_match) <- NULL
  
  # Séparer score en score recevant et visiteur
  df_info_match$score_final <- df_info_match$score[nrow(df_info_match)]
  df_info_match <- df_info_match %>% separate(col = score,
                                              into = c("score_recevant", "score_visiteur"),
                                              sep = '-')
  df_info_match$score_recevant <- as.numeric(df_info_match$score_recevant)
  df_info_match$score_visiteur <- as.numeric(df_info_match$score_visiteur)
  
  # Journée
  ind_journee <- which(str_detect(lines, "J[0-9]")==T)
  ligne_J <- lines[ind_journee]
  journee <- regmatches(ligne_J, regexpr("J\\d+", ligne_J))
  df_info_match$journee <- journee
  
  return(df_info_match)
}


ajout_variables <- function(df_info_match) {   
  if (!is.data.frame(df_info_match)) {
    stop("L'entrée n'est pas un data frame!")
  }
  
  df_info_match$Temps_Sec <- as.numeric(df_info_match$Temps_Sec)
  df_info_match$score_recevant <- as.numeric(df_info_match$score_recevant)
  df_info_match$score_visiteur <- as.numeric(df_info_match$score_visiteur)
  
  # Extraire les index où chaque équipe a fait une action
  index_visiteur <- which((str_detect(df_info_match$action, "Visiteur|JV"))==TRUE)
  index_recevant <- which((str_detect(df_info_match$action, "Recevant|JR"))==TRUE)
  
  # Créer une colonne "qui fait l'action"
  df_info_match$action_equipe <- ""
  df_info_match$action_equipe[index_recevant] <- "r"
  df_info_match$action_equipe[index_visiteur] <- "v"
  
  df_final <- df_info_match
  # Ajouter une colonne si l'équipe recevant est en train de perdre, gagner, égalité
  df_final$statut_recevant <- rep(0, nrow(df_final))
  for (i in 1:nrow(df_final)) {
    if (df_final$score_recevant[i] > df_final$score_visiteur[i]) {
      df_final$statut_recevant[i] <- 1
    } else if (df_final$score_recevant[i] < df_final$score_visiteur[i]) {
      df_final$statut_recevant[i] <- -1
    }
  }
  
  df_final$ecart_recevant <- df_final$score_recevant - df_final$score_visiteur
  df_final <- df_final %>%
    mutate(action = substr(as.character(action), 1, 3))
  
  return(df_final)
}

ajout_variables(traitement_pdf("pdf/rencontre_D1H_2021_589.pdf"))

pdf_file <- "pdf/rencontre_D1H_2021_383.pdf"

