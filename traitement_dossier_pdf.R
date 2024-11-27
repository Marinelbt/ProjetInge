library(dplyr)

source("fonction_traitement_pdf.R")

traiter_dossier_pdf <- function(dossier_pdf, df_existant = NULL){   # format chemin du dossier
  # Obtenir la liste des fichiers pdf du dossier
  fichiers <- list.files(dossier_pdf, pattern = "\\.pdf$", full.names=TRUE)
  
  # Vérifier quels fichiers ont déjà été traités
  if (!is.null(df_existant)) {
    fichiers_existants <- unique(df_existant$fichier)  # Récupérer les fichiers déjà présents
    fichiers <- fichiers[!(basename(fichiers) %in% fichiers_existants)]  # Exclure ceux déjà traités
  }
  
  # Si aucun nouveau fichier, retourner le dataframe existant
  if (length(fichiers) == 0) {
    message("Aucun nouveau fichier à traiter.")
    return(df_existant)
  }
  
  resultats <- lapply(fichiers, function(fichier) {
    # Étape 1 : Traiter le PDF
    df_pdf <- traitement_pdf(fichier)
    
    # Étape 2 : Ajouter des variables
    df_avec_variables <- ajout_variables(df_pdf)
    
    df_avec_variables$fichier <- basename(fichier)
    
    return(df_avec_variables)
  })
  
  # Combiner tous les dataframes en un seul
  df_complet_final <- bind_rows(resultats)
  
  return(df_complet_final)
}

df <- traiter_dossier_pdf("pdf")

# Enregistrer le dataframe final dans un fichier csv
write.csv(resultat, "df_pdfs.csv", row.names = FALSE)  
