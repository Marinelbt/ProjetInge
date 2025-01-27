library(dplyr)

source("Script_data/fonction_traitement_pdf.R")

traiter_dossier_pdf <- function(dossier_pdf, df_existant = NULL) {
  # Obtenir la liste des fichiers pdf du dossier
  fichiers <- list.files(dossier_pdf, pattern = "\\.pdf$", full.names = TRUE)
  
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
  
  # Liste pour enregistrer les fichiers ayant échoué
  fichiers_echoues <- list()
  
  resultats <- lapply(fichiers, function(fichier) {
    tryCatch({
      message("Début du traitement du fichier : ", basename(fichier))
      
      # Étape 1 : Traiter le PDF
      df_pdf <- traitement_pdf(fichier)
      
      # Vérifier si le résultat est NULL
      if (is.null(df_pdf)) {
        # Si le traitement échoue, ajouter le fichier à la liste des fichiers échoués
        fichiers_echoues <<- c(fichiers_echoues, basename(fichier))
        message("Le fichier ", basename(fichier), " n'a pas pu être traité.")
        return(NULL)  # Passer à l'itération suivante sans continuer le traitement
      }
      
      # Vérifier la structure du dataframe après le traitement du PDF
      message("Structure du dataframe pour ", basename(fichier), ":")
      print(str(df_pdf))  # Affichage de la structure du DataFrame
      
      # Étape 2 : Ajouter des variables
      df_avec_variables <- ajout_variables(df_pdf)
      
      # Vérifier la structure après ajout des variables
      message("Structure après ajout des variables pour ", basename(fichier), ":")
      print(str(df_avec_variables))  # Affichage après ajout des variables
      
      # Ajouter le nom du fichier au dataframe final
      df_avec_variables$fichier <- basename(fichier)
      
      return(df_avec_variables)
      
    }, error = function(e) {
      message("Une erreur s'est produite lors du traitement du fichier ", basename(fichier), ": ", e$message)
      fichiers_echoues <<- c(fichiers_echoues, basename(fichier))
      return(NULL)
    })
  })
  
  # Filtrer les résultats NULL et combiner les dataframes valides
  df_complet_final <- bind_rows(Filter(Negate(is.null), resultats))
  
  # Vérifier la structure du dataframe combiné
  message("Structure du dataframe combiné final :")
  print(str(df_complet_final))  # Affichage de la structure du dataframe final combiné
  
  # Si des fichiers ont échoué, afficher un message
  if (length(fichiers_echoues) > 0) {
    message("Les fichiers suivants ont échoué lors du traitement :")
    print(fichiers_echoues)
  }
  
  return(df_complet_final)
}


df <- traiter_dossier_pdf("pdf")

# Enregistrer le dataframe final dans un fichier csv
write.csv(df, "data_final/df_pdfs_final.csv", row.names = FALSE)  
