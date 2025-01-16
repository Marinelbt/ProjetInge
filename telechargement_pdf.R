
library(RSelenium)
library(httr)
library(stringr)
library(dplyr)
library(rvest)

dossier_sauvegarde <- "pdf"

ligue <- c("N1F6", "N1F7", "N1F8", "N1F9")
saisons <- c("2021")

df_url <- data.frame(matrix(NA, nrow = length(ligue), ncol = length(saisons)))
rownames(df_url) <- ligue
colnames(df_url) <- saisons

df_url[1,1] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/nationale-1-feminine-15605/poule-78056/rencontre-1264"
df_url[2,1] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/nationale-1-feminine-15605/poule-78057/rencontre-1264"
df_url[3,1] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/nationale-1-feminine-15605/poule-78058/rencontre-1264"
df_url[4,1] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/nationale-1-feminine-15605/poule-78059/rencontre-1264"

df_num_init <- data.frame(matrix(NA, nrow = length(ligue), ncol = length(saisons)))
rownames(df_num_init) <- ligue
colnames(df_num_init) <- saisons

df_num_init[1,1] <- 529
df_num_init[2,1] <- 559
df_num_init[3,1] <- 589
df_num_init[4,1] <- 619


df_num_fin <- data.frame(matrix(NA, nrow = length(ligue), ncol = length(saisons)))
rownames(df_num_fin) <- ligue
colnames(df_num_fin) <- saisons


df_num_fin[1,1] <- 537
df_num_fin[2,1] <- 564
df_num_fin[3,1] <- 597
df_num_fin[4,1] <- 627
  

telechargement <- function(saison, ligue, url, numero_recontre_init, numero_recontre_fin) {
  # Lancer un navigateur avec RSelenium
  driver <- rsDriver(browser = "chrome", port = as.integer(4444))
  remDr <- driver[["client"]]
  
  base_url <- url
  
  for (i in numero_recontre_init:numero_recontre_fin) {
    # Nom du fichier PDF
    nom_fichier <- paste0("rencontre_", ligue, "_", saison, "_", i, ".pdf")
    chemin_fichier <- file.path(dossier_sauvegarde, nom_fichier)
    
    # Vérifier si le fichier existe déjà
    if (file.exists(chemin_fichier)) {
      next  # Passer à la prochaine rencontre sans faire la recherche
    }
    
    # Construire l'URL de la page
    url_page <- paste0(base_url, i)
    remDr$navigate(url_page)
    Sys.sleep(3)  # Attendre 3 secondes pour que la page charge
    
    # Extraire le HTML de la page après le chargement
    page_source <- remDr$getPageSource()[[1]]
    page <- read_html(page_source)
    
    # Trouver le lien du PDF
    pdf_link <- page %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset("\\.pdf$")  # Filtrer les liens se terminant par .pdf
    
    # Débogage : Afficher le lien trouvé
    cat("Rencontre", i, "- Lien PDF détecté :", pdf_link, "\n")
    
    # Si un lien PDF est trouvé
    if (length(pdf_link) > 0) {
      pdf_url <- pdf_link[1]  # Prendre le premier lien si plusieurs sont trouvés
      pdf_url_complete <- ifelse(str_starts(pdf_url, "http"), pdf_url, paste0("https://www.ffhandball.fr", pdf_url))
      
      # Vérifier que le lien PDF est valide
      if (!is.null(pdf_url_complete) && nzchar(pdf_url_complete)) {
        # Débogage : Afficher le lien complet
        cat("Rencontre", i, "- URL PDF complète :", pdf_url_complete, "\n")
        
        # Télécharger le PDF
        tryCatch({
          GET(pdf_url_complete, write_disk(chemin_fichier, overwrite = TRUE))
          cat("Fichier", nom_fichier, "téléchargé\n")
        }, error = function(e) {
          cat("Erreur lors du téléchargement du fichier pour la rencontre", i, ":", conditionMessage(e), "\n")
        })
      } else {
        cat("Lien PDF invalide pour la rencontre", i, "\n")
      }
    } else {
      cat("Aucun PDF trouvé pour la rencontre", i, "\n")
    }
  }
  
  # Arrêter le serveur Selenium
  remDr$close()
  driver$server$stop()
}


for (i in ligue) {
  for (j in saisons) {
    saison <- j
    ligue <- i
    
    # Accéder à l'URL pour la ligue i et la saison j
    url <- df_url[i, as.character(saison)]
    
    # Accéder au numéro de rencontre initial et final pour la ligue i et la saison j
    numero_recontre_init <- df_num_init[i, as.character(saison)]
    numero_recontre_fin <- df_num_fin[i, as.character(saison)]
    
    # Vérification si tous les fichiers attendus existent déjà
    fichiers_manquants <- FALSE
    for (num in numero_recontre_init:numero_recontre_fin) {
      nom_fichier <- paste0("rencontre_", ligue, "_", saison, "_", num, ".pdf")
      chemin_fichier <- file.path(dossier_sauvegarde, nom_fichier)
      
      if (!file.exists(chemin_fichier)) {
        fichiers_manquants <- TRUE
        break  # Pas besoin de continuer à vérifier
      }
    }
    
    # Si des fichiers sont manquants, lancer le téléchargement
    if (fichiers_manquants) {
      cat("Des fichiers manquent pour", ligue, "saison", saison, "- téléchargement en cours.\n")
      telechargement(saison, ligue, url, numero_recontre_init, numero_recontre_fin)
    } else {
      cat("Tous les fichiers pour", ligue, "saison", saison, "sont déjà présents. Téléchargement ignoré.\n")
    }
  }
}