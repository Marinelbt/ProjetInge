library(RSelenium)
library(httr)
library(stringr)
library(dplyr)

# Dossier pour sauvegarder les fichiers PDF
dossier_sauvegarde <- "fichiers_pdf"

# Lancer un navigateur avec RSelenium
driver <- rsDriver(browser = "chrome", port = as.integer(4444))
remDr <- driver[["client"]]

# Base de l'URL des rencontres
base_url <- "https://www.ffhandball.fr/competitions/saison-2024-2025-20/national/liqui-moly-starligue-2024-25-25798/poule-147541/rencontre-2126"

# Boucle sur les numéros de rencontre de 139 à 220
for (i in 139:220) {
  # Construire l'URL complète
  url_page <- paste0(base_url, i)
  
  # Ouvrir la page dans le navigateur
  remDr$navigate(url_page)
  
  # Attendre que le contenu soit chargé (ajuster le délai si nécessaire)
  Sys.sleep(3)  # attendre 3 secondes
  
  # Extraire le HTML de la page après le chargement
  page_source <- remDr$getPageSource()[[1]]
  
  # Charger le HTML dans rvest
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
    
    # Construire l'URL complète du PDF
    pdf_url_complete <- ifelse(str_starts(pdf_url, "http"), pdf_url, paste0("https://www.ffhandball.fr", pdf_url))
    
    # Vérifier que le lien PDF est valide
    if (!is.null(pdf_url_complete) && nzchar(pdf_url_complete)) {
      # Débogage : Afficher le lien complet
      cat("Rencontre", i, "- URL PDF complète :", pdf_url_complete, "\n")
      
      # Nom du fichier PDF
      nom_fichier <- paste0("rencontre_", i, ".pdf")
      
      # Télécharger le PDF
      tryCatch({
        GET(pdf_url_complete, write_disk(file.path(dossier_sauvegarde, nom_fichier), overwrite = TRUE))
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