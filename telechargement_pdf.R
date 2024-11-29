library(RSelenium)
library(httr)
library(stringr)
library(dplyr)
library(rvest)

dossier_sauvegarde <- "fichiers_pdf"

ligue <- c("D1F", "D2F_1", "D1H", "D2H", "D2F_2")
saisons <- c("1819", "1920", "2021", "2122", "2223", "2324", "2425")

df_url <- data.frame(matrix(NA, nrow = length(ligue), ncol = length(saisons)))
rownames(df_url) <- ligue
colnames(df_url) <- saisons

#rajouter poule 2 D2F 1819

df_url[1,1] <- "https://www.ffhandball.fr/competitions/saison-2018-2019-14/national/championnat-lfh-2018-2019-10469/poule-46820/rencontre-778"
df_url[2,1] <- "https://www.ffhandball.fr/competitions/saison-2018-2019-14/national/championnat-d2-feminine-2018-2019-10502/poule-46945/rencontre-767"
df_url[3,1] <- "https://www.ffhandball.fr/competitions/saison-2018-2019-14/national/lidl-starligue-11084/poule-47726/rencontre-791"
df_url[4,1] <- "https://www.ffhandball.fr/competitions/saison-2018-2019-14/national/proligue-11079/poule-47672/rencontre-7"
df_url[5,1] <- "https://www.ffhandball.fr/competitions/saison-2018-2019-14/national/championnat-d2-feminine-2018-2019-10502/poule-46946/rencontre-767"
  
df_url[1,2] <- "https://www.ffhandball.fr/competitions/saison-2019-2020-15/national/ligue-butagaz-energie-13144/poule-64816/rencontre-103"
df_url[2,2] <- "https://www.ffhandball.fr/competitions/saison-2019-2020-15/national/d2-feminine-13145/poule-64807/rencontre-103"
df_url[3,2] <- "https://www.ffhandball.fr/competitions/saison-2019-2020-15/national/lidl-starligue-13591/poule-65230/rencontre-103"
df_url[4,2] <- "https://www.ffhandball.fr/competitions/saison-2019-2020-15/national/proligue-13621/poule-65280/rencontre-103"
df_url[5,2] <- "https://www.ffhandball.fr/competitions/saison-2019-2020-15/national/d2-feminine-13145/poule-64808/rencontre-103"
  
df_url[1,3] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/ligue-butagaz-energie-15603/poule-78325/rencontre-127"
df_url[2,3] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/d2-feminine-15604/poule-78105/rencontre-1265"
df_url[3,3] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/lidl-starligue-16145/poule-78580/rencontre-1287"
df_url[4,3] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/proligue-16144/poule-78944/rencontre-1289"
df_url[5,3] <- "https://www.ffhandball.fr/competitions/saison-2020-2021-16/national/d2-feminine-15604/poule-78106/rencontre-1265"
  
df_url[1,4] <- "https://www.ffhandball.fr/competitions/saison-2021-2022-17/national/ligue-butagaz-energie-17701/poule-89934/rencontre-1416"
df_url[2,4] <- "https://www.ffhandball.fr/competitions/saison-2021-2022-17/national/d2-feminine-17700/poule-89807/rencontre-1418"
df_url[3,4] <- "https://www.ffhandball.fr/competitions/saison-2021-2022-17/national/liqui-moly-starligue-2021-2022-17869/poule-90373/rencontre-1420"
df_url[4,4] <- "https://www.ffhandball.fr/competitions/saison-2021-2022-17/national/proligue-2021-2022-17871/poule-90408/rencontre-14"
  
df_url[1,5] <- "https://www.ffhandball.fr/competitions/saison-2022-2023-18/national/ligue-butagaz-energie-2022-23-19868/poule-107982/rencontre-163"
df_url[2,5] <- "https://www.ffhandball.fr/competitions/saison-2022-2023-18/national/d2-feminine-2022-23-19872/poule-108094/rencontre-1643"
df_url[3,5] <- "https://www.ffhandball.fr/competitions/saison-2022-2023-18/national/liqui-moly-starligue-2022-23-20342/poule-108997/rencontre-165"
df_url[4,5] <- "https://www.ffhandball.fr/competitions/saison-2022-2023-18/national/proligue-2022-2023-20359/poule-108936/rencontre-1653"
  
df_url[1,6] <- "https://www.ffhandball.fr/competitions/saison-2023-2024-19/national/ligue-butagaz-energie-2023-24-23036/poule-128468/rencontre-1897"
df_url[2,6] <- "https://www.ffhandball.fr/competitions/saison-2023-2024-19/national/d2-feminine-2023-24-23037/poule-128858/rencontre-1910"
df_url[3,6] <- "https://www.ffhandball.fr/competitions/saison-2023-2024-19/national/liqui-moly-starligue-2023-24-23266/poule-128388/rencontre-1892"
df_url[4,6] <- "https://www.ffhandball.fr/competitions/saison-2023-2024-19/national/proligue-2023-24-23227/poule-128331/rencontre-1891"
  
df_url[1,7] <- "https://www.ffhandball.fr/competitions/saison-2024-2025-20/national/ligue-butagaz-energie-2024-25-25625/poule-147028/rencontre-2130"
df_url[2,7] <- "https://www.ffhandball.fr/competitions/saison-2024-2025-20/national/d2-feminine-2024-25-25626/poule-147030/rencontre-2167"
df_url[3,7] <- "https://www.ffhandball.fr/competitions/saison-2024-2025-20/national/liqui-moly-starligue-2024-25-25798/poule-147541/rencontre-2126"
df_url[4,7] <- "https://www.ffhandball.fr/competitions/saison-2024-2025-20/national/proligue-2024-25-25808/poule-147544/rencontre-2128"

df_num_init <- data.frame(matrix(NA, nrow = length(ligue), ncol = length(saisons)))
rownames(df_num_init) <- ligue
colnames(df_num_init) <- saisons

df_num_init[1,1] <- 810
df_num_init[2,1] <- 195
df_num_init[3,1] <- 207
df_num_init[4,1] <- 90911
df_num_init[5,1] <- 251

df_num_init[1,2] <- 7172
df_num_init[2,2] <- 7832
df_num_init[3,2] <- 3984
df_num_init[4,2] <- 9181
df_num_init[5,2] <- 7888

df_num_init[1,3] <- 7915
df_num_init[2,3] <- 705
df_num_init[3,3] <- 351
df_num_init[4,3] <- 533
df_num_init[5,3] <- 761

df_num_init[1,4] <- 176
df_num_init[2,4] <- 142
df_num_init[3,4] <- 439
df_num_init[4,4] <- 20969

df_num_init[1,5] <- 2969
df_num_init[2,5] <- 351
df_num_init[3,5] <- 3949
df_num_init[4,5] <- 362

df_num_init[1,6] <- 527
df_num_init[2,6] <- 221
df_num_init[3,6] <- 440
df_num_init[4,6] <- 488
  
df_num_init[1,7] <- 210
df_num_init[2,7] <- 173
df_num_init[3,7] <- 139
df_num_init[4,7] <- 780

df_num_fin <- data.frame(matrix(NA, nrow = length(ligue), ncol = length(saisons)))
rownames(df_num_fin) <- ligue
colnames(df_num_fin) <- saisons


df_num_fin[1,1] <- 941
df_num_fin[2,1] <- 250
df_num_fin[3,1] <- 388
df_num_fin[4,1] <- 91092
df_num_fin[5,1] <- 306

df_num_fin[1,2] <- 7285
df_num_fin[2,2] <- 7887
df_num_fin[3,2] <- 4109
df_num_fin[4,2] <- 9313
df_num_fin[5,2] <- 7943

df_num_fin[1,3] <- 8005
df_num_fin[2,3] <- 760
df_num_fin[3,3] <- 590
df_num_fin[4,3] <- 714
df_num_fin[5,3] <- 816

df_num_fin[1,4] <- 357
df_num_fin[2,4] <- 323
df_num_fin[3,4] <- 678
df_num_fin[4,4] <- 21208


df_num_fin[1,5] <- 3150
df_num_fin[2,5] <- 532
df_num_fin[3,5] <- 4188
df_num_fin[4,5] <- 601

df_num_fin[1,6] <- 708
df_num_fin[2,6] <- 402
df_num_fin[3,6] <- 679
df_num_fin[4,6] <- 726

df_num_fin[1,7] <- 258
df_num_fin[2,7] <- 221
df_num_fin[3,7] <- 226
df_num_fin[4,7] <- 856

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
