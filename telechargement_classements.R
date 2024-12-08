library(RSelenium)
library(rvest)

# Lancer un serveur Selenium (assurez-vous que Selenium est installé)
driver <- rsDriver(browser = "chrome", port = as.integer(4444))
remote_driver <- driver[["client"]]

# Ouvrir la page web
remote_driver$navigate("https://www.ffhandball.fr/competitions/saison-2022-2023-18/national/proligue-2022-2023-20359/poule-108936/classements/")

# Extraire le code source après chargement de la page
page_source <- remote_driver$getPageSource()[[1]]

# Charger le contenu avec rvest
page <- read_html(page_source)

# Extraire la table
classement <- page %>%
  html_node("table") %>%
  html_table(fill = TRUE)

# Fermer le navigateur
remote_driver$close()
driver$server$stop()

classement <- classement[,-11]

write.csv(classement, "classement_D2H_2223.csv", row.names = FALSE)

