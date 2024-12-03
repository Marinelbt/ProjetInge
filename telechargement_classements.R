library(RSelenium)

# Lancer un serveur Selenium (assurez-vous que Selenium est installé)
driver <- rsDriver(browser = "chrome", port = as.integer(4444))
remote_driver <- driver[["client"]]

# Ouvrir la page web
remote_driver$navigate("https://www.ffhandball.fr/competitions/saison-2023-2024-19/national/liqui-moly-starligue-2023-24-23266/poule-128388/classements/")

# Extraire le code source après chargement de la page
page_source <- remote_driver$getPageSource()[[1]]

# Charger le contenu avec rvest
page <- read_html(page_source)

# Extraire la table
classement_D1H_2324 <- page %>%
  html_node("table") %>%
  html_table(fill = TRUE)

# Fermer le navigateur
remote_driver$close()
driver$server$stop()

classement_D1H_2324 <- classement_D1H_2324[,-11]


# Lancer un serveur Selenium (assurez-vous que Selenium est installé)
driver <- rsDriver(browser = "chrome", port = as.integer(4444))
remote_driver <- driver[["client"]]

# Ouvrir la page web
remote_driver$navigate("https://www.ffhandball.fr/competitions/saison-2022-2023-18/national/liqui-moly-starligue-2022-23-20342/poule-108997/classements/")

# Extraire le code source après chargement de la page
page_source <- remote_driver$getPageSource()[[1]]

# Charger le contenu avec rvest
page <- read_html(page_source)

# Extraire la table
classement_D1H_2223 <- page %>%
  html_node("table") %>%
  html_table(fill = TRUE)

# Fermer le navigateur
remote_driver$close()
driver$server$stop()

classement_D1H_2223 <- classement_D1H_2223[,-11]

write.csv(classement_D1H_2223, "classement_D1H_2223.csv", row.names = FALSE)
write.csv(classement_D1H_2324, "classement_D1H_2324.csv", row.names = FALSE)

