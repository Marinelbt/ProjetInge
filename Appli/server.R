library(data.table)
library(ggplot2)
library(dplyr)

server <- function(input, output) {
  
  # Utiliser data.table pour des lectures plus rapides et optimisées
  df <- fread("/Users/marine/Desktop/ACO M2 /ProjetInge/Handball/df_pdfs_final2.csv")
  setDT(df)  # Convertir en data.table
  
  df$statut_recevant <- as.factor(df$statut_recevant)
  
  # Filtrer les données relatives aux temps-morts
  data_filtered <- df[action == "Tem"]
  
  # Calculer la période avec dplyr mais en utilisant data.table pour les performances
  data_filtered[, periode := fcase(
    Temps_Sec >= 0 & Temps_Sec < 600, 1,
    Temps_Sec >= 600 & Temps_Sec < 1200, 2,
    Temps_Sec >= 1200 & Temps_Sec < 1800, 3,
    Temps_Sec >= 1800 & Temps_Sec < 2400, 4,
    Temps_Sec >= 2400 & Temps_Sec < 3000, 5,
    Temps_Sec >= 3000, 6,
    default = NA_real_
  )]
  
  data_filtered$statut_action <- ifelse(data_filtered$action_equipe == "v", 
                                        ifelse(data_filtered$statut_recevant == "-1", "1",
                                               ifelse(data_filtered$statut_recevant == "1", "-1", "0")),
                                        as.character(data_filtered$statut_recevant))
  
  # Calcul des pourcentages par période
  data_pourcentage <- data_filtered[, .(total_periode = .N), by = periode]
  data_pourcentage[, pourcentage := total_periode / sum(total_periode) * 100]
  
  # Joindre les pourcentages au dataset original
  data_filtered <- merge(data_filtered, data_pourcentage, by = "periode", all.x = TRUE)
  
  # Graphique
  output$repartition_TM <- renderPlot({
    ggplot(data = data_filtered, aes(x = as.factor(periode), fill = statut_action)) +
      geom_bar() +
      geom_text(
        aes(
          x = as.factor(periode),
          y = after_stat(count),  # Utiliser after_stat(count) pour la comptabilisation
          label = paste0(round(pourcentage, 1), "%")  # Afficher les pourcentages
        ),
        stat = "count",  # Statistique de comptage pour les barres
        inherit.aes = FALSE,  # Empêcher l'héritage des esthétiques globales
        vjust = -0.5,  # Position du texte
        color = "black",
        size = 3
      ) +
      geom_vline(
        xintercept = 3.5,  # Ligne verticale entre les périodes 3 et 4
        linetype = "dashed",  # Style de ligne pointillée
        color = "black",  # Couleur de la ligne
        linewidth = 1  # Utiliser linewidth au lieu de size
      ) +
      labs(
        title = "Nombre de temps-morts par période",
        x = "Périodes (1 période = 10 min)",
        y = "Nombre de Temps-Morts",
        fill = "Statut équipe"
      ) +
      scale_fill_manual(
        values = c("-1" = "indianred", "0" = "gray", "1" = "darkolivegreen3"),
        labels = c("-1" = "Perd", "0" = "Égalité", "1" = "Gagne")
      ) +
      theme_minimal()
    p2 <- data[data$action == "Tem", ] %>%
      filter(Temps_Sec >= 59 * 60) %>%
      ggplot(aes(x = as.factor(periode), fill = statut_action)) +
      geom_bar() +  # Garde les barres en nombre total
      geom_text(
        stat = "count",
        aes(
          label = scales::percent(after_stat(count) / tapply(after_stat(count), ..x.., sum)[as.character(..x..)], accuracy = 1)
        ),
        position = position_stack(vjust = 0.5),  # Empile les labels au centre des segments
        color = "white",
        size = 3
      ) +
      labs(
        title = "Nombre de temps-\nmorts à la dernière \nminute",
        x = "59 à 60min (dernière minute)",
        y = "Nombre de Temps-Morts",
        fill = "Statut équipe"
      ) +
      scale_fill_manual(
        values = c("-1" = "indianred", "0" = "gray", "1" = "darkolivegreen3"),
        labels = c("-1" = "Perd", "0" = "Égalité", "1" = "Gagne")
      ) +
      theme_minimal()
    
    ggarrange(p1, p2, widths = c(2, 1))
  })
}
