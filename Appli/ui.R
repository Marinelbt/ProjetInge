library(shiny)
library(shinydashboard)

# Définition de vos couleurs personnalisées
sidebar_color <- "#00274d"   # Bleu foncé pour la barre latérale
content_color <- "#ffffff"   # Blanc pour le contenu
button_color <- "#003366"    # Bleu pour les boutons
hover_color <- "#004080"     # Bleu plus clair pour le survol des boutons
active_button_color <- "#00509e" # Bleu foncé pour le bouton actif

ui <- dashboardPage(
  dashboardHeader(
    title = HTML("<div style='line-height: 1.2em;'>Analyse des<br>prises de temps-mort</div>")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse des matchs", tabName = "analyse", icon = icon("chart-line")),
      menuItem("Prédiction", tabName = "prediction", icon = icon("chart-pie"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML(
      "
      .main-sidebar {
        background-color: #00274d !important;
        color: white !important;
      }
      .sidebar-menu > li > a {
        background-color: #003366;
        color: white !important;
        padding: 10px 15px;
        text-decoration: none;
        border-radius: 5px;
      }
      .sidebar-menu > li > a:hover {
        background-color: #004080 !important;
        color: white !important;
      }
      .sidebar-menu > li.active > a {
        background-color: #00509e !important;
        font-weight: bold;
        color: white !important;
      }
      
      .main-header {
        background-color: #00274d !important;
      }
      
      .main-header .logo {
        background-color: #00274d !important;
        white-space: normal !important;
        line-height: 1.4em;
        color: white !important;
      }

      .navbar {
        background-color: #00274d !important;
      }
      
      .navbar .sidebar-toggle {
        background-color: #00274d !important;
      }

      .content-wrapper {
        background-color: #ffffff !important;
      }
      
      .box {
        background-color: #ffffff;
        border-top: 5px solid #00274d;
      }
      "
    )),
    
    tabItems(
      # Onglet "Analyse des matchs"
      tabItem(
        tabName = "analyse",
        h2("Analyse des matchs"),
        
        fluidRow(
          box(
            title = "Filtres", status = "primary", solidHeader = TRUE, width = 12
          )
        ),
        
        fluidRow(
          column(
            width = 6,  # Utiliser column au lieu de box pour enlever le contour
            plotOutput("repartition_TM")
          )
        )
      ),
      
      # Onglet "Prédiction"
      tabItem(
        tabName = "prediction",
        h2("Prédiction"),
        p("C'est ici que vous pouvez ajouter votre contenu pour l'onglet Prédiction.")
      )
    )
  )
)
