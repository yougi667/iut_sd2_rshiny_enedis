library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(ggplot2)
library(dplyr)

# Chargement des données

base <- read.csv("donneesNeufsAnciens.csv")

# Conversion des étiquettes DPE en scores numériques
base$Score_DPE <- case_when(
  base$Etiquette_DPE == "A" ~ 1,
  base$Etiquette_DPE == "B" ~ 2,
  base$Etiquette_DPE == "C" ~ 3,
  base$Etiquette_DPE == "D" ~ 4,
  base$Etiquette_DPE == "E" ~ 5,
  base$Etiquette_DPE == "F" ~ 6,
  base$Etiquette_DPE == "G" ~ 7
)

# Création de la colonne Couleur_echelle
base$Couleur_echelle <- case_when(
  base$Etiquette_DPE == "A" ~ "darkgreen",
  base$Etiquette_DPE == "B" ~ "green",
  base$Etiquette_DPE == "C" ~ "green",
  base$Etiquette_DPE == "D" ~ "yellow",
  base$Etiquette_DPE == "E" ~ "orange",
  base$Etiquette_DPE == "F" ~ "darkorange",
  base$Etiquette_DPE == "G" ~ "red"
)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Analyse DPE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil"),
      menuItem("Vue d’ensemble", tabName = "vue_ensemble"),
      menuItem("Carte interactive", tabName = "carte"),
      menuItem("Corrélation", tabName = "correlation"),
      menuItem("Données", tabName = "donnees")
    )
  ),
  dashboardBody(
    tabItems(
      # Onglet Accueil
      tabItem(tabName = "accueil",
              fluidPage(
                titlePanel("Projet DPE - GreenTech Solutions"),
                h4("Ce projet vise à analyser les diagnostics de performance énergétique (DPE) des logements en France."),
                img(src = "DPE.png", height = "50%", width = "50%")
              )
      ),
      
      # Onglet Vue d’ensemble
      tabItem(tabName = "vue_ensemble",
              fluidRow(
                box(width = 12,
                    selectInput("departement", "Sélectionner un département:", choices = unique(base$Code_postal_.BAN.), multiple = TRUE),
                    checkboxGroupInput("type_logement", "Type de logement:", choices = unique(base$Type_bâtiment))
                )
              ),
              fluidRow(
                infoBoxOutput("nbDPE", width = 3),
                infoBoxOutput("coutChauffage", width = 3),
                infoBoxOutput("emissionCO2", width = 3),
                infoBoxOutput("scoreDPE", width = 3)  # Nouveau KPI
              ),
              fluidRow(
                box(plotOutput("histDPE"), width = 6, background = "blue", title = "Répartition des étiquettes DPE"),
                box(plotOutput("pieType"), width = 6, background = "green", title = "Répartition des types de bâtiments")
              ),
              fluidRow(
                box(plotOutput("energieType"), width = 6, background = "orange", title = "Répartition des Etiquettes GES"),
                box(plotOutput("surfaceDist"), width = 6, background = "purple", title = "Répartition de la consommation moyenne annuelle en kWhef/an par Etiquette DPE")
              )
      ),
      
      # Onglet Carte interactive
      tabItem(tabName = "carte",
              fluidPage(
                # Titre de la page
                titlePanel("Carte interactive des DPE"),
                
                # Panneau de contrôle (KPIs et filtres)
                fluidRow(
                  column(width = 4,
                         box(width = NULL, title = "Filtres et KPIs", status = "primary",
                             # Filtre par code postal
                             selectInput("code_postal", "Filtrer par code postal :", 
                                         choices = unique(base$Code_postal_.BAN.),  # Liste des codes postaux
                                         multiple = FALSE),  # Sélection unique
                             actionButton("appliquer_filtre", "Appliquer le filtre"),
                             hr(),
                             # KPIs
                             infoBoxOutput("kpi_nb_dpe_carte", width = NULL),
                             infoBoxOutput("kpi_cout_chauffage_carte", width = NULL),
                             infoBoxOutput("kpi_emission_co2_carte", width = NULL)
                         )
                  ),
                  column(width = 8,
                         # Carte Leaflet
                         box(width = NULL, leafletOutput("map", height = "600px"))
                  )
                )
              )
      ),
      
      # Onglet Corrélation
      tabItem(tabName = "correlation",
              sidebarLayout(
                sidebarPanel(
                  selectInput("x", "Variable X:", choices = names(base)),
                  selectInput("y", "Variable Y:", choices = names(base)),
                  actionButton("refresh", "Tirage aléatoire")
                ),
                mainPanel(
                  plotOutput("scatterPlot"),
                  downloadButton("downloadPlot", "Télécharger le graphique")
                )
              )
      ),
      
      # Onglet Données
      tabItem(tabName = "donnees",
              fluidPage(
                dataTableOutput("dataTable"),
                downloadButton("downloadData", "Télécharger CSV")
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Filtrage des données en fonction des inputs
  base_filtered <- reactive({
    data <- base
    if (!is.null(input$departement) && length(input$departement) > 0) {
      data <- data %>% filter(Code_postal_.BAN. %in% input$departement)
    }
    if (!is.null(input$type_logement) && length(input$type_logement) > 0) {
      data <- data %>% filter(Type_bâtiment %in% input$type_logement)
    }
    return(data)
  })
  
  # KPI 1 : Nombre de DPE
  output$nbDPE <- renderInfoBox({
    infoBox(
      title = "Nombre de DPE",
      value = nrow(base_filtered()),
      icon = icon("file"),
      color = "blue",
      fill = TRUE
    )
  })
  
  # KPI 2 : Moyenne du coût de chauffage/an
  output$coutChauffage <- renderInfoBox({
    infoBox(
      title = "Moyenne du coût de chauffage/an",
      value = paste(round(mean(as.numeric(base_filtered()$Coût_chauffage), na.rm = TRUE), 2), '€', sep = " "),
      icon = icon("fire"),
      color = "red",
      fill = TRUE
    )
  })
  
  # KPI 3 : Moyenne des émissions de CO2/an
  output$emissionCO2 <- renderInfoBox({
    infoBox(
      title = "Moyenne des émissions de CO2/an",
      value = paste(round(mean(as.numeric(base_filtered()$Emission_GES_5_usages), na.rm = TRUE), 2), "kg de CO2/an", sep = " "),
      icon = icon("leaf"),
      color = "green",
      fill = TRUE
    )
  })
  
  # KPI 4 : Score moyen des DPE
  output$scoreDPE <- renderInfoBox({
    infoBox(
      title = "Score moyen des DPE",
      subtitle = "A=1 et G=7",  # Sous-titre ajouté ici
      value = round(mean(base_filtered()$Score_DPE, na.rm = TRUE), 2),
      icon = icon("chart-line"),
      color = "purple",
      fill = TRUE
    )
  })
  
  # Carte interactive
  output$map <- renderLeaflet({
    base_filtered_map <- base_filtered() %>% filter(!is.na(lon) & !is.na(lat))
    leaflet(base_filtered_map) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat,  
        color = ~Couleur_echelle,                  
        radius = 8,                                
        fillOpacity = 0.8,                         
        stroke = FALSE,                            
        popup = ~paste0("<br><b>Numéro DPE :</b> ", `N.DPE`,
                        "<br><b>Etiquette DPE :</b> ", Etiquette_DPE,
                        "<br><b>Etiquette GES :</b> ", Etiquette_GES,
                        "<br><b>Type de bâtiment :</b> ", Type_bâtiment,
                        "<br><b>Adresse :</b> ", `Adresse_.BAN.`,
                        "<br><b>Longitude :</b> ", lon,
                        "<br><b>Latitude :</b> ", lat),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Graphique : Répartition des étiquettes DPE
  output$histDPE <- renderPlot({
    ggplot(base_filtered(), aes(x = Etiquette_DPE, fill = Etiquette_DPE)) +  # Utilisation de Etiquette_DPE pour le remplissage
      geom_bar() +
      scale_fill_manual(values = c(
        "A" = "darkgreen",  # Vert foncé
        "B" = "green",      # Vert
        "C" = "green",      # Vert
        "D" = "yellow",     # Jaune
        "E" = "orange",     # Orange
        "F" = "darkorange", # Orange foncé
        "G" = "red"         # Rouge
      )) +
      theme_minimal() +
      labs(title = "Répartition des étiquettes DPE", x = "Etiquette DPE", y = "Nombre de logements")
  })
  
  # Graphique : Répartition des types de bâtiments
  output$pieType <- renderPlot({
    ggplot(base_filtered(), aes(x = "", fill = Type_bâtiment)) +
      geom_bar(width = 1) +
      coord_polar("y") +
      theme_minimal() +
      labs(title = "Répartition des types de logements")
  })
  
  output$energieType <- renderPlot({
    ggplot(base_filtered(), aes(x = Etiquette_GES, fill = Etiquette_GES)) +  # Utilisation de Etiquette_GES pour le remplissage
      geom_bar() +
      scale_fill_manual(values = c(
        "A" = "#ADD8E6",  # Bleu clair
        "B" = "#87CEEB",  # Bleu ciel
        "C" = "#1E90FF",  # Bleu dodger
        "D" = "#0000FF",  # Bleu foncé
        "E" = "#8A2BE2",  # Bleu violet
        "F" = "#4B0082",  # Indigo
        "G" = "#2E004F"   # Violet/noir foncé
      )) +
      theme_minimal() +
      labs(title = "Répartition des étiquettes GES", x = "Etiquette GES", y = "Nombre de logements")
  })
  output$surfaceDist <- renderPlot({
    # Calcul de la moyenne de la consommation par étiquette DPE
    conso_moyenne <- base_filtered() %>%
      group_by(Etiquette_DPE) %>%
      summarise(Conso_moyenne = mean(Conso_5_usages_é_finale, na.rm = TRUE))
    
    # Graphique en barres
    ggplot(conso_moyenne, aes(x = Etiquette_DPE, y = Conso_moyenne, fill = Etiquette_DPE)) +
      geom_bar(stat = "identity") +  # Utilisation de stat = "identity" pour des valeurs prédéfinies
      scale_fill_manual(values = c(
        "A" = "darkgreen",  # Vert foncé
        "B" = "green",      # Vert
        "C" = "green",      # Vert
        "D" = "yellow",     # Jaune
        "E" = "orange",     # Orange
        "F" = "darkorange", # Orange foncé
        "G" = "red"         # Rouge
      )) +
      theme_minimal() +
      labs(
        title = "Répartition de la consommation par étiquette DPE",
        x = "Etiquette DPE",
        y = "Consommation moyenne (kWh/m².an)"
      )
  })
  # Filtrage des données pour la carte
  base_filtered_carte <- reactive({
    data <- base
    # Filtre par code postal
    if (!is.null(input$code_postal) && input$code_postal != "") {
      data <- data %>% filter(Code_postal_.BAN. == input$code_postal)
    }
    return(data)
  })
  
  # KPIs pour la carte
  output$kpi_nb_dpe_carte <- renderInfoBox({
    infoBox(
      title = "Nombre de DPE",
      value = nrow(base_filtered_carte()),
      icon = icon("file"),
      color = "blue",
      fill = TRUE
    )
  })
  
  output$kpi_cout_chauffage_carte <- renderInfoBox({
    infoBox(
      title = "Moyenne du coût de chauffage/an",
      value = paste(round(mean(as.numeric(base_filtered_carte()$Coût_chauffage), na.rm = TRUE), 2), '€', sep = " "),
      icon = icon("fire"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$kpi_emission_co2_carte <- renderInfoBox({
    infoBox(
      title = "Moyenne des émissions de CO2/an",
      value = paste(round(mean(as.numeric(base_filtered_carte()$Emission_GES_5_usages), na.rm = TRUE), 2), "kg de CO2/an", sep = " "),
      icon = icon("leaf"),
      color = "green",
      fill = TRUE
    )
  })
  
  # Carte interactive
  output$map <- renderLeaflet({
    base_filtered_map <- base_filtered_carte() %>% filter(!is.na(lon) & !is.na(lat))
    leaflet(base_filtered_map) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat,  
        color = ~Couleur_echelle,                  
        radius = 8,                                
        fillOpacity = 0.8,                         
        stroke = FALSE,                            
        popup = ~paste0("<br><b>Numéro DPE :</b> ", `N.DPE`,
                        "<br><b>Etiquette DPE :</b> ", Etiquette_DPE,
                        "<br><b>Etiquette GES :</b> ", Etiquette_GES,
                        "<br><b>Type de bâtiment :</b> ", Type_bâtiment,
                        "<br><b>Adresse :</b> ", `Adresse_.BAN.`,
                        "<br><b>Longitude :</b> ", lon,
                        "<br><b>Latitude :</b> ", lat),
        clusterOptions = markerClusterOptions()
      )
  })
  
  
  #Corrélation
  # Graphique de corrélation
  output$scatterPlot <- renderPlot({
    # Vérifier que les variables sélectionnées sont valides
    if (!is.null(input$x) && !is.null(input$y)) {
      # Créer le graphique de dispersion
      ggplot(base_filtered(), aes(x = .data[[input$x]], y = .data[[input$y]])) +
        geom_point(color = "steelblue", alpha = 0.6) +  # Points avec transparence
        geom_smooth(method = "lm", color = "red", se = FALSE) +  # Ligne de régression linéaire
        theme_minimal() +
        labs(
          title = paste("Corrélation entre", input$x, "et", input$y),
          x = input$x,
          y = input$y
        )
    }
  })
  
  # Gestion du tirage aléatoire des variables
  observeEvent(input$refresh, {
    # Sélectionner deux variables aléatoires
    variables <- names(base)[sapply(base, is.numeric)]  # Sélectionner uniquement les variables numériques
    updateSelectInput(session, "x", selected = sample(variables, 1))
    updateSelectInput(session, "y", selected = sample(variables, 1))
  })
  # Téléchargement des données au format CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("donnees_dpe_", Sys.Date(), ".csv", sep = "")  # Nom du fichier avec la date
    },
    content = function(file) {
      write.csv(base_filtered(), file, row.names = FALSE)  # Écriture des données filtrées dans un fichier CSV
    }
  )
  # Tableau de données
  output$dataTable <- renderDataTable({
    datatable(base_filtered(), options = list(pageLength = 10))
  })
}

# Lancement de l'application
shinyApp(ui, server)