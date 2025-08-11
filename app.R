#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages(c("leaflet", "sf", "openxlsx", "leaflet.extras"))

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(openxlsx)
library(leaflet.extras)
library(ggplot2)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Application d'Échantillonnage"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calcul", tabName = "main", icon = icon("calculator")),
      menuItem("Tirage de Points", tabName = "sampling", icon = icon("map-marked")),
      menuItem("Aide Méthodes", tabName = "help", icon = icon("info-circle")),
      menuItem("À propos du concepteur", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
      #download_sample, #download_results {
        background-color: #adc4ad !important;
        color: white !important;
        border: 1px solid #00561b !important;
        margin-top: 10px;
      }
      #download_sample:hover, #download_results:hover {
        background-color: #00561b !important;
      }
    ")),
    
    tabItems(
      # Onglet principal : Calcul
      tabItem(tabName = "main",
              fluidRow(
                column(width = 4,
                       h2("Bienvenue dans l'application"),
                       uiOutput("real_time"), # Affiche la date et l'heure en temps réel
                       h2("Paramètres de calcul"),
                       numericInput("N", "Taille population", value = 1000, min = 1),
                       numericInput("conf", "Niveau de confiance (%)", value = 95, min = 1, max = 99),
                       numericInput("e", "Marge d'erreur (%)", value = 5, min = 0.1, max = 30),
                       numericInput("p", "Proportion (p)", value = 0.5, min = 0.01, max = 0.99, step = 0.01),
                       numericInput("buffer", "Buffer (%)", value = 10, min = 0, max = 30, step = 1),
                       selectInput("method", "Méthode de calcul", 
                                   choices = c("Swartz" = "swartz",
                                               "Aléatoire simple (Cochran)" = "srs",
                                               "Stratifié (allocation proportionnelle)" = "strat",
                                               "Échantillonnage par grappes" = "cluster",
                                               "Échantillonnage systématique" = "system",
                                               "Puissance statistique" = "power")),
                       actionButton("calc", "Calculer", icon = icon("calculator"))
                ),
                column(width = 8,
                       h3("Résultats"),
                       uiOutput("result"),
                       downloadButton("download_results", "Télécharger les résultats en Excel", style="position:right"),
                       plotOutput("plot")
                )
              )
      ),
      
      
      # Onglet Aide Méthodes
      tabItem(tabName = "help",
              h2("Aide aux Méthodes d'Échantillonnage"),
              div(style = "border: 1px solid #ddd; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); padding: 15px; margin-bottom: 20px;",
                  h3("Définition de l'échantillonnage", style="font-weight:bold;"),
                  p(style = "text-align:justify; font-size:15px;", "Pour des raisons de coûts et de délai, il n'est pas toujours possible d'étudier l'ensemble de 
        la population. Il est alors nécessaire de créer un échantillon représentatif de la population 
        permettant ainsi de fournir une estimation aussi précise que possible d'une variable. La question 
        de la taille de l'échantillon dépend de la contrainte de coût."),
                  p(style = "text-align:justify; font-size:15px;", "Si cette contrainte est forte, l'échantillon correspond au rapport entre le budget global alloué
        à l'étude et le coût unitaire d'une enquête. À l'inverse, si la contrainte de coût est faible, la
        taille de l'échantillon est liée à la précision souhaitée a priori de l'estimation. En effet, on ne
        peut pas dire qu'une certaine taille d'échantillon est suffisante pour étudier une population d'une
        certaine taille. La taille de l'échantillon se définit nécessairement par rapport au degré de précision
        exigé par l'enquêteur et au phénomène que l'on veut étudier."),
                  p(style = "text-align:justify; font-size:15px;", "De manière générale, plus l’échantillon est grand et plus l’estimation sera précise. La notion de
précision est matérialisée par un seuil de confiance (en général 95%) et une marge d’erreur. Par
exemple si l’on définit un seuil de confiance de 95% et une marge d’erreur de 2%, cela signifie que
l’échantillon permettra d’extrapoler le résultat avec 5% de risques de se tromper de plus ou moins
2%.")),
              div(style = "border: 1px solid #ddd; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); padding: 15px; margin-bottom: 20px;",
                h3("À quoi sert le Buffer ?", style="font-weight:bold;"),
                p("Le buffer (ou marge de sécurité) est un paramètre crucial dans les études statistiques pour garantir la fiabilité des résultats."),
                p("Le buffer est une marge de sécurité qui :"),
                tags$ul(
                  tags$li("Compense les non-réponses et données manquantes"),
                  tags$li("Prévient les imprévus lors de la collecte"),
                  tags$li("Garantit que la taille finale reste suffisante")
                ),
                p("Exemple : un buffer de 10% augmente la taille théorique de 11%"),
                p("Valeurs typiques :"),
                tags$ul(
                  tags$li("Enquêtes standards : 5-10%"),
                  tags$li("Zones difficiles : 15-20%"),
                  tags$li("Groupes vulnérables : 20-25%")
                )
              ),
                  
              selectInput("selected_method", "Choisir une méthode :",
                          choices = c("Méthode de Swartz" = "swartz",
                                      "Aléatoire simple (Cochran)" = "srs",
                                      "Stratifié (allocation proportionnelle)" = "strat",
                                      "Échantillonnage par grappes" = "cluster",
                                      "Échantillonnage systématique" = "system",
                                      "Puissance statistique" = "power")),
              uiOutput("method_details")
      ),
      
      # Nouvel onglet Tirage de Points
      tabItem(tabName = "sampling",
              h2("Tirage aléatoire de points d'échantillonnage"),
              fluidRow(
                column(4,
                       fileInput("zone_file", "Charger la zone d'étude (KML/GPKG)",
                                 accept = c(".kml", ".gpkg")),
                       actionButton("sample_btn", "Générer les points", class = "btn-primary"),
                       downloadButton("download_sample", "Exporter les résultats")
                ),
                column(8,
                       leafletOutput("sampling_map", height = "600px")
                )
              )
      ),
      
      # Onglet À propos du concepteur
      tabItem(tabName = "about",
              h2("À propos du concepteur"),
              fluidRow(
                column(width = 12,
                       div(style = "text-align: center;",
                           img(src = "logo.png", height = 100, width = 100), # Votre logo
                           h3("Technologies Des SIG Appliqués",style = "font-weight:bold;"),
                           p(style = "flex:1; text-align:justify; font-size:20px;", "Nous sommes un cabinet de consultation en Système d'Information Géographique et Télédétection."),
                           p(style = "flex:1; text-align:justify; font-size:20px;", "Nous intervenons dans plusieurs domaine de la gestion de l'information. Technologies des SIG Appliqués
                             offrent les services suivants : 
                               Je suis un data scientist passionné par les statistiques et le développement d'applications interactives.
							 Cette application a été conçue pour simplifier les calculs de taille d'échantillon pour des études statistiques."),
                           
                           
                           # Section Services
                           h3("Services Offerts", style = "margin-top: 30px; font-weight:bold;"),
                           fluidRow(
                             column(3,
                                    div(style = "border: 2px solid #4CAF50; border-radius: 10px; padding: 15px; height: auto; transition: transform 0.3s, border-color 0.3s;",
                                        style = ":hover { transform: scale(1.05); border-color: #45a049; }",
                                        img(src = "https://cdn-icons-png.flaticon.com/512/1534/1534959.png", 
                                            height = 100, width = 100),
                                        h5("Cartographie & Analyse Spatiale", style ="font-weight:bold;"),
                                        p(style = "flex:1; text-align:justify; font-size:15px;",
                                          tags$ul(style = "text-align:justify; font-size:15px;",
                                                  tags$li("Création de cartes thématiques"),
                                                  tags$li("Analyse de l’accessibilité aux infrastructures (écoles, centres de santé)."),
                                                  tags$li("Cartographie des zones à risque d’inondation, érosion hydrique, etc."),
                                                  tags$li("Analyse spatiale à l’aide de données géospatiales."),
                                                  tags$li("Production de cartes interactives Web (Leaflet, QGIS2Web).")
                                          )
                                        )
                                    )
                             ),
                             column(3,
                                    div(style = "border: 2px solid #2196F3; border-radius: 10px; padding: 15px; height: auto; transition: transform 0.3s, border-color 0.3s;",
                                        style = ":hover { transform: scale(1.05); border-color: #1e88e5; }",
                                        img(src = "https://cdn-icons-png.flaticon.com/512/1534/1534966.png", 
                                            height = 100, width = 100),
                                        h5("Télédétection & Modélisation", style ="font-weight:bold;"),
                                        p(style = "flex:1; text-align:justify; font-size:15px;",
                                          tags$ul(style = "text-align:justify; font-size:15px;",
                                                  tags$li("Traitement d'images satellitaires"),
                                                  tags$li("Cartographie de la déforestation et de la couverture du sol."),
                                                  tags$li("Analyse de l’évolution de l’occupation des sols"),
                                                  tags$li("Détection des zones urbaines en expansion et des zones succeptibles à l'inondation par imagerie satellite."),
                                                  tags$li("Modélisation de l’érosion des sols ou de la disponibilité en eau."),
                                                  tags$li("Modelisation prédictive de l'occupation des sols, de l'expension urbian")
                                          ))
                                    )
                             ),
                             column(3,
                                    div(style = "border: 2px solid #9C27B0; border-radius: 10px; padding: 15px; height: auto; transition: transform 0.3s, border-color 0.3s;",
                                        style = ":hover { transform: scale(1.05); border-color: #8e24aa; }",
                                        img(src = "https://cdn-icons-png.flaticon.com/512/1534/1534982.png", 
                                            height = 100, width = 100),
                                        h5("Traitement & Analyse de Données", style ="font-weight:bold;"),
                                        p(style = "flex:1; text-align:justify; font-size:15px;",
                                          tags$ul(style = "text-align:justify; font-size:15px;",
                                                  tags$li("Elaboration et implémentation d'outils de collecte de données"),
                                                  tags$li("Nettoyage et structuration de bases de données d’enquêtes terrain."),
                                                  tags$li("Analyse des données"),
                                                  tags$li("Visualisation interactive des données (dashboards, graphiques, cartes)."),
                                                  tags$li("Analyse de la qualité des données collectées (détection d’anomalies)."),
                                                  tags$li("Croisement de données spatiales et statistiques (géostatistique).")
                                          ))
                                    )
                             ),
                             column(3,
                                    div(style = "border: 2px solid #FF9800; border-radius: 10px; padding: 15px; height: auto; transition: transform 0.3s, border-color 0.3s;",
                                        style = ":hover { transform: scale(1.05); border-color: #fb8c00; }",
                                        img(src = "https://cdn-icons-png.flaticon.com/512/1534/1534973.png", 
                                            height = 100, width = 100),
                                        h5("Formation & Appuie technique", style ="font-weight:bold;"),
                                        p(style = "flex:1; text-align:justify; font-size:15px;",
                                          tags$ul(style = "text-align:justify; font-size:15px;",
                                                  tags$li("Ateliers de formation sur QGIS, ArcGIS, ou R/Excel"),
                                                  tags$li("Développement de modules de formation en ligne pour l’analyse de données"),
                                                  tags$li("Support technique pour la mise en place de systèmes d’information géographique (SIG)."),
                                                  tags$li("Assistance au développement d'applications Web SIG personnalisées.")
                                          ))
                                    )
                             )
                           ),
                           
                           # Section Contact
                           
                           h4("Statistiques de la plateforme"),
                           p("Nombre de visiteurs :", textOutput("visitor_count", inline = TRUE)),
                           hr(),
                           p(style = "font-size: 12px; color: #888;", 
                             "© Technologies Des SIG, Février 2025")
                       )
                )
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Variables réactives
  spatial_data <- reactiveVal(NULL)  # Stocke les données spatiales chargées
  sampled_points <- reactiveVal(NULL)  # Stocke les points échantillonnés
  sample_size_value <- reactiveVal(0)  # Stocke la taille de l'échantillon calculée
  sample_theorique <- reactiveVal(0)  # Stocke la taille théorique
  
  # Fonction pour afficher l'heure en temps réel
  output$real_time <- renderUI({
    invalidateLater(1000, session) # Rafraîchit toutes les secondes (1000 ms)
    tags$div(
      style = "font-size: 20px; font-weight: bold; color: #2c7fb8;",
      format(Sys.time(), "%d-%m-%Y %H:%M:%S") # Format : 21-02-2025 13:35:44
    )
  })
  
  # Calcul de la taille d'échantillon
  observeEvent(input$calc, {
    req(input$N, input$conf, input$e, input$p, input$method, input$buffer)
    
    N <- input$N
    z <- qnorm(1 - (1 - input$conf/100)/2)
    e <- input$e/100
    p <- input$p
    buffer <- input$buffer/100  # Convertir le pourcentage en décimale
    
    tryCatch({
      n_theorique <- switch(input$method,
                            "swartz" = {
                              n0 <- (z^2 * p*(1-p)) / e^2
                              ceiling(n0 / (1 + (n0-1)/N))
                            },
                            "srs" = { # Aléatoire simple (Cochran)
                              n0 <- (z^2 * p*(1-p)) / e^2
                              ceiling(n0 / (1 + (n0-1)/N))
                            },
                            "strat" = { # Stratifié (allocation proportionnelle)
                              n0 <- (z^2 * p*(1-p)) / e^2
                              n <- ceiling(n0 / (1 + (n0-1)/N))
                              n*1.2 # Augmentation de 20% pour illustration
                            },
                            "cluster" = { # Échantillonnage par grappes
                              n0 <- (z^2 * p*(1-p)) / e^2
                              n <- ceiling(n0 / (1 + (n0-1)/N))
                              n*1.5 # Augmentation de 50% pour effet cluster
                            },
                            "system" = { # Échantillonnage systématique
                              n0 <- (z^2 * p*(1-p)) / e^2
                              ceiling(n0 / (1 + (n0-1)/N))
                            },
                            "power" = { # Puissance statistique
                              n0 <- (z^2 * p*(1-p)) / e^2
                              ceiling(n0 / (1 + (n0-1)/N)) * 1.1 # Augmentation de 10% pour la puissance
                            })
      
      # Stocker la taille théorique
      sample_theorique(n_theorique)
      
      # Calculer la taille finale avec buffer
      n_final <- ceiling(n_theorique / (1 - buffer))
      
      # Mettre à jour la valeur réactive
      sample_size_value(n_final)
      
      # Affichage du résultat
      output$result <- renderUI({
        HTML(paste0(
          "<strong>Taille d'échantillon théorique : ", n_theorique, "</strong><br>",
          "<strong>Taille d'échantillon avec buffer (", input$buffer, "%) : ", n_final, "</strong>"
        ))
      })
      
    }, error = function(e) {
      showNotification(paste("Erreur de calcul :", e$message), type = "error")
    })
  })
  
  # Chargement du fichier de zone (KML/GeoPackage)
  observeEvent(input$zone_file, {
    req(input$zone_file)
    
    tryCatch({
      # Chargement avec suppression des coordonnées Z/M
      zone <- st_read(input$zone_file$datapath) %>% 
        st_zm(drop = TRUE, what = "ZM") %>% # Supprime les coordonnées 3D
        st_make_valid() # Rend les géométries valides
      
      # Validation renforcée
      validate(
        need(inherits(zone, "sf"), "Format de fichier invalide"),
        need(nrow(zone) > 0, "Aucune géométrie trouvée"),
        need(all(st_is_valid(zone)), "Contient des géométries invalides")
      )
      
      spatial_data(zone)
    }, error = function(e) {
      showNotification(paste("Erreur spatiale :", e$message), type = "error")
      spatial_data(NULL)
    })
  })
  
  # Carte interactive sécurisée
  output$sampling_map <- renderLeaflet({
    req(spatial_data())
    
    validate(
      need(inherits(spatial_data(), "sf"), "Données spatiales invalides"),
      need(nrow(spatial_data()) > 0, "Aucune donnée chargée"),
      need(any(st_geometry_type(spatial_data()) %in% c("POLYGON", "MULTIPOLYGON")), 
           "Seuls les polygones sont supportés")
    )
    
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      {
        if(!is.null(spatial_data()) && all(st_is_valid(spatial_data()))) {
          addPolygons(., data = spatial_data(), 
                      color = "#FF0000",
                      weight = 2,
                      fillOpacity = 0.5)
        } else .
      } %>%
      {
        if(!is.null(sampled_points()) && nrow(sampled_points()) > 0) {
          addCircleMarkers(., data = sampled_points(),
                           radius = 5,
                           color = "#0000FF",
                           popup = ~paste("Ménage:", menage_id, "<br>X:", x_menage, "<br>Y:", y_menage))
        } else .
      } %>%
      addLayersControl(
        baseGroups = c("OSM", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Génération des points aléatoires
  observeEvent(input$sample_btn, {
    req(spatial_data(), sample_size_value() > 0)
    
    tryCatch({
      points <- st_sample(spatial_data(), size = sample_size_value(), type = "random")
      coords <- st_coordinates(points)
      
      sampled_data <- st_sf(
        menage_id = 1:sample_size_value(),
        x_menage = coords[,1],
        y_menage = coords[,2],
        geometry = points
      )
      
      sampled_points(sampled_data)
      showNotification("Points générés avec succès !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lors du tirage des points :", e$message), type = "error")
      sampled_points(NULL)  # Réinitialiser les points échantillonnés
    })
  })
  
  # Export des résultats en Excel
  output$download_sample <- downloadHandler(
    filename = function() {
      paste("echantillon_menages_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(sampled_points())
      df <- st_drop_geometry(sampled_points())
      write.xlsx(df, file)
    }
  )
  
  #Téléchargement du résultat en xlsx
  output$download_results <- downloadHandler(
    filename = function() {
      paste("resultats_calcul_echantillon_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Utiliser les variables réactives directement
      results_df <- data.frame(
        `Taille de la population` = input$N,
        `Niveau de confiance (%)` = input$conf,
        `Marge d'erreur (%)` = input$e,
        `Proportion (p)` = input$p,
        `Buffer (%)` = input$buffer,
        `Méthode de calcul` = input$method,
        `Taille d'échantillon théorique` = sample_theorique(),
        `Taille d'échantillon avec buffer` = sample_size_value(),
        check.names = FALSE
      )
      
      # Créer un fichier Excel
      write.xlsx(results_df, file, rowNames = FALSE)
    }
  )
  
  # Graphique
  output$plot <- renderPlot({
    margins <- seq(1, 30, by = 1)/100
    sizes <- sapply(margins, function(e) {
      n0 <- (qnorm(1 - (1 - input$conf/100)/2)^2 * input$p*(1-input$p)) / (e^2)
      ceiling(n0 / (1 + (n0-1)/input$N))
    })
    
    ggplot(data.frame(Marge = margins, Taille = sizes), aes(x = Marge, y = Taille)) +
      geom_line(color = "#2c7fb8") +
      geom_point(color = "#e41a1c") +
      labs(title = "Taille d'échantillon en fonction de la Marge d'erreur",
           x = "Marge d'erreur", y = "Taille d'échantillon") +
      theme_minimal()
  })
  
  
  # Explications des méthodes
  output$method_explanations <- renderUI({
    
    h3("Détails des Méthodes d'Échantillonnage", style="font-weight:bold;")
    lapply(names(content), function(m) {
      div(style = "border: 1px solid #ddd; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); padding: 15px; margin-bottom: 20px;",
          h4(switch(m,
                    "swartz" = "Méthode de Swartz",
                    "srs" = "Aléatoire simple (Cochran)",
                    "strat" = "Stratifié (allocation proportionnelle)",
                    "cluster" = "Échantillonnage par grappes",
                    "system" = "Échantillonnage systématique",
                    "power" = "Puissance statistique")),
          content[[m]]
      )
    })
    
  })
  
  # Contenu des explications
  content <- list(
    "swartz" = withMathJax(
      h4(style="font-weight:bold;font-size:15px;", "Méthode de Swartz"),
      p(style="font-weight:bold;font-size:15px;", "Formule :"),
      "$$ n_0 = \\frac{Z^2 p(1-p)}{e^2} $$",
      p(style = "text-align:justify; font-size:15px;", "Correction population finie :"),
      "$$ n = \\frac{n_0}{1 + \\frac{n_0-1}{N}} $$",
      h5(style="font-weight:bold;font-size:15px;", "Composantes :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Z : Score Z correspondant au niveau de confiance (ex: 1.96 pour 95%)"),
              tags$li("p : Proportion estimée dans la population (0.5 par défaut)"),
              tags$li("e : Marge d'erreur acceptable (ex: 0.05 pour 5%)"),
              tags$li("N : Taille de la population"),
              tags$li("n_0 : Taille d'échantillon initiale sans correction"),
              tags$li("n : Taille d'échantillon finale avec correction")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Forces :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Simple et rapide à appliquer"),
              tags$li("Convient bien aux études de prévalence"),
              tags$li("Basée sur une approche statistique classique")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Faiblesses :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Ne prend pas en compte la taille finie de la population sans correction"),
              tags$li("Suppositions fortes sur la normalité et l’estimation de 
𝑝
p"),
              tags$li("Ne tient pas compte de la puissance statistique")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Domaines d'application :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Études préliminaires, enquêtes rapides"),
              tags$li("Études de prévalence en épidémiologie"),
              tags$li("Enquêtes de marché et sondages d’opinion"),
              tags$li("Estimation de la proportion d’un phénomène dans une population")
      )
    ),
    
    "srs" = withMathJax(
      h4(style="font-weight:bold;font-size:15px;", "Aléatoire simple (Cochran)"),
      p(style = "text-align:justify; font-size:15px;",
        "Formule de Cochran (pour les grandes populations et études descriptives)"),
      p(style="font-weight:bold;font-size:15px;", "Formule :"),
      "$$ n_0 = \\frac{Z^2 p(1-p)}{e^2} $$",
      p("Correction population finie :"),
      "$$ n = \\frac{n_0}{1 + \\frac{n_0-1}{N}} $$",
      h5(style="font-weight:bold;font-size:15px;", "Composantes :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Z : Score Z (1.96 pour 95% de confiance)"),
              tags$li("p : Proportion estimée (0.5 si inconnue)"),
              tags$li("e : Marge d'erreur (précision souhaitée)"),
              tags$li("N : Taille de la population"),
              tags$li("n_0 : Taille initiale pour population infinie"),
              tags$li("n : Taille ajustée pour population finie")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Forces :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Méthode standard, largement acceptée"),
              tags$li("Adaptée aux grandes populations"),
              tags$li("Applicable à diverses études quantitatives"),
              tags$li("Permet une bonne estimation des proportions")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Faiblesses :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Difficile à atteindre en pratique."),
              tags$li("Nécessite une liste précise de toute la population."),
              tags$li("Coûteux du fait de la dispersion des échantillons sur la zone entière."),
              tags$li("Peut être inefficace pour les populations hétérogènes"),
              tags$li("Même limitations que Swartz concernant 
𝑝
p et la normalité "),
              tags$li("Peu adapté aux comparaisons de groupes")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Domaines d'application :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Enquêtes générales, études transversales"),
              tags$li("Études démographiques et statistiques générales"),
              tags$li("Sondages d’opinion et enquêtes de satisfaction"),
              tags$li("Études de santé publique")
      )
    ),
    
    "strat" = withMathJax(
      h4(style="font-weight:bold;font-size:15px;", "Stratifié (allocation proportionnelle)"),
      p(style = "text-align:justify; font-size:15px;",
        "Nous divisons une population hétérogène, qui présente des caractéristiques différentes au sein 
      du groupe (âge, sexe, classessociales ...) en strates homogènes (sous-ensemble avec une 
      caractéristique commune) appelés strates afin de trouver dans l'échantillon chaque strate de la population de base."),
      p(style="font-weight:bold;font-size:15px;", "Formule :"),
      "$$ n_h = n \\times \\frac{N_h}{N} $$",
      h5(style="font-weight:bold;font-size:15px;", "Composantes :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("n_h : Taille d'échantillon pour la strate h"),
              tags$li("n : Taille d'échantillon totale"),
              tags$li("N_h : Taille de la population dans la strate h"),
              tags$li("N : Taille totale de la population")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Forces :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Permet de faire des observations dans des zones plus accessibles, sans biaiser le résultat. Permet de réduire 
        la variabilité dans l'échantillon à moins que la moyenne des strates ne soit la même"),
              tags$li("Améliore la précision pour les populations hétérogènes"),
              tags$li("Meilleure précision que l’échantillonnage aléatoire simple"),
              tags$li("Permet de comparer des sous-populations"),
              tags$li("Réduit la variabilité intra-groupe")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Faiblesses :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Suppose l’existence d’une liste de la population."),
              tags$li("Il faut connaître comment cette population est répartie (strates). En cas d’erreur le biais peut être important"),
              tags$li("Nécessite une connaissance préalable des strates"),
              tags$li("Nécessite des informations préalables sur la population"),
              tags$li("Peut être complexe à mettre en œuvre")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Domaines d'application :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Études démographiques, enquêtes sectorielles"),
              tags$li("Études socio-économiques avec segmentation (revenu, âge, sexe, etc.)"),
              tags$li("Études de marché avec différents segments de clientèle"),
              tags$li("Études de santé publique par catégories de population")
      )
    ),
    
    "cluster" = withMathJax(
      h4(style="font-weight:bold;font-size:15px;", "Méthode par Grappes (Cluster Sampling)"),
      p(style = "text-align:justify; font-size:15px;",
        "On divise la population en grappes (sous-groupes hétérogènes) et un certain nombre de grappes sont sélectionnées au hasard pour représenter la population. Tous 
      les individus ou un échantillon d'individus de chaque groupe est ensuite étudié, interviewé, etc."),
      p(style="font-weight:bold;font-size:15px;", "Formule :"),
      "$$ n_{cluster} = n \\times DEFF $$",
      p("DEFF (Design Effect) :"),
      "$$ DEFF = 1 + (m-1)\\rho $$",
      h5(style="font-weight:bold;font-size:15px;", "Composantes :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("n_{cluster} : Taille d'échantillon ajustée pour grappes"),
              tags$li("n : Taille d'échantillon calculée par méthode standard"),
              tags$li("DEFF : Effet de plan (généralement entre 1.5 et 2.0)"),
              tags$li("m : Taille moyenne des grappes"),
              tags$li("\\rho : Coefficient de corrélation intra-classe")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Forces :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Réduit les coûts de collecte de données"),
              tags$li("Moins coûteux et plus facile à réaliser"),
              tags$li("Utile lorsque les listes complètes de la population ne sont pas disponibles"),
              tags$li("Réduit le travail de collecte des données")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Faiblesses :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Moins précis que les autres méthodes"),
              tags$li("Moins précis que la stratification"),
              tags$li("Risque de biais si les grappes ne sont pas homogènes entre elles")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Domaines d'application :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Études à grande échelle, enquêtes nationales"),
              tags$li("Études géographiques (villages, quartiers)"),
              tags$li("Enquêtes scolaires (sélection d’écoles, puis d’élèves)"),
              tags$li("Études de populations difficiles à atteindre")
      )
    ),
    
    "system" = withMathJax(
      h4(style="font-weight:bold;font-size:15px;", "Méthode Systématique"),
      p(style = "text-align:justify; font-size:15px;", "On sélectionne les individus à intervalles réguliers dans une liste ordonnée."),
      p("Formule :", style="font-weight:bold;font-size:15px;"),
      "$$ k = \\frac{N}{n} $$",
      h5(style="font-weight:bold;font-size:15px;", "Composantes :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("k : Intervalle de sélection (pas d'échantillonnage)"),
              tags$li("N : Taille de la population"),
              tags$li("n : Taille d'échantillon souhaitée")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Forces :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Facile à mettre en œuvre"),
              tags$li("Plus rapide que l’échantillonnage aléatoire simple"),
              tags$li("Permet une meilleure dispersion de l’échantillon"),
              tags$li("Facile à mettre en place sur une liste ordonnée")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Faiblesses :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Risque de biais si la population présente une périodicité"),
              tags$li("Moins flexible que les autres méthodes")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Domaines d'application :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Contrôle qualité, études de séries temporelles"),
              tags$li("Enquêtes sur des bases de données structurées"),
              tags$li("Études longitudinales avec échantillons récurrents")
      )
    ),
    
    "power" = withMathJax(
      h4(style="font-weight:bold;font-size:15px;", "Approche par Puissance Statistique (Tests d’Hypothèse)"),
      p(style = "text-align:justify; font-size:15px;",
        "Utilisée lorsqu’on veut comparer des groupes ou tester un effet statistique"),
      p(style="font-weight:bold;font-size:15px;", "Formule :"),
      "$$ n = \\left(\\frac{Z_{\\alpha/2} + Z_{\\beta}}{\\delta}\\right)^2 p(1-p) $$",
      h5(style="font-weight:bold;font-size:15px;", "Composantes :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Z_{\\alpha/2} : Score Z pour l'erreur de type I (généralement 1.96)"),
              tags$li("Z_{\\beta} : Score Z pour l'erreur de type II (généralement 0.84 pour 80% de puissance)"),
              tags$li("\\delta : Taille d'effet minimale détectable"),
              tags$li("p : Proportion de référence"),
              tags$li("n : Taille d'échantillon requise")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Forces :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Permet de détecter des effets plus petits"),
              tags$li("Assure une puissance statistique suffisante"),
              tags$li("Évite les erreurs de type II (ne pas détecter un effet existant")
      ),
      h5(style="font-weight:bold;font-size:15px;", "Faiblesses :"),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Nécessite des hypothèses supplémentaires"),
              tags$li("Plus complexe à calculer"),
              tags$li("Dépend de paramètres souvent inconnus à l’avance")
      ),
      h5(style="font-weight:bold; font-size:15px;", "Domaines d'application :" ),
      tags$ul(style = "text-align:justify; font-size:15px;",
              tags$li("Études cliniques, tests d'hypothèses (Études cliniques comparant traitements et placebos)"),
              tags$li("Tests A/B en marketing"),
              tags$li("Études psychologiques et comportementales")
      )
      
    )
  )
  
  # Affichage dynamique selon la méthode sélectionnée
  output$method_details <- renderUI({
    req(input$selected_method)
    div(style = "border: 1px solid #ddd; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); 
               padding: 15px; margin-top: 20px;",
        content[[input$selected_method]]
    )
  })
  
}


shinyApp(ui, server)
