# New app.R version
# Library ----
# Dans le fichier app.R
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)

library(sf)
library(leaflet)
#library(plotly)
library(rmapshaper) # réduis la précision (et le poids) du shapefile

library(tidyr) # fonction gather, drop_na
library(ggplot2)
library("readxl")
library(DT)


# start_app.R ----
#getwd()
# setwd("app/")
# source("start_app.R")

# Importation  ----
## Shapefile ----
### PULSE shp ----
# Fichier originel et méthode pour réduire la taille
# shp_PULSE <- sf::st_read("data/shapefile/PULSE_shp/FAO_GAUL2014_WGS1984_PanEurope_2022.shp") %>%
#   select(geometry, ADM0_NAME) %>%
#   rename(coutry_name = ADM0_NAME) %>%
#   ms_simplify(keep = 0.09) %>%
#   st_write("data/shapefile/shp_PULSE(compressed).shp")

# shp_PULSE <- sf::st_read("data/shapefile/PULSE_shp/shp_PULSE(compressed).shp")
# 
# ### WESTBEES shp ----
# # shp_WESTBEES <- sf::st_read("data/shapefile/WESTBEES_shp/base mediterranean hotspot.shp") %>%
# #   select(geometry, adm0_name) %>%
# #   rename(coutry_name = adm0_name) %>%
# #   st_zm() %>% # Retire la composante Z (altitude), qui fait buguer leaflet
# #   st_write("data/shp_WESTBEES.shp")
# 
#  shp_WESTBEES <- sf::st_read("data/shapefile/WESTBEES_shp/shp_WESTBEES.shp") 
# 
#readxl::read_excel("./data/SCS_clean.xlsx")

# start_app.R fin ----






# ui.R ----
## new ui.R ----   
# Supprimer les lignes avec des valeurs manquantes dans GENUS ou SP
SCS <- readxl::read_excel("./data/SCS_clean.xlsx")
menuDeroulant <- na.omit(SCS[c("GENUS", "SP")])
# Générer les choix pour le menu déroulant
genus_choices <- unique(menuDeroulant$GENUS)
grouped_species <- SCS %>% 
  group_by(GENUS) %>% 
  summarize(species = list(unique(SP)))

species_choices <- setNames(grouped_species$species, grouped_species$GENUS)



ui <- dashboardPage(
  dashboardHeader(title = "Wild Bees Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = tags$span(tags$img(src = "map.png", height = "20px", width = "20px"),
                         "Map"),
        tabName = "map"),
      menuItem(text = tags$span(tags$img(src = "db.png", height = "20px", width = "20px"),
                                "Data"),
               tabName = "data"),
      menuItem(text = tags$span(tags$img(src = "information.png", height = "20px", width = "20px"),
                                "Read me"),
               tabName = "readme"),
      pickerInput(inputId = "selectSpecies", label = tags$span(tags$img(src = "bee.png", height = "15px", width = "15px"),
        "Select a species:"
      ), choices = c(species_choices),
                  selected = "Megachile parietina",
                  options = list(`live-search` = TRUE)),
      sliderInput("jitterInput", "Jitter Amount", min = 0, max = 0.0005, value = 0.0005, step = 0.0001)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              leafletOutput("beeMap", height = "800px")),
      tabItem(tabName = "data",
              DT::dataTableOutput("beeData")),
      tabItem(tabName = "readme",
              includeMarkdown("README.md"))
    )
  )
)






# server.R ----
## Data ---- 
server <- shinyServer(function(input, output, session) {
  
  # Mesurer le temps avant l'affichage de la carte
  start_time <- Sys.time()
### importation ----  
  # Shapefile
  shp_PULSE <- sf::st_read("data/shapefile/PULSE_shp/shp_PULSE(compressed).shp")
  shp_WESTBEES <- sf::st_read("data/shapefile/WESTBEES_shp/shp_WESTBEES.shp")
    
  # Filtrer les données en fonction de la sélection du menu déroulant
  filteredData <- reactive({
    req(input$selectSpecies)  # S'assurer qu'une sélection est faite
    species <- input$selectSpecies
    # Charger uniquement les données pour l'espèce sélectionnée
    subset_data <- subset(SCS, SP == species)
    return(subset_data)
  })
  
  pal <- colorFactor(pal = c("#B22123", "#F19101", "#F3E61F","#7EB02D", "#54784A", "#CCC9C0"), domain = c("CR", "EN", "VU", "NT", "LC", "DD"))

#### test dynamique slider ----
  # initial_map <- leaflet() %>%
  #   setView(lng = 6.6, lat = 44, zoom = 8) %>%
  #   addTiles()
  # 
  # # Utilisez leafletProxy pour mettre à jour les marqueurs
  # output$beeMap <- renderLeaflet({
  #   leaflet(data = filteredData()) %>%
  #     setView(lng = 6.6, lat = 44, zoom = 8) %>%
  #     addTiles() %>%
  #     addCircleMarkers(data = filteredData(),
  #                      lng = ~ifelse(input$jitterInput == 0, LONGITUDE_DECIMAL, jitter(LONGITUDE_DECIMAL, amount = as.numeric(input$jitterInput))),
  #                      lat = ~ifelse(input$jitterInput == 0, LATITUDE_DECIMAL, jitter(LATITUDE_DECIMAL, amount = as.numeric(input$jitterInput))),
  #                      radius = 5,
  #                      popup = ~popup,
  #                      color = ~pal(category),
  #                      stroke = FALSE,
  #                      fillOpacity = 0.8)
  # })
  # 
  # observeEvent(input$jitterInput, {
  #   proxy <- leafletProxy("beeMap")
  # 
  #   jittered_lat <- if (input$jitterInput == 0) filteredData()$LATITUDE_DECIMAL else jitter(filteredData()$LATITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
  #   jittered_lon <- if (input$jitterInput == 0) filteredData()$LONGITUDE_DECIMAL else jitter(filteredData()$LONGITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
  # 
  #   proxy %>%
  #     clearMarkers() %>%
  #     addCircleMarkers(lng = jittered_lon, lat = jittered_lat,
  #                      radius = 5, popup = ~popup,
  #                      color = ~pal(category),
  #                      stroke = FALSE, fillOpacity = 0.8)
  # })
  
## carte ----
  output$beeMap <- renderLeaflet({
    jittered_lat <- if (input$jitterInput == 0) filteredData()$LATITUDE_DECIMAL else jitter(filteredData()$LATITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
    jittered_lon <- if (input$jitterInput == 0) filteredData()$LONGITUDE_DECIMAL else jitter(filteredData()$LONGITUDE_DECIMAL, amount = as.numeric(input$jitterInput))

    leaflet(data = filteredData()) %>% # Utilise les données filtrées
      setView(lng = 6.6, lat = 44, zoom = 8) %>% # Vue de départ
      addTiles(group = "OSM (default)") %>% # ajout OpenStreetMap (OSM)
      addProviderTiles(providers$Jawg.Light, group = "Light") %>% # ajout d'autres fonds de cartes : http://leaflet-extras.github.io/leaflet-providers/preview/index.html
      addProviderTiles(providers$OpenTopoMap, group = "Relief topo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Relief satelitte") %>%
      addProviderTiles(providers$Esri.WorldTerrain, group = "Relief white") %>%
      addPolygons(data = shp_PULSE,
                  color = "black", # Couleur des contours
                  weight = 2,       # Épaisseur des contours (en pixels)
                  fillColor = "darkred",
                  group = "PULSE") %>%  # Couleur de remplissage des polygones
      addPolygons(data = shp_WESTBEES,
                  color = "black", # Couleur des contours
                  weight = 2,       # Épaisseur des contours (en pixels)
                  fillColor = "#0090a8",
                  group = "WESTBEES") %>%  # Couleur de remplissage des polygones
      addCircleMarkers(lng = ~jittered_lon, lat = ~jittered_lat, # est-ce que j'ajoute des points/clusters ? https://rstudio.github.io/leaflet/markers.html
                       radius = 5, popup = ~popup,
                       color = ~pal(category),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal = pal, values = ~category, opacity = 1, title = "IUCN category") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Center view", # image bouton pour centrer
        onClick = JS("function(btn, map){map.setView([44,  6.8], 10)}") # Vue bouton centrer
      )) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Light", "Relief topo", "Relief satelitte", "Relief white"),
        overlayGroups = c("PULSE", "WESTBEES"),
                       options = layersControlOptions(collapsed = TRUE)) %>% # Contrôle pour afficher/masquer les shapefiles
      htmlwidgets::onRender(" 
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Shapefile:</label>');
        }") %>%  # Ajout du titre de la légende
      hideGroup(c("PULSE", "WESTBEES")) # par défaut masquer les shapefiles
  })

  
  # Calculer le temps écoulé et l'afficher dans la console
  observe({
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    print(paste("Temps pour afficher la carte:", round(as.numeric(elapsed_time), 2), "secondes"))
  })
  
## Data Table ----  
  output$beeData <-DT::renderDataTable(datatable(
    filteredData(), 
    extensions = 'Buttons',
    filter = "top" # Activer la recherche par colonne
  ))
  
 
})


# Launch ShinyApp ----
# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
