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

# READ .xlsx files
# xlsx est le plus lent, benchmark:
# https://github.com/rifset/finding-fastest-excel-reader-in-r
library("readxl")
#library("writexl")
#library(Microsoft365R)
library(duckdb)
 
library(DT)
#library(data.table)


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
# # Supprimer les lignes avec des valeurs manquantes dans GENUS ou TAXON
# df <- readxl::read_excel("./data/test_template2.xlsx")
# #df[, GENUS := gsub(" .*", "", TAXON)]
# df$GENUS <- gsub(" .*", "", df$TAXON)
# 
# df$LATITUDE  <- as.numeric(df$LATITUDE_PRIVACY)
# df$LONGITUDE <- as.numeric(df$LONGITUDE_PRIVACY)
# # Ajout des catégories IUCN
# IUCN <- readxl::read_excel("./data/EU_Bees_list_and_country_records_2023_05_12.xlsx", sheet = "Full data")
# df$category <- IUCN$RL_EU_2015[match(df$TAXON, IUCN$internal_name)]
# 
# 
# df <- mutate(df, popup=paste0(
#   '<strong>Specis: </strong>', TAXON,
#   '<br><strong>ID:</strong> ', REFERENCE_CODE,
#   '<br><strong>Determinator:</strong> ', DETERMINATOR,
#   '<br><strong>Validator:</strong> ', VALIDATOR,
#   '<br><strong>Flower:</strong> ', PLANT_TAXA,
#   '<br><strong>Year:</strong> ', YEAR_2,
#   '<br><strong>Month:</strong> ', MONTH_2,
#   '<br><strong>N =</strong> ', NUMBER_OF_SPECIMENS_2,
#   '<br><strong>Locality:</strong> ', LOCALITY,
#   '<br><strong>Privacy:</strong> ', PRIVACY)
# )
# writexl::write_xlsx(df, "./data/test_template2_clean.xlsx")

# df <- readxl::read_excel("./data/test_template2_clean.xlsx")


# test duckdb ----
# https://wp.sciviews.org/sdd-umons2/?iframe=wp.sciviews.org/sdd-umons2-2023/big-data.html
#con <- DBI::dbConnect(duckdb::duckdb(), dbname = "ma_db_test")

## Première fois : créer database à partir de df :
# # Créer une connexion à DuckDB. Créer le fichier s'il n'existe pas.
# con <- DBI::dbConnect(duckdb::duckdb(dbdir = "dbz.duckdb"))
# DBI::dbWriteTable(con, "table_df", df)
# # Liste des tables dans la base de données DuckDB
# tables <- DBI::dbListTables(con)
# print(tables)
# # Liste des colonnes dans la table "table_df"
# fields <- DBI::dbListFields(con, "table_df")
# print(fields)
# # Lie les données à la connexion pour les manipuler avec dplyr
# db <- tbl(con, "table_df")
# DBI::dbDisconnect(con, shutdown = TRUE)


con <- DBI::dbConnect(duckdb::duckdb(dbdir = "dbz.duckdb"))
# Lie les données à la connexion pour les manipuler avec dplyr
db <- tbl(con, "table_df")



grouped_species <- db %>%
  select(GENUS, TAXON) %>%
  distinct() %>%
  group_by(GENUS) %>%
  summarize(species = list(TAXON)) %>%
  collect()




species_choices <- setNames(grouped_species$species, grouped_species$GENUS)

# Générer les choix pour le menu déroulant
#menuDeroulant <- na.omit(df[c("GENUS", "TAXON")])
#genus_choices <- unique(df[c("GENUS", "TAXON")]) 

# grouped_species <- df %>% 
#   group_by(GENUS) %>% 
#   summarize(species = list(unique(TAXON)))
# species_choices <- setNames(grouped_species$species, grouped_species$GENUS)




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
      selected = "Dasypoda crassicornis",
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
    subset_data <- db %>% 
      filter(TAXON == species) %>%
      collect()  # Collecter les résultats localement
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
  
  
  leaflet() %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant='transport-dark', apikey = 'YOUR-KEY')
    )
  

## carte ----
  output$beeMap <- renderLeaflet({
    jittered_lat <- if (input$jitterInput == 0) filteredData()$LATITUDE else jitter(filteredData()$LATITUDE, amount = as.numeric(input$jitterInput))
    jittered_lon <- if (input$jitterInput == 0) filteredData()$LONGITUDE else jitter(filteredData()$LONGITUDE, amount = as.numeric(input$jitterInput))

    leaflet(data = filteredData()) %>% # Utilise les données filtrées
      setView(lng = 23, lat = 56, zoom = 4) %>% # Vue de départ
      addTiles(group = "OSM (default)") %>% # ajout OpenStreetMap (OSM)
      addProviderTiles(providers$OpenTopoMap, group = "Relief topo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Relief satelitte") %>%
      addProviderTiles(providers$Esri.WorldTerrain, group = "Relief white") %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Night") %>%
      addTiles(
        urlTemplate = "https://{s}.tile.thunderforest.com/spinal-map/{z}/{x}/{y}.png?apikey={apikey}",
        attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
        options = tileOptions(variant='transport-dark', apikey = '8bc51695809642969ef460824c1f9608'),
        group = "Scenario RCP 8.5") %>%
      addTiles(
        urlTemplate = "https://{s}.tile.thunderforest.com/mobile-atlas/{z}/{x}/{y}.png?apikey={apikey}",
        attribution = '&copy; <a href="http://www.thunderforest.com/">Thunderforest</a>, &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        options = tileOptions(variant='transport-dark', apikey = '8bc51695809642969ef460824c1f9608'),
        group = "Light") %>%
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
                       stroke = FALSE, fillOpacity = 0.5) %>%
      addLegend(pal = pal, values = ~category, opacity = 1, title = "IUCN category") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Center view", # image bouton pour centrer
        onClick = JS("function(btn, map){map.setView([56,  23], 4)}") # Vue bouton centrer
      )) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", 
                       "Light", 
                       "Relief topo", 
                       "Relief satelitte", 
                       "Relief white",
                       "Night",
                       "Scenario RCP 8.5"),
        overlayGroups = c("PULSE", "WESTBEES"),
        options = layersControlOptions(collapsed = TRUE)) %>% # Contrôle pour afficher/masquer les shapefiles
      htmlwidgets::onRender(" 
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
    select(filteredData(), -popup),
    extensions = 'Buttons',options = list(ordering = FALSE, searching = FALSE, pageLength = 25),
    filter = "top" # Activer la recherche par colonne
  ))
  
  # Déconnexion de la base de données
  onStop(function() {
    DBI::dbDisconnect(con, shutdown = TRUE)
  })
 
  
})


# Launch ShinyApp ----
# Lancer l'application Shiny
shinyApp(ui = ui, server = server)


