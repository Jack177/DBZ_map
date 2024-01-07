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

shp_PULSE <- sf::st_read("data/shapefile/PULSE_shp/shp_PULSE(compressed).shp")

### WESTBEES shp ----
# shp_WESTBEES <- sf::st_read("data/shapefile/WESTBEES_shp/base mediterranean hotspot.shp") %>%
#   select(geometry, adm0_name) %>%
#   rename(coutry_name = adm0_name) %>%
#   st_zm() %>% # Retire la composante Z (altitude), qui fait buguer leaflet
#   st_write("data/shp_WESTBEES.shp")

shp_WESTBEES <- sf::st_read("data/shapefile/WESTBEES_shp/shp_WESTBEES.shp") 

SCS <- readxl::read_excel("./data/peyresq_2022-2023.xlsx")


# SP
SCS <- SCS %>%
  # Créer SP en fusionnant GENUS et SPECIES
  mutate(SP = paste(GENUS, SPECIES, sep = " ")) %>%
  # Mettre la colonne SP en première position
  select(SP, everything())


# dms to decimal ----
data <- SCS

# Fonction pour convertir une chaîne de caractères en degrés décimaux
convert_degrees <- function(coord) {
  parts <- strsplit(coord, "[°'\"]", perl = TRUE)[[1]]
  
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  direction <- substr(parts[4], nchar(parts[4]), nchar(parts[4]))
  
  decimal_degrees <- degrees + minutes/60 + seconds/3600
  
  if (direction %in% c("S", "W")) {
    decimal_degrees <- -decimal_degrees
  }
  
  return(decimal_degrees)
}

# Conversion des latitudes et longitudes en décimales
data$LATITUDE_DECIMAL <- sapply(data$LATITUDE, convert_degrees)
data$LONGITUDE_DECIMAL <- sapply(data$LONGITUDE, convert_degrees)

# Affichage du tibble avec les valeurs converties
# select(data, LONGITUDE, LATITUDE, LATITUDE_DECIMAL,LONGITUDE_DECIMAL)

SCS <- data
rm(data)
SCS$LATITUDE_DECIMAL  <-  as.numeric(SCS$LATITUDE_DECIMAL)
SCS$LONGITUDE_DECIMAL <-  as.numeric(SCS$LONGITUDE_DECIMAL)

## Extraction des jours, mois, année
# Extrait les 4 premiers caractères
SCS$year <- substr(SCS$DATE_2, 1, 4)
# Extrait les caractères 5 à 6
SCS$month <- as.numeric(substr(SCS$DATE_2, 5, 6))
# Extrait les caractères 7 à 8
SCS$day <- substr(SCS$DATE_2, 7, 8)

# Liste des individus incorrects
SP_to_filter <- c("(?) Broken ind. sp",
                  "Chelostoma ? (broken ind.)",
                  "Bombus (bombus ss) sp",
                  "Carabidae sp", 
                  "NA NA",
                  "Chrysura radians", 
                  "Melitta leucozonium")
SPECIES_to_filter <- c("sp",
                       "sp.", NA)

# Retirer les indidividus incorrects
SCS <- SCS %>%
  filter(!SP %in% SP_to_filter) %>%
  filter(!SPECIES %in% SPECIES_to_filter) %>% 
  filter(SPECIMEN_ID != "FRPE0193") # Bombus hortorum avec longitude incorrect (Ukraine)

# Remplacement des noms d'espèces incorrect
SCS$SP[SCS$SP == "Andrena ajzeliella"] <- "Andrena afzeliella"
#SCS$SP[SCS$SP == "Andrena intermedium"] <- "Andrena intermedia"
#SCS$SP[SCS$SP == "Hoplitis cf. lati"] <- "Hoplitis cf. loti"



# Ajout des catégories IUCN
IUCN <- readxl::read_excel("./data/EU_Bees_list_and_country_records_2023_05_12.xlsx", sheet = "Full data")
SCS$category <- IUCN$RL_EU_2015[match(SCS$SP, IUCN$internal_name)]


# Ajout des catégories aux espèces cf., s.l. ou qui ne sont pas repris dans Nieto et al. (2014)
SCS <- SCS %>%
  mutate(category = case_when(
    SP == "Hoplitis cf. loti" ~ "LC",
    SP == "Hylaeus kahri s.l." ~ "LC",
    SP == "Bombus cf. sylvestris" ~ "LC",
    SP == "Anthophora cf. balneorum" ~ "LC",
    SP == "Megachile cf. centuncularis" ~ "LC",
    SP == "Andrena afzeliella" ~ "NA",
    SP == "Hylaeus purpurissatus" ~ "NA",
    SP == "Seladonia tumulorum" ~ "NA",
    TRUE ~ category
  ))

# Transformer en facteur (utile pour plotly)
SCS$category <- factor(SCS$category, levels = c("CR", "EN", "VU", "NT", "LC", "DD", "NA"))
SCS           <-  as.data.frame(SCS)
# Vérification des catégories
#NA_IUCN <- subset(SCS, select = c("SP", "category"), is.na(category))


# Renommer plus simplement
# rename(SCS, "sp" = "TAXPRIO" ) -> SCS

# Remplacement des noms d'espèces désuets
# SCS$sp[SCS$sp == "Bombus (Bombus)  sp."] <- "Terrestribombus  sp."

# start_app.R fin ----



# Conserver seulement les colonnes d'intérêts et arrondir les coordonnées GPS
mutate(SCS, 
       LONGITUDE_text = round(SCS$LONGITUDE_DECIMAL, digits = 2), 
       LATITUDE_text = round(SCS$LATITUDE_DECIMAL, digits = 2)) %>%
  select(SP, GENUS, SEX, LOCALITY, ALTITUDE, LATITUDE_DECIMAL, LONGITUDE_DECIMAL,LONGITUDE_text,LATITUDE_text, DATE_2, category) %>%
  drop_na(LATITUDE_DECIMAL) %>%
  drop_na(SP) -> SCS


# ui.R ----
## ui Peyresq-R ----
# ui <- dashboardPage(
#   dashboardHeader(title = "Interactive map"),
#   dashboardSidebar(
#     selectInput("genreInput", "Genus", choices = c("Select all genus", unique(data$GENUS))),
#     sliderInput("jitterInput", "Jitter Amount", min = 0, max = 0.0005, value = 0.0005, step = 0.0001)
#   ),
#   dashboardBody(
#     fluidRow(
#       column(
#         width = 12,
#         plotlyOutput(outputId = "map", width = "100%", height = "600px")
#       )
#     ),
#     fluidRow(
#       column(
#         width = 12,
#         div(
#           id = "legend",
#           style = "text-align: center; margin-top: 10px;",
#           HTML("<h4>Legend</h4>"),
#           div(
#             style = "display: inline-block; margin-right: 10px;",
#             class = "legend-item",
#             tags$span(
#               style = "background-color: #B22123; color: #FFFFFF; padding: 3px; display: block; width: 160%;",
#               "CR"
#             ),
#             tags$span(
#               style = "background-color: #B22123; color: #FFFFFF; display: block; width: 160%;",
#               sum(!is.na(data$category) & data$category == "CR")
#             )
#           ),
#           div(
#             style = "display: inline-block; margin-right: 10px;",
#             class = "legend-item",
#             tags$span(
#               style = "background-color: #F19101; color: #000000; padding: 3px; display: block; width: 160%;",
#               "EN"
#             ),
#             tags$span(
#               style = "background-color: #F19101; color: #000000; display: block; width: 160%;",
#               sum(!is.na(data$category) & data$category == "EN")
#             )
#           ),
#           div(
#             style = "display: inline-block; margin-right: 10px;",
#             class = "legend-item",
#             tags$span(
#               style = "background-color: #F3E61F; color: #000000; padding: 3px; display: block; width: 160%;",
#               "VU"
#             ),
#             tags$span(
#               style = "background-color: #F3E61F; color: #000000; display: block; width: 160%;",
#               sum(!is.na(data$category) & data$category == "VU")
#             )
#           ),
#           div(
#             style = "display: inline-block; margin-right: 10px;",
#             class = "legend-item",
#             tags$span(
#               style = "background-color: #7EB02D; color: #000000; padding: 3px; display: block; width: 160%;",
#               "NT"
#             ),
#             tags$span(
#               style = "background-color: #7EB02D; color: #000000; display: block; width: 160%;",
#               sum(!is.na(data$category) & data$category == "NT")
#             )
#           ),
#           div(
#             style = "display: inline-block; margin-right: 10px;",
#             class = "legend-item",
#             tags$span(
#               style = "background-color: #54784A; color: #FFFFFF; padding: 3px; display: block; width: 170%;",
#               "LC"
#             ),
#             tags$span(
#               style = "background-color: #54784A; color: #FFFFFF; display: block; width: 170%;",
#               sum(!is.na(data$category) & data$category == "LC")
#             )
#           ),
#           div(
#             style = "display: inline-block; margin-right: 10px;",
#             class = "legend-item",
#             tags$span(
#               style = "background-color: #CCC9C0; color: #000000; padding: 3px; display: block; width: 160%;",
#               "DD"
#             ),
#             tags$span(
#               style = "background-color: #CCC9C0; color: #000000; display: block; width: 160%;",
#               sum(!is.na(data$category) & data$category == "DD")
#             )
#           ),
#           div(
#             style = "display: inline-block; margin-right: 10px;",
#             class = "legend-item",
#             tags$span(
#               style = "background-color: #B7A; color: #000000; padding: 3px; display: block; width: 160%;",
#               "NA"
#             ),
#             tags$span(
#               style = "background-color: #B7A; color: #000000; display: block; width: 160%;",
#               sum(!is.na(data$category) & data$category == "NA")
#             )
#           )
#         )
#       )
#     )
#   )
# )



## new ui.R ----   
# Supprimer les lignes avec des valeurs manquantes dans GENUS ou SP
menuDeroulant <- na.omit(SCS[c("GENUS", "SP")])
# Générer les choix pour le menu déroulant
genus_choices <- unique(menuDeroulant$GENUS)
grouped_species <- SCS %>% 
  group_by(GENUS) %>% 
  summarize(species = list(unique(SP)))

species_choices <- setNames(grouped_species$species, grouped_species$GENUS)



ui <- navbarPage("Data Base Zoology: Interactive map of Wild bees", id="main",
                 tabPanel(title = "Map",
                          tags$div(
                            leafletOutput("beeMap", height = 800),
                            style = "position:relative;",
                            tags$div(
                              style = "position:absolute; top:10px; left:50%; transform: translateX(-50%); z-index:1000;",
                              pickerInput("selectSpecies", "Select a species: ", choices = c(species_choices),
                                          selected = "Megachile parietina",
                                          options = list(`live-search` = TRUE))
                            ),
                            tags$div(
                              style = "position:absolute; bottom: 10px; left: 50%; transform: translateX(-50%); z-index:1000;",
                              sliderInput("jitterInput", "Jitter Amount", min = 0, max = 0.0005, value = 0.0005, step = 0.0001)
                            )
                          )
                 ),
                 tabPanel("Data", DT::dataTableOutput("beeData")),
                 tabPanel("Read me", includeMarkdown("README.md"))
)





# server.R ----

## server new ----
server <- shinyServer(function(input, output) {
  
  SCS=filter(SCS, LATITUDE_DECIMAL != "NA") # removing NA values
  SCS=filter(SCS, LONGITUDE_DECIMAL != "NA") # removing NA values
  
  SCS <- mutate(SCS, popup=paste0( 
    '<strong>Specis: </strong>', SP, 
    '<br><strong>Category:</strong> ', category, 
    '<br><strong>Altitude:</strong> ', ALTITUDE, "m" )
  ) 

  filteredData <- reactive({
    if ("Megachile parietina" == input$selectSpecies) {
      filter(SCS, SP == "Megachile parietina")
    }
    else {
      filter(SCS, SP == input$selectSpecies)
    }
  })


  pal <- colorFactor(pal = c("#B22123", "#F19101", "#F3E61F","#7EB02D", "#54784A", "#CCC9C0"), domain = c("CR", "EN", "VU", "NT", "LC", "DD"))


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
  

  output$beeMap <- renderLeaflet({
    jittered_lat <- if (input$jitterInput == 0) filteredData()$LATITUDE_DECIMAL else jitter(filteredData()$LATITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
    jittered_lon <- if (input$jitterInput == 0) filteredData()$LONGITUDE_DECIMAL else jitter(filteredData()$LONGITUDE_DECIMAL, amount = as.numeric(input$jitterInput))

    leaflet(data = filteredData()) %>% # Utilise les données filtrées
      setView(lng = 6.6, lat = 44, zoom = 8) %>% # Vue de départ
      addTiles() %>% # ajout OpenStreetMap (OSM)
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
      addCircleMarkers(lng = ~jittered_lon, lat = ~jittered_lat,
                       radius = 5, popup = ~popup,
                       color = ~pal(category),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal = pal, values = ~category, opacity = 1, title = "IUCN category") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Center view", # image bouton pour centrer
        onClick = JS("function(btn, map){map.setView([44,  6.8], 10)}") # Vue bouton centrer
      )) %>%
      addLayersControl(overlayGroups = c("PULSE", "WESTBEES"),
                       options = layersControlOptions(collapsed = FALSE)) %>% # Contrôle pour afficher/masquer les shapefiles
      hideGroup(c("PULSE", "WESTBEES")) # par défaut masquer les shapefiles
  })
  
  
  output$beeData <-DT::renderDataTable(datatable(
    SCS, 
    filter = "top" # Activer la recherche par colonne
  ))
    # bb_data[,c(-1,-23,-24,-25,-28:-35)],filter = 'top',
    # colnames = c("Blood Bank Name", "State", "District", "City", "Address", "Pincode","Contact No.",
    #              "Mobile","HelpLine","Fax","Email", "Website","Nodal Officer", "Contact of Nodal Officer",
    #              "Mobile of Nodal Officer", "Email of Nodal Officer","Qualification", "Category", "Blood Component Available",
    #              "Apheresis", "Service Time", "Lat", "Long.")
    # ))

})  


## server old ----
# server <- function(input, output, session) {
#   zoom <- reactiveVal(9)
#   center <- reactiveVal(list(lon = 6.5, lat = 44.1))
#   
#   output$checkData <- renderPrint({
#     print(names(data))
#   })
#   
#   filtered_data <- reactive({
#     if ("Select all genus" %in% input$genreInput) {
#       select(data, GENUS, SP, category, LATITUDE_DECIMAL, LONGITUDE_DECIMAL, ALTITUDE)
#     } else {
#       select(data, GENUS, SP, category, LATITUDE_DECIMAL, LONGITUDE_DECIMAL, ALTITUDE) %>%
#         filter(GENUS %in% input$genreInput)
#     }
#   })
#   
#   observeEvent(input$map_zoom, {
#     zoom(input$map_zoom)
#   })
#   
#   observeEvent(input$map_center, {
#     center(input$map_center)
#   })
#   
#   output$map <- renderPlotly({
#     jittered_lat <- if (input$jitterInput == 0) filtered_data()$LATITUDE_DECIMAL else jitter(filtered_data()$LATITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
#     jittered_lon <- if (input$jitterInput == 0) filtered_data()$LONGITUDE_DECIMAL else jitter(filtered_data()$LONGITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
#     
#     # Créer un ordre de tri basé sur la catégorie
#     category_order <- c("EN", "VU", "NT", "LC", "DD", "CR")
#     
#     # Ajouter une colonne d'ordre de tri dans les données filtrées
#     filtered_data_ordered <- filtered_data() %>%
#       mutate(category_order = factor(category, levels = category_order))
#     
#     plot_ly(
#       data = filtered_data_ordered,
#       lat = ~jittered_lat,
#       lon = ~jittered_lon,
#       marker = list(
#         color = case_when(
#           filtered_data_ordered$category == "CR" ~ "#B22123",
#           filtered_data_ordered$category == "EN" ~ "#F19101",
#           filtered_data_ordered$category == "VU" ~ "#F3E61F",
#           filtered_data_ordered$category == "NT" ~ "#7EB02D",
#           filtered_data_ordered$category == "LC" ~ "#54784A",
#           filtered_data_ordered$category == "DD" ~ "#CCC9C0",
#           TRUE ~ "#B7A"
#         ),
#         size = 12,
#         opacity = 0.8
#       ),
#       type = 'scattermapbox', mode = 'markers',
#       hovertext = paste0("Species: ", filtered_data()$SP, "<br>", "Category: ", filtered_data()$category, "<br>", "Altitude: ", filtered_data()$ALTITUDE, "m"),
#       hoverinfo = "text"
#     ) %>%
#       layout(
#         margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
#         mapbox = list(
#           style = 'open-street-map',
#           zoom = zoom(),  # Utiliser le niveau de zoom actuel
#           center = center()  # Utiliser la position actuelle
#         ),
#         legend = list(
#           title = list(text = "Category"),
#           bgcolor = "#191A1A",
#           bordercolor = "#191A1A",
#           borderwidth = 1,
#           orientation = "v",
#           xanchor = "right",
#           x = 0.85,
#           y = 0.5
#         ),
#         margin = list(r = 150)  # Espacer la légende de la carte
#       )
#   })
# }


# Lunch ShinyApp ----
# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
