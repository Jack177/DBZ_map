# New app.R version
# Library ----
# Dans le fichier app.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(plotly)
library(tidyr) # fonction gather, drop_na
library(ggplot2)
library("readxl")

# start_app.R ----
#getwd()
# setwd("app/")
# source("start_app.R")


# Importation 


SCS <- readxl::read_excel("data/peyresq_2022-2023.xlsx")

getwd()

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
SCS$SP[SCS$SP == "Andrena intermedium"] <- "Andrena intermedia"
SCS$SP[SCS$SP == "Hoplitis cf. lati"] <- "Hoplitis cf. loti"



# Ajout des catégories IUCN
IUCN <- readxl::read_excel("data/EU_Bees_list_and_country_records_2023_05_12.xlsx", sheet = "Full data")
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
  drop_na(SP) -> data


# ui.R ----
ui <- dashboardPage(
  dashboardHeader(title = "Interactive map"),
  dashboardSidebar(
    selectInput("genreInput", "Genus", choices = c("Select all genus", unique(data$GENUS))),
    sliderInput("jitterInput", "Jitter Amount", min = 0, max = 0.0005, value = 0.0005, step = 0.0001)
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 12,
        plotlyOutput(outputId = "map", width = "100%", height = "600px")
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          id = "legend",
          style = "text-align: center; margin-top: 10px;",
          HTML("<h4>Legend</h4>"),
          div(
            style = "display: inline-block; margin-right: 10px;",
            class = "legend-item",
            tags$span(
              style = "background-color: #B22123; color: #FFFFFF; padding: 3px; display: block; width: 160%;",
              "CR"
            ),
            tags$span(
              style = "background-color: #B22123; color: #FFFFFF; display: block; width: 160%;",
              sum(!is.na(data$category) & data$category == "CR")
            )
          ),
          div(
            style = "display: inline-block; margin-right: 10px;",
            class = "legend-item",
            tags$span(
              style = "background-color: #F19101; color: #000000; padding: 3px; display: block; width: 160%;",
              "EN"
            ),
            tags$span(
              style = "background-color: #F19101; color: #000000; display: block; width: 160%;",
              sum(!is.na(data$category) & data$category == "EN")
            )
          ),
          div(
            style = "display: inline-block; margin-right: 10px;",
            class = "legend-item",
            tags$span(
              style = "background-color: #F3E61F; color: #000000; padding: 3px; display: block; width: 160%;",
              "VU"
            ),
            tags$span(
              style = "background-color: #F3E61F; color: #000000; display: block; width: 160%;",
              sum(!is.na(data$category) & data$category == "VU")
            )
          ),
          div(
            style = "display: inline-block; margin-right: 10px;",
            class = "legend-item",
            tags$span(
              style = "background-color: #7EB02D; color: #000000; padding: 3px; display: block; width: 160%;",
              "NT"
            ),
            tags$span(
              style = "background-color: #7EB02D; color: #000000; display: block; width: 160%;",
              sum(!is.na(data$category) & data$category == "NT")
            )
          ),
          div(
            style = "display: inline-block; margin-right: 10px;",
            class = "legend-item",
            tags$span(
              style = "background-color: #54784A; color: #FFFFFF; padding: 3px; display: block; width: 170%;",
              "LC"
            ),
            tags$span(
              style = "background-color: #54784A; color: #FFFFFF; display: block; width: 170%;",
              sum(!is.na(data$category) & data$category == "LC")
            )
          ),
          div(
            style = "display: inline-block; margin-right: 10px;",
            class = "legend-item",
            tags$span(
              style = "background-color: #CCC9C0; color: #000000; padding: 3px; display: block; width: 160%;",
              "DD"
            ),
            tags$span(
              style = "background-color: #CCC9C0; color: #000000; display: block; width: 160%;",
              sum(!is.na(data$category) & data$category == "DD")
            )
          ),
          div(
            style = "display: inline-block; margin-right: 10px;",
            class = "legend-item",
            tags$span(
              style = "background-color: #B7A; color: #000000; padding: 3px; display: block; width: 160%;",
              "NA"
            ),
            tags$span(
              style = "background-color: #B7A; color: #000000; display: block; width: 160%;",
              sum(!is.na(data$category) & data$category == "NA")
            )
          )
        )
      )
    )
  )
)


# server.R ----
server <- function(input, output, session) {
  zoom <- reactiveVal(9)
  center <- reactiveVal(list(lon = 6.5, lat = 44.1))
  
  output$checkData <- renderPrint({
    print(names(data))
  })
  
  filtered_data <- reactive({
    if ("Select all genus" %in% input$genreInput) {
      select(data, GENUS, SP, category, LATITUDE_DECIMAL, LONGITUDE_DECIMAL, ALTITUDE)
    } else {
      select(data, GENUS, SP, category, LATITUDE_DECIMAL, LONGITUDE_DECIMAL, ALTITUDE) %>%
        filter(GENUS %in% input$genreInput)
    }
  })
  
  observeEvent(input$map_zoom, {
    zoom(input$map_zoom)
  })
  
  observeEvent(input$map_center, {
    center(input$map_center)
  })
  
  output$map <- renderPlotly({
    jittered_lat <- if (input$jitterInput == 0) filtered_data()$LATITUDE_DECIMAL else jitter(filtered_data()$LATITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
    jittered_lon <- if (input$jitterInput == 0) filtered_data()$LONGITUDE_DECIMAL else jitter(filtered_data()$LONGITUDE_DECIMAL, amount = as.numeric(input$jitterInput))
    
    # Créer un ordre de tri basé sur la catégorie
    category_order <- c("EN", "VU", "NT", "LC", "DD", "CR")
    
    # Ajouter une colonne d'ordre de tri dans les données filtrées
    filtered_data_ordered <- filtered_data() %>%
      mutate(category_order = factor(category, levels = category_order))
    
    plot_ly(
      data = filtered_data_ordered,
      lat = ~jittered_lat,
      lon = ~jittered_lon,
      marker = list(
        color = case_when(
          filtered_data_ordered$category == "CR" ~ "#B22123",
          filtered_data_ordered$category == "EN" ~ "#F19101",
          filtered_data_ordered$category == "VU" ~ "#F3E61F",
          filtered_data_ordered$category == "NT" ~ "#7EB02D",
          filtered_data_ordered$category == "LC" ~ "#54784A",
          filtered_data_ordered$category == "DD" ~ "#CCC9C0",
          TRUE ~ "#B7A"
        ),
        size = 12,
        opacity = 0.8
      ),
      type = 'scattermapbox', mode = 'markers',
      hovertext = paste0("Species: ", filtered_data()$SP, "<br>", "Category: ", filtered_data()$category, "<br>", "Altitude: ", filtered_data()$ALTITUDE, "m"),
      hoverinfo = "text"
    ) %>%
      layout(
        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
        mapbox = list(
          style = 'open-street-map',
          zoom = zoom(),  # Utiliser le niveau de zoom actuel
          center = center()  # Utiliser la position actuelle
        ),
        legend = list(
          title = list(text = "Category"),
          bgcolor = "#191A1A",
          bordercolor = "#191A1A",
          borderwidth = 1,
          orientation = "v",
          xanchor = "right",
          x = 0.85,
          y = 0.5
        ),
        margin = list(r = 150)  # Espacer la légende de la carte
      )
  })
}


# Lunch ShinyApp ----
# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
