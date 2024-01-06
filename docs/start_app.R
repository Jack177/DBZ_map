# Load the required packages
library(leaflet)
library(plotly)
library(dplyr)
library(tidyr) # fonction gather
library(vegan) # specpool () estimateR() poolaccum() estaccumR()
library(iNEXT)
library(ggplot2)
library("readxl") 
library(forcats)
library(RODBC)
library(viridis) # couleur daltonien
library(shiny)
library(shinydashboard)
#library(grDevices) # pdf() dev.off()
# getwd()


# Importation 


SCS <- readxl::read_excel("www/data/Fusion-donnees-2022-2023_ok.xlsx")
  
getwd()

# SP
SCS <- SCS %>%
  # Créer SP en fusionnant GENUS et SPECIES
  mutate(SP = paste(GENUS, SPECIES, sep = " ")) %>%
  # Mettre la colonne SP en première position
    select(SP, everything())
  
# Convertir les degrés minutes secondes en décimale
source("www/docs/dms_to_decimal.R")

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
IUCN <- readxl::read_excel("EU_Bees_list_and_country_records_2023_05_12.xlsx", sheet = "Full data")
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

