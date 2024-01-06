install_parzer <- function() {
  if (!require("parzer")) {
    install.packages("parzer")
  }
}

install_parzer()
library("parzer")
library(tidyverse)
library(sf)
library(leaflet)

## Extraction des DMS à partir des coordonnées décimales
# ---LATITUDE---
convert_lati_to_dms <- function(df){

  df$lati_d <- pz_degree(lon = df$LATI)
  df$lati_m <- pz_minute(lon = df$LATI)
  df$lati_s <- pz_second(lon = df$LATI)
  # Combine les 3 colonnes avec les unités
  df$lati_dms <- paste(df$lati_d, "°", df$lati_m, "'", round(df$lati_s, digits = 1), "\"", sep="")
  # Ajoute le point cardinal N
  df$lati_dir <- "N"
  # Fusionne le point cardinal
  df$lati_dms <- paste(df$lati_dms, df$lati_dir, sep=" ")
  
  return(df$lati_dms)
}

# ---LONGITUDE---
convert_long_to_dms <- function(df){
  
  df$long_d <- pz_degree(lon = df$LONG)
  df$long_m <- pz_minute(lon = df$LONG)
  df$long_s <- pz_second(lon = df$LONG)
  # Combine les 3 colonnes avec les unités
  df$long_dms <- paste(df$long_d, "°", df$long_m, "'", round(df$long_s, digits = 1), "\"", sep="")
  # Ajoute le point cardinal E
  df$long_dir <- "E"
  # Fusionne le point cardinal
  df$long_dms <- paste(df$long_dms, df$long_dir, sep=" ")
  
  return(df$long_dms)
}


# # Extraction des DMS à partir des coordonnées décimales
# SCS$long_d <- pz_degree(lon = SCS$LONG)
# SCS$long_m <- pz_minute(lon = SCS$LONG)
# SCS$long_s <- pz_second(lon = SCS$LONG)
# 
# SCS$lati_d <- pz_degree(lon = SCS$LATI)
# SCS$lati_m <- pz_minute(lon = SCS$LATI)
# SCS$lati_s <- pz_second(lon = SCS$LATI)
# 
# 
# # Combine les 3 colonnes avec les unités
# SCS$long_dms <- paste(SCS$long_d, "°", SCS$long_m, "'", round(SCS$long_s, digits = 1), "\"", sep="")
# # Ajoute le point cardinal E
# SCS$long_dir <- "E"
# # Fusionne le point cardinal
# SCS$long_dms <- paste(SCS$long_dms, SCS$long_dir, sep=" ")
# 
# # Combine les 3 colonnes avec les unités
# SCS$lati_dms <- paste(SCS$lati_d, "°", SCS$lati_m, "'", round(SCS$lati_s, digits = 1), "\"", sep="")
# # Ajoute le point cardinal N
# SCS$lati_dir <- "N"
# # Fusionne le point cardinal
# SCS$lati_dms <- paste(SCS$lati_dms, SCS$lati_dir, sep=" ")
# 
# # Sélectionner seulement les colonnes d'intérêts
# SCS <- select(SCS, Country, LOCA, ALTI, DATE, lati_dms, long_dms, plante, REC)


# Exportation en .csv
#write.csv(SCS, "Output/SCS_dms.csv")



