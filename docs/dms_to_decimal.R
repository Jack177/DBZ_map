# Données d'exemple
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
