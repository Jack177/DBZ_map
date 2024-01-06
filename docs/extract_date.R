library(lubridate)

extract_date <- function(df){
  # Extrait les 4 premiers caractères
  df$year <- substr(df$DAT2, 1, 4)
  # Extrait les caractères 5 à 6 et les convertis en chiffre romain
  df$month <- substr(df$DAT2, 5, 6)
  # Extrait les caractères 7 à 8
  df$day <- substr(df$DAT2, 7, 8)
  
  return(c(df$day, df$month, df$year))
}
