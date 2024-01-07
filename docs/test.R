#test
shp_PULSE <- sf::st_read("../data/shapefile/PULSE/FAO_GAUL2014_WGS1984_PanEurope_2022.shp") %>% 
  select(geometry)
pal <- colorFactor(pal = c("#B22123", "#F19101", "#F3E61F","#7EB02D", "#54784A", "#CCC9C0"), domain = c("CR", "EN", "VU", "NT", "LC", "DD"))


SCS <- mutate(SCS, popup=paste0( 
  '<strong>Specis: </strong>', SP, 
  '<br><strong>Category:</strong> ', category, 
  '<br><strong>Altitude:</strong> ', ALTITUDE, "m" )
) 

leaflet(data = SCS) %>% # Utilise les données filtrées
  setView(lng = 6.6, lat = 44, zoom = 8) %>% 
  addTiles() %>%
  addPolygons(data = shp_WESTBEES, 
              color = "black", # Couleur des contours
              weight = 2,       # Épaisseur des contours (en pixels)
              fillColor = "darkred")  %>%     # Épaisseur des lignes (en pixels)
  addEasyButton(easyButton(
    icon = "fa-crosshairs", title = "Center view", # image bouton pour centrer
    onClick = JS("function(btn, map){map.setView([44,  6.8], 10)}")
  ))


leaflet(data = filteredData()) %>% # Utilise les données filtrées
  setView(lng = 6.6, lat = 44, zoom = 8) %>% 
  addTiles() %>%
  addPolygons(data = shp_PULSE, 
              color = "black", # Couleur des contours
              weight = 2,       # Épaisseur des contours (en pixels)
              fillColor = "darkred") %>%  # Couleur de remplissage des polygones
  addPolygons(data = shp_WESTBEES,
              color = "black", # Couleur des contours
              weight = 2,       # Épaisseur des contours (en pixels)
              fillColor = "#0090a8") %>%  # Couleur de remplissage des polygones
  addCircleMarkers(lng = ~LONGITUDE_DECIMAL, lat = ~LATITUDE_DECIMAL, 
                   radius = 5, popup = ~popup,
                   color = ~pal(category),
                   stroke = FALSE, fillOpacity = 0.8) %>%
  addLegend(pal = pal, values = ~category, opacity = 1, title = "IUCN category") %>%
  addEasyButton(easyButton(
    icon = "fa-crosshairs", title = "Center view", # image bouton pour centrer
    onClick = JS("function(btn, map){map.setView([44,  6.8], 10)}")
  ))
