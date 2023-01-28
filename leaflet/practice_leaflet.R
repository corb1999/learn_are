
# https://rstudio.github.io/leaflet/

library(tidyverse)
library(leaflet)
library(sf)

leaflet::leaflet() |> 
  leaflet::fitBounds(lng1 = 170, 
                     lng2 = 180, 
                     lat1 = -40, 
                     lat2 = -35) |> 
  leaflet::addTiles() |> 
  leaflet::addCircleMarkers(lng = 174.768, 
                            lat = -36.852, 
                            radius = 50, 
                            fill = FALSE) |> 
  leaflet::addRectangles(lng1 = 175, 
                         lng2 = 173, 
                         lat1 = -34, 
                         lat2 = -38, 
                         fillColor = 'transparent') |> 
  leaflet::addMarkers(lng = 174.768, 
                      lat = -36.852, 
                      label = 'Aukland', 
                      labelOptions = labelOptions(noHide = FALSE, 
                                                  textsize = '25px', 
                                                  direction = 'top')) |> 
  leaflet::addPopups(lng = 177, 
                     lat = -39, 
                     popup = 'Yo this a popup', 
                     options = popupOptions(closeButton = TRUE))

# example of using using an sf object in leaflet
nc <- st_read(system.file("shape/nc.shp", package="sf"))
nc

ggplot(data = nc) + 
  geom_sf(aes(fill = AREA), 
          color = 'black') + 
  theme_minimal()

leaflet::leaflet(data = nc) |> 
  addTiles() |> 
  addPolygons(fillOpacity = 1, 
              weight = 1, 
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,
                                                  bringToFront = TRUE), 
              fillColor = ~colorNumeric('viridis', AREA))
