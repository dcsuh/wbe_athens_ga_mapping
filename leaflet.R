#testing using leaflet instead of mapview to make maps

leaflet(n1_n2_catchment) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", total_copies)(total_copies),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

library(tmap)
tm_shape(n1_n2_catchment$geometry) + tm_fill(col=n1_n2_catchment$total_copies)

leaflet(data=n1_n2_catchment$geometry) %>%
  addPolygons()