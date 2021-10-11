# Preliminary Asthma Data Mapping 
library(tidyverse)
library(readr)
library(leaflet)

df <- read_rds("data/asthma_prevalence_2017_capcog.rds")

df$child_prev <- df$child_prev * 10000
df$adult_prev <- df$adult_prev * 10000
df$total_prev <- df$total_prev * 10000

df <- st_as_sf(df) |> 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")

leaflet(df) |> 
addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.7,
        fillColor = ~colorQuantile("YlOrRd", df$total_prev)(df$total_prev),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ))


leaflet(df) |> 
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 0.7,
    fillColor = ~colorQuantile("YlOrRd", df$child_prev)(df$child_prev),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ))

leaflet(df) |> 
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 0.7,
    fillColor = ~colorQuantile("YlOrRd", df$adult_prev)(df$adult_prev),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ))
