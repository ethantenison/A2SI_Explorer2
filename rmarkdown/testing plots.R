health <- readRDS("./data/asthma_for_app.rds") |>
  as.data.frame() |>
  filter(var == "Asthma ED incidence") |>
  select(value,
         "% White-Alone",
         "% Black-Alone",
         "% Asian-Alone",
         "% Hispanic"  ,
         "% low-income") |>
  pivot_longer(
    cols = `% White-Alone`:`% low-income`,
    names_to = "group",
    values_to = "percent"
  ) |> 
  select(value, group, percent) |> 
  filter(!is.na(value))



theme_set(theme_bw())
ggplot(health, aes(x = percent, y = group, color = value, size = value)) +
  geom_jitter(alpha = 0.6) +
  scale_color_gradient(low = "white", high = "red")

nonsimplified <- readRDS("./data/asthma_for_app.rds")

library(rmapshaper)

simp <- ms_simplify(nonsimplified, keep = 0.001,
                                keep_shapes = TRUE)

round(c(object.size(nonsimplified),
        object.size(simp)) / 1024)


library(sf)
library(rmapshaper)
library(tidyverse)
flood <- st_read("data/shape_files/geo_export_083bfd0c-c67c-4914-b072-6d1caad91b5a.shp", stringsAsFactors = FALSE) |> 
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") |> 
  ms_simplify(keep = 0.01,keep_shapes = FALSE) 


saveRDS(flood, "data/flood_polygon.rds")

library(leaflet)

leaflet(flood) |> 
  addTiles() |> 
  addPolygons()
  

health <- readRDS("./data/asthma_for_app.rds") |>
  as.data.frame() |>
  filter(var == "Asthma ED incidence") |>
  select("GEOID",
         "asian_prev",
         "white_prev",
         "black_prev",
         "hispanic_prev") |> 
  pivot_longer(cols = asian_prev:hispanic_prev) 

grouped <- health |> 
  group_by(name) |> 
  summarize(incidence = mean(value, na.rm = TRUE))


ggplot(grouped, aes(x = name, y = incidence)) +
  geom_bar(stat = "identity")


ggplot(health, aes(x=name, y = value)) +
  geom_boxplot()





