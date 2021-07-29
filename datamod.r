#Data Set Module

dataInput <- function(id) {
  tagList(
    pickerInput(
      NS(id, "var"),
      label = NULL,
      width = '100%',
      inline = FALSE,
      options = list(`actions-box` = TRUE,
                     size = 10),
      choices =
        list(
          "Air Hazards" = list(
            "O3",
            "Ozone - CAPCOG",
            "Percentile for Ozone level in air",
            "PM2.5",
            "PM2.5 - CAPCOG",
            "Percentile for PM2.5 level in air"
          ),
          "Environmental Measures" = list(
            "Wildfire Exposure",
            "Heat Exposure",
            "Flood Exposure",
            "Multihazard Exposure",
            "Population Sensitivity",
            "Multihazard Exposure and Population Sensitivity",
            "Average Impervious Cover",
            "Average Tree Cover"
          ),
          "Demograpic Information" = list(
            "Total population",
            "Population Density",
            "% people of color",
            "% low-income",
            "% under age 5",
            "% over age 64",
            "Average Vehicles per person",
            "Percent of households without a car"
          )
        ),
      selected = "Multihazard Exposure and Population Sensitivity"
    )
  )
}

dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    austin_map <- readRDS("./data/austin_composite.rds")
    austin_map <- as.data.frame(austin_map)
    austin_map <- st_as_sf(austin_map)
    austin_map <-
      st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
    austin_map$value <- as.numeric(austin_map$value)
    
    
    variable <- reactive({
      austin_map |> dplyr::filter(var == input$var)
    })
    
    input <- reactive({
      input$var
    })
    
    
    #datalist <- list(variable, input)
    variable
  })
}