# Barplot module 

barplotInput <- function(id) {
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
    ),
    plotlyOutput(NS(id,"barplot"), height = "300px")
  )
}


barplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    austin_map <- readRDS("./data/austin_composite.rds")
    austin_map$value <- as.numeric(austin_map$value)
    
    
    #Data for barplot
    bar <- reactive({
      bar <-
        austin_map |>
        dplyr::filter(var == input$var) |>
        mutate(
          `> 50% People of Color` = if_else(`% people of color` >= 0.5, 1, 0),
          `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0)
        )
      
      total_av <- mean(bar$value)
      
      poc <- bar |> filter(`> 50% People of Color` == 1)
      poc_av <- mean(poc$value)
      
      lowincome <- bar |> filter(`> 50% Low Income` == 1)
      lowincome_av <- mean(lowincome$value)
      
      
      bar_to_plotly <-
        data.frame(
          y = c(total_av, poc_av, lowincome_av),
          x = c("Austin Average",
                "> 50% People of Color",
                "> 50% Low Income")
        )
      
      return(bar_to_plotly)
    })
    
    # #Plotly Barplot
    output$barplot <- renderPlotly({
      plot_ly(
        x = bar()$x,
        y = bar()$y,
        color = I("#00a65a"),
        type = 'bar'
        
      ) |>
        config(displayModeBar = FALSE)
      
    })
  })
}


