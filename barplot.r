#UI

barplotUI <- function(id) {
  tagList(plotlyOutput(NS(id, "barplot"), height = "300px"))
}

#Server
#' @param data Reactive element from another module: reactive(dplyr::filter(austin_map, var == input$var)) 
barplotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    #Data Manipulation
    bardata <- reactive({
      bar <-
        data() |>
        mutate(
          `> 50% People of Color` = if_else(`% people of color` >= 0.5, 1, 0),
          `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0)
        )
      
      total_av <- mean(bar$value, na.rm = TRUE)
      poc <- bar |> filter(`> 50% People of Color` == 1)
      poc_av <- mean(poc$value, na.rm = TRUE)
      lowincome <- bar |> filter(`> 50% Low Income` == 1)
      lowincome_av <- mean(lowincome$value, na.rm = TRUE)
      bar_to_plotly <-
        data.frame(
          y = c(total_av, poc_av, lowincome_av),
          x = c("Region Average",
                "> 50% People of Color",
                "> 50% Low Income")
        )
      
      return(bar_to_plotly)
    })
    
    #Plotly Barplot
    output$barplot <- renderPlotly({
      plot_ly(
        x = bardata()$x,
        y = bardata()$y,
        color = I("#00a65a"),
        type = 'bar'
        
      ) |>
        config(displayModeBar = FALSE)
      
    })
  })
}
