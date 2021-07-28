# Barplot module 

barplotInput <- function(id) {
  tagList(
    plotlyOutput(NS(id,"barplot"), height = "300px")
  )
}



histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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




