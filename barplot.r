# UI ----

barplotUI <- function(id) {
  tagList(plotlyOutput(NS(id, "barplot"), height = "300px"))
}

boxplotUI <- function(id) {
  tagList(plotlyOutput(NS(id, "boxplot"), height = "300px"))
}

# Server ----
#' @param data Reactive element from another module: reactive(dplyr::filter(austin_map, var == input$var)) 
barplotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    ### Barplot ---- 
    bardata <- reactive({
      bar <-
        data() |>
        mutate(
          `> 50% White` = if_else(`% White-Alone` >= 0.5, 1, 0),
          `> 50% Asian` = if_else(`% Asian-Alone` >= 0.5, 1, 0),
          `> 50% Hispanic` = if_else(`% Hispanic` >= 0.5, 1, 0),
          `> 50% Black` = if_else(`% Black-Alone` >= 0.5, 1, 0),
          `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0)
        )
      
      total_av <- mean(bar$value, na.rm = TRUE)
      
      whi <- bar |> filter(`> 50% White` == 1)
      whi_av <- mean(whi$value, na.rm = TRUE)
      
      asi <- bar |> filter(`> 50% Asian` == 1)
      asi_av <- mean(asi$value, na.rm = TRUE)
      
      his <- bar |> filter(`> 50% Hispanic` == 1)
      his_av <- mean(his$value, na.rm = TRUE)
      
      bla <- bar |> filter(`> 50% Black` == 1)
      bla_av <- mean(bla$value, na.rm = TRUE)
      
      lowincome <- bar |> filter(`> 50% Low Income` == 1)
      lowincome_av <- mean(lowincome$value, na.rm = TRUE)
      
      bar_to_plotly <-
        data.frame(
          y = c(total_av, whi_av,asi_av,his_av,bla_av, lowincome_av),
          x = c("Region Average",
                "> 50% White",
                "> 50% Asian",
                "> 50% Hispanic",
                "> 50% Black",
                "> 50% Low Income")
        ) |>
        mutate(x = factor(x, levels = c("Region Average",
                                        "> 50% White",
                                        "> 50% Asian",
                                        "> 50% Hispanic",
                                        "> 50% Black",
                                        "> 50% Low Income")))
      
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


boxplotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ### Boxplot ---- 
    boxdata <- reactive({
      box <-
        data() |>
        filter(var == "PM2.5") |>
        mutate(
          `> 50% White` = if_else(`% White-Alone` >= 0.5, 1, 0),
          `> 50% Asian` = if_else(`% Asian-Alone` >= 0.5, 1, 0),
          `> 50% Hispanic` = if_else(`% Hispanic` >= 0.5, 1, 0),
          `> 50% Black` = if_else(`% Black-Alone` >= 0.5, 1, 0),
          `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0),
          `Region Average` = 1
        ) |>
        pivot_longer(cols = `> 50% White`:`Region Average`, names_to = "demo", 
                     values_to = "val2") |>
        filter(val2 != 0) |>
        mutate(demo = factor(demo, levels = c("Region Average",
                                              "> 50% White",
                                              "> 50% Asian",
                                              "> 50% Hispanic",
                                              "> 50% Black",
                                              "> 50% Low Income")))
      
      to_plotly <- as.data.frame(box) 
      
      return(to_plotly)
    })
    
    #Plotly Boxplot
    output$boxplot <- renderPlotly({
      plot_ly(boxdata(), y = ~boxdata()$value, color = ~boxdata()$demo, type = "box")|>
        config(displayModeBar = FALSE)
      
    })
    
    
    
  })
}
