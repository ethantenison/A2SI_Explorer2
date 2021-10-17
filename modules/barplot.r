# UI ----

plotsUI <- function(id) {
  tagList(
    plotlyOutput(NS(id, "barplot"), height = "325px"),
    plotlyOutput(NS(id, "boxplot"), height = "340px")
  )
}


# Server ----
plotsServer <- function(id, data) {
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
          y = c(total_av, whi_av, asi_av, his_av, bla_av, lowincome_av),
          x = c(
            "Region Average",
            "> 50% White",
            "> 50% Asian",
            "> 50% Hispanic",
            "> 50% Black",
            "> 50% Low Income"
          )
        ) |>
        mutate(x = factor(
          x,
          levels = c(
            "Region Average",
            "> 50% White",
            "> 50% Asian",
            "> 50% Hispanic",
            "> 50% Black",
            "> 50% Low Income"
          )
        ))
      
      return(bar_to_plotly)
    })
    
    
    #Plotly Barplot
    output$barplot <- renderPlotly({
      bar <- plot_ly(
        x = bardata()$x,
        y = bardata()$y,
        color = I("#00a65a"),
        type = 'bar'
        
      ) |>
        config(displayModeBar = FALSE) |>
        layout(
               yaxis = list(title = paste0(unique(data()$var)," Average")),
               xaxis = list(title = "")) |>
        layout(title = unique(data()$var),
               font=list(size = 12),
               titlefont=list(size=25),
               margin = list(l=50, r=50, b=50, t=75, pad=4) )
      
    })
    
    ### Boxplot ----
    boxdata <- reactive({
      box <-
        data() |>
        mutate(
          `> 50% White` = if_else(`% White-Alone` >= 0.5, 1, 0),
          `> 50% Asian` = if_else(`% Asian-Alone` >= 0.5, 1, 0),
          `> 50% Hispanic` = if_else(`% Hispanic` >= 0.5, 1, 0),
          `> 50% Black` = if_else(`% Black-Alone` >= 0.5, 1, 0),
          `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0),
          `Region Average` = 1
        ) |>
        pivot_longer(
          cols = `> 50% White`:`Region Average`,
          names_to = "demo",
          values_to = "val2"
        ) |>
        filter(val2 != 0) |>
        mutate(demo = factor(
          demo,
          levels = c(
            "Region Average",
            "> 50% White",
            "> 50% Asian",
            "> 50% Hispanic",
            "> 50% Black",
            "> 50% Low Income"
          )
        ))
      
      to_plotly <- as.data.frame(box)
      
      return(to_plotly)
    })
    
    #Plotly Boxplot
    output$boxplot <- renderPlotly({
      plot_ly(
        x = boxdata()$demo,
        y = boxdata()$value,
        color = I("#00a65a"),
        type = "box"
      ) |>
        config(displayModeBar = FALSE) |>
        layout(showlegend = FALSE,
               yaxis = list(title = unique(data()$var)),
               xaxis = list(title = ""))
      
    })
    
    
  })
}
