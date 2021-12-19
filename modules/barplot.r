# UI ----

plotsUI <- function(id) {
  tagList(
    fluidRow(
      column(10),
      column(1,
             style = 'padding: 0px;',
             circleButton(
               NS(id, "btn2"),
               icon = icon("question"),
               status = "danger",
               size = "sm"
             ))
    ),
    fluidRow(
    column(
      width = 12,
      plotlyOutput(NS(id, "barplot"), height = "325px")
    )
  ),
  br(),
  fluidRow(column(
    width = 12,
    plotlyOutput(NS(id, "boxplot"), height = "325px") 
  )),
  fluidRow(column(
    width = 12,
    br(),
    HTML(
      '<div style="text-align: right">
    *Missing groups signifies missing information
    </div>'
    )
  ))
  
  )
}


# Server ----
plotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observe_helpers()
    
    ### Barplot ----
    bardata <- reactive({
      bar <-
        data() 
      
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
          y = c(whi_av, asi_av, his_av, bla_av, lowincome_av),
          x = c(
            "White",
            "Asian",
            "Hispanic",
            "Black",
            "Low Income"
          )
        ) |>
        mutate(x = factor(
          x,
          levels = c(
            "White", 
            "Asian",
            "Hispanic",
            "Black",
            "Low Income"
          )
        ))
      
      return(bar_to_plotly)
    })
    
    
    #Plotly Barplot
    hline <- function(y = 0, color = "red") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color,  dash="dot")
      )
    }
    
    output$barplot <- renderPlotly({
      
      geo <- ifelse(unique(data()$var) %in% c("Asthma ED incidence",
                                            "Asthma ED incidence Children",
                                            "Asthma ED incidence Adults"),
                    "Tract", "Block Group")
      
      bar <- plot_ly(
        x = bardata()$x,
        y = bardata()$y,
        color = I("#00a65a"),
        type = 'bar'
        
      ) |>
        config(displayModeBar = FALSE) |>
        layout(
               yaxis = list(title = paste0(unique(data()$var)," Average")),
               xaxis = list(title = paste0("Census ",geo, " Majority"))) |>
        layout(title = unique(data()$var),
               font=list(size = 12),
               titlefont=list(size=25),
               margin = list(l=50, r=50, b=50, t=75, pad=4),
               shapes = list(hline(mean(bardata()$y, na.rm = TRUE)))) |> 
        add_annotations(showlegend = FALSE, x = 2.7, y = mean(bardata()$y, na.rm = TRUE),
                 text = c("Region Average"), font = list(color = '#264E86'))
      
    })
    
    ### Boxplot ----
    boxdata <- reactive({
      box <-
        data() |>
        rename("White" = "> 50% White",
               "Asian" = "> 50% Asian",
               "Hispanic" = "> 50% Hispanic",
               "Black" = "> 50% Black",
               "Low Income" = "> 50% Low Income") |> 
        pivot_longer(
          cols = `White`:`Low Income`,
          names_to = "demo",
          values_to = "val2"
        ) |>
        filter(val2 != 0) |>
        mutate(demo = factor(
          demo,
          levels = c(
            "White",
            "Asian",
            "Hispanic",
            "Black",
            "Low Income"
          )
        ))
      
      to_plotly <- as.data.frame(box)
      
      return(to_plotly)
    })
    
    #Plotly Boxplot
    output$boxplot <- renderPlotly({
      
      geo <- ifelse(unique(data()$var) %in% c("Asthma ED incidence",
                                              "Asthma ED incidence Children",
                                              "Asthma ED incidence Adults"),
                    "Tract", "Block Group")
      
      plot_ly(
        x = boxdata()$demo,
        y = boxdata()$value,
        color = I("#00a65a"),
        type = "box"
      ) |>
        config(displayModeBar = FALSE) |>
        layout(showlegend = FALSE,
               yaxis = list(title = unique(data()$var)),
               xaxis = list(title = paste0("Census ",geo, " Majority")),
              shapes = list(hline(mean(boxdata()$value, na.rm = TRUE)))
               ) |> 
        add_annotations(showlegend = FALSE, x = 2.7, y = mean(boxdata()$value, na.rm = TRUE),
                        text = c("Region Average"), font = list(color = '#264E86'))
      
    })
    
    
    observeEvent(input$btn2, {
        showModal(
          modalDialog(
            includeHTML(
              knitr::knit2html("tooltips/plothelp.md", fragment.only = TRUE)
            ),
            #must knit
            easyClose = TRUE,
            size = "l",
            fade = TRUE
          )
        )
})
    
  })
}
