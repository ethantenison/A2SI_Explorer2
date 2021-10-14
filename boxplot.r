# pm25 <- df3 |>
#   filter(var == "PM2.5") |>
#   mutate(
#     `> 50% White` = if_else(`% White-Alone` >= 0.5, 1, 0),
#     `> 50% Asian` = if_else(`% Asian-Alone` >= 0.5, 1, 0),
#     `> 50% Hispanic` = if_else(`% Hispanic` >= 0.5, 1, 0),
#     `> 50% Black` = if_else(`% Black-Alone` >= 0.5, 1, 0),
#     `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0),
#     `Region Average` = 1
#   ) |>
#   pivot_longer(cols = `> 50% White`:`Region Average`, names_to = "demo", 
#                values_to = "val2") |>
#   filter(val2 != 0) |>
#   mutate(demo = factor(demo, levels = c("Region Average",
#                                         "> 50% White",
#                                         "> 50% Asian",
#                                         "> 50% Hispanic",
#                                         "> 50% Black",
#                                         "> 50% Low Income")))
# 
# to_plotly <- as.data.frame(pm25) 
# 
# 
# fig <- plot_ly(pm25, y = ~value, color = ~demo, type = "box")
# 
# fig


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
  plot_ly(y = ~boxdata()$value, color = ~boxdata()$demo, type = "box")
  
})

