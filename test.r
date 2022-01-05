library(shiny)
library(leaflet)

#Modules
source("modules/datamod_v2.r")
source("modules/mapmod.r")

aq <- readRDS("./data/aq.rds")

ui <- fluidPage(tabsetPanel(id = "tab_being_displayed", # will set input$tab_being_displayed
                            tabPanel("Tab 1"),
                            tabPanel(
                              "Map Tab",
                              dataUI(
                                "air",
                                choices = list("Ozone - CAPCOG",
                                               "PM2.5"),
                                selected = "PM2.5"
                              ),
                              mapUI("air_map", height = "650")
                            )))

server <- function(input, output, session) {
  
  #Variable to visualize
  air <- dataServer("air", data = aq, info = "aq")
  variable_air <- air$df
  selected_air <- air$var
  
  
  mapServer("air_map", data = variable_air, selected = selected_air)
  plotsServer("aq_bar", data = variable_air)
  
}
shinyApp(ui, server)