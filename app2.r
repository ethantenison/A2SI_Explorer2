
# -----------Libraries----------- #



library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(shinydashboard)


# ------- Reference Data -------- #



source("barplot.r")


# ----------Dashboard UI--------- #


ui = dashboardPage(
  skin = "black",
  header = shinydashboard::dashboardHeader(title = "Reproducible Example"),
  sidebar = shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Data Explorer",
        tabName = "data",
        icon = icon("project-diagram")
      ),
      conditionalPanel(condition = "input.tabs == 'data'")
    )
  ),
  body = shinydashboard::dashboardBody(tabItems(tabItem(
    tabName = "data",
    fluidRow(
      shinydashboard::box(
        title = "Select a Variable",
        width = 12,
        solidHeader = FALSE,
        status = "primary",
        background = "light-blue",
        barplotInput("barplot")
      )
    )
  )))
)




# --------Dashboard Server------- #


server <- function(input, output, session) {
  
  barplotServer("barplot")
  
}

# -----Run the application------- #

shinyApp(ui,server)
