# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #


library(shiny)
library(shinyjs)
library(rintrojs)
library(RColorBrewer)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(htmltools)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)
library(ggmap)
library(googleway)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #

options(scipen = 999)
source("data/key.r")
set_key(key = key)

#Modules
source("datamod_v2.r")
source("barplot.r")
source("mapmod.r")

austin_map <- readRDS("./data/austin_composite.rds")

health <- readRDS("./data/asthma_for_app.rds")

#Data sources not in modules
definitions <- read_csv("data/definitions.csv") |>
  dplyr::select(-c("Additional Information")) |>
  mutate(Units = replace_na(Units, ""))



# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
     var element = document.documentElement,
 enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
 exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
 if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
 enterFS.call(element);
 } else {
 exitFS.call(document);
 }
 }'


# UI ----
ui = shinydashboard::dashboardPage(
  skin = "black",
  title = "A2SI",
  header = shinydashboard::dashboardHeader(title = tagList(
    span(class = "logo-lg", tags$img(src = 'images/logo_skinny.png', width =
                                       '50%')),
    img(src = 'images/arrow.png', width = '150%')
  )),
  sidebar = shinydashboard::dashboardSidebar(
    useShinyjs(),
    shinyjs::extendShinyjs(text = jsToggleFS, functions = "toggleFullScreen"),
    introjsUI(),
    collapsed = FALSE,
    ## Sidebar ----
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome Page",
               tabName = "welcome",
               icon = icon("search")),
      conditionalPanel(condition = "input.tabs == 'welcome'"),
      menuItem("Air Quality",
               tabName = "air",
               icon = icon("wind")),
      conditionalPanel(
        condition = "input.tabs == 'air'"),
      menuItem("Environment",
               tabName = "environment",
               icon = icon("tree")),
      conditionalPanel(condition = "input.tabs == 'environment'"),
      menuItem("Health",
               tabName = "health",
               icon = icon("plus-square")),
      conditionalPanel(condition = "input.tabs == 'health'"),
      menuItem(
        "Social Vulnerability",
        tabName = "social",
        icon = icon("users")
      ),
      conditionalPanel(condition = "input.tabs == 'social'"),
      menuItem("Definitions",
               tabName = "definitions",
               icon = icon("book")),
      conditionalPanel(condition = "input.tabs == 'definitions'"),
      menuItem("Attributions",
               tabName = "attributions",
               icon = icon("info-circle")),
      conditionalPanel(condition = "input.tabs == 'attributions'"),
      hr(style = "margin-top: 5px; margin-bottom: 5px; width:90%"),
      HTML(
        "<button type='button' class='btn btn-default action-button shiny-bound-input' style='display: block;
        margin: 6px 5px 6px 15px; width: 200px;color: #152934;' onclick = 'shinyjs.toggleFullScreen();
        '><i class='fa fa-expand fa-pull-left'></i> Fullscreen</button>"
      )
      
    )
    
  ),
  ## Body ----
  body = shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tags$script(
      HTML(
        '
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Austin Area Sustainability Indicators </span>\');
      })
     '
      )
    ),
    tabItems(
      ### Welcome ----
      tabItem(
        tabName = "welcome",
        column(
          width = 12,
          fluidRow(
            shinydashboard::box(
              title = NULL,
              width = 12,
              solidHeader = TRUE,
              background = "light-blue",
              status = "primary",
              h1(
                strong("Welcome to the A2SI Dashboard!"),
                style = "font-size:75px;"
              ),
              hr(),
              h3(
                "This project is brought to you by the Austin Area Sustainability Indicators
                     project (A2SI) at the University of Texas
                     at Austin in collaborations with the City of Austin Office of Sustainability
                     and the Capital Area Council of Governments (CAPCOG)."
              )
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = NULL,
              width = 4,
              solidHeader = FALSE,
              status = "primary",
              div(style = "text-align: center;",
                  img(src = "images/AASI_logo_v1b-01.png",
                      width = "75%")),
              br(),
              "The",
              a("Austin Area Sustainability Indicators", href = "http://www.austinindicators.org/"),
              "(A2SI), an initative led by Professor Patrick Bixler at the LBJ
              School of Public Affairs, aim is to measure the quality of life, sustainability trends, and
              serve as the foundation for a systems approach to address challenges in Central Texas.",
              br(),
              br(),
              "Indicators describe context, identify trends, and translate data into points that are
                easier to communicate. These indicators span population demographics, civic life, education,
                health, mobility, economy and environment."
            ),
            shinydashboard::box(
              width = 4,
              solidHeader = FALSE,
              status = "primary",
              div(style = "text-align: center;",
                  img(src = "images/aos.png",
                      width = "100%")),
              br(),
              "The City of Austin's Office of Sustainability provides leadership,
              influences positive action through engagement, and creates measurable
              benefits for Austin. We seek ways to ensure a thriving, equitable,
              and ecologically resilient community. Take a look at some of our achievements."
              
            ),
            shinydashboard::box(
              width = 4,
              solidHeader = FALSE,
              status = "primary",
              div(style = "text-align: center;",
                  img(src = "images/capcog.png",
                      width = "50%")),
              br(),
              "The Capital Area Council of Governments (CAPCOG) serves as an advocate,
              planner and coordinator on important regional issues in the 10-county
              encompassing Austin-Round Rock-Georgetown Metropolitan Statistical Area.",
              br(),
              br(),
              "With more than 90 member governments and organizations, including
              cities, counties, school and appraisal districts, utilities,
              chambers of commerce and more, CAPCOG has helped the region recognize
              opportunities for cooperation and eliminate unnecessary duplication
              in emergency communications, elderly assistance, law enforcement
              training, criminal justice planning, solid waste reduction, homeland security planning,
              infrastructure development, transportation planning and economic development."
              
            )
            
          )
          
        )
      ),
      ### AQ ----
      tabItem(tabName = "air",
              column(
                width = 12,
                offset = 0,
                fluidRow(column(
                  style='padding-left:30px;padding-bottom: 30px;',
                  width = 6,
                  h1(strong("Air Quality"), style = "font-size:50px;")
                ),
                column(width = 4)),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "primary",
                    dataUI( #Selector UI
                      "air",
                      choices = list(
                        "Ozone - CAPCOG",
                        "PM2.5",
                        "PM2.5 - CAPCOG",
                        "Percentile for PM2.5 level in air"
                      ),
                      selected = "PM2.5"
                    ),
                    mapUI("air_map", height = "700") #Map UI
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "primary"
                  )
                  
                ))
              )),
      ### Environment ----
      tabItem(tabName = "environment",
              column(
                width = 12,
                offset = 0,
                fluidRow(column(
                  style='padding-left:30px;padding-bottom: 30px;',
                  width = 6,
                  h1(strong("Environment"), style = "font-size:50px;")
                  
                ),
                column(width = 4)),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "primary",
                    dataUI(
                      "environment",
                      choices = list(
                        "Wildfire Exposure",
                        "Heat Exposure",
                        "Flood Exposure",
                        "Multihazard Exposure",
                        "Multihazard Exposure and Population Sensitivity",
                        "Average Impervious Cover",
                        "Average Tree Cover"
                      ),
                      selected = "Average Tree Cover"
                    ),
                    mapUI("env_map", height = "700")
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "primary",
                    barplotUI("env_bar")
                  )
                  
                ))
              )),
      ### Health ----
      tabItem(tabName = "health",
              column(
                width = 12,
                offset = 0,
                fluidRow(column(
                  style='padding-left:30px;padding-bottom: 30px;',
                  width = 6,
                  h1(strong("Health"), style = "font-size:50px;")
                ),
                column(width = 4)),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "primary",
                    dataUI(
                      "health",
                      choices = list(
                        "Asthma Prevalence",
                        "Asthma Prevalence in Children",
                        "Asthma Prevalence in Adults"
                      ),
                      selected = "Asthma Prevalence"
                    ),
                    mapUI("hel_map", height = "700")
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "primary",
                    barplotUI("hel_bar")
                  )
                  
                ))
              )),
      ### Social ----
      tabItem(tabName = "social",
              column(
                width = 12,
                offset = 0,
                fluidRow(column(
                  style='padding-left:30px;padding-bottom: 30px;',
                  width = 6,
                  h1(strong("Social Vulnerability"), style = "font-size:50px;")
                ),
                column(width = 4)),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "primary",
                    dataUI(
                      "social",
                      choices = list(
                        "Log Population Density",
                        "% people of color",
                        "% low-income",
                        "% under age 5",
                        "% over age 64",
                        "Average Vehicles per person",
                        "Percent of households without a car",
                        "Population Sensitivity"
                      ),
                      selected = "Log Population Density"
                    ),
                    mapUI("soc_map", height = "700")
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "primary",
                    barplotUI("soc_bar")
                  )
                  
                )),
              )),
      ### Definitions ----
      tabItem(tabName = "definitions",
              column(
                width = 10,
                offset = 1,
                style='padding-left:30px;padding-bottom: 30px;',
                h1(strong("Variables and Definitions"), style = "font-size:50px;"),
                fluidRow(
                  shinydashboard::box(
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    dataTableOutput("definitions")
                  )
                ),
                
              )),
      ### Attributions ----
      tabItem(
        tabName = "attributions",
        column(
          width = 10,
          offset = 1,
          style='padding-left:30px;padding-bottom: 30px;',
          h1(strong("Attributions"), style = "font-size:50px;"),
          fluidRow(
            userBox(
              title = userDescription(
                title = "Phoebe Romero",
                subtitle = "Environmental Program Coordinator",
                image = "images/Phoebe 2.jpg",
                type = 2
              ),
              footer = "City of Austin Office of Sustainability",
              status = "primary",
              "Phoebe Romero is passionate about the intersection of climate policy and racial equity. She currently works at the City of Austin Office of Sustainability focusing on air quality and climate action that reduces environmental impact and improves quality of life outcomes for historically impacted communities."
            ),
            userBox(
              title = userDescription(
                title = "Marc Coudert",
                subtitle = "Environmental Conservation Program Manager",
                image = "images/Coudert-sm.jpg",
                type = 2
              ),
              footer = "City of Austin Office of Sustainability",
              status = "primary",
              "As an employee of the City of Austin Office of Sustainability, Marc works with city departments to embed climate change resiliency into long term operation and asset management planning. In this role, he also supports community organizers to increase climate resilience in the Eastern Crescent."
            )
          ),
          fluidRow(
            userBox(
              title = userDescription(
                title = "Ethan Tenison",
                subtitle = "Project Manager for RGK Data Initiaves",
                image = "images/ethan.jpg",
                type = 2
              ),
              status = "primary",
              footer = "Austin Area Sustainability Indicators",
              "Ethan manages and evaluates the RGK Center's data initiatives at the University of Texas at Austin. He holds a masters degree in Global Policy Studies from the LBJ School of Public Affairs specializing in Data Science for Policy Analysis."
            ),
            userBox(
              title = userDescription(
                title = "Patrick Bixler",
                subtitle = "Assistant Professor",
                image = "images/thumbnail_Bixler Headshot.jpg",
                type = 2
              ),
              status = "primary",
              footer = "Austin Area Sustainability Indicators",
              "Patrick Bixler is an Assistant Professor at the LBJ School of Public Affairs, core faculty at the RGK Center for Philanthropy and Community Service, and has a joint appointment in the Community and Regional Planning program at the University of Texas. He directs the Austin Area Sustainability Indicators project and co-leads a Planet Texas 2050 Flagship initiative."
            ),
            userBox(
              title = userDescription(
                title = "Christiane Heggelund",
                subtitle = "Regional Planning and Services Program Coordinator",
                image = "images/CAlepuz Headshot.jpg",
                type = 2
              ),
              status = "primary",
              footer = "Capital Area Council of Governments",
              "Christiane Heggelund is the Regional Planning and Services Program Coordinator for the Capital Area Council of Governments (CAPCOG). In CAPCOG’s Air Quality Program, Christiane works on air quality monitoring, air quality education and outreach, and air quality analyses. She is a New Orleans, Louisiana, native, who has lived in Texas for 12 years. She holds a Bachelor of Science degree in Geology from Tulane University."
            )
          )
          
        )
      )
    )
  )
)








# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #

# Server ----
server <- function(input, output, session) {
  
  ### AQ ----
  
  #Variable to visualize
  air <- dataServer("air",data = austin_map)
  variable_air <- air$df
  selected_air <- air$var
  
  
  mapServer("air_map", data = variable_air, selected = selected_air)
  
  ### Environment ----
  
  #Environment Variables to visualize
  env <- dataServer("environment", data = austin_map)
  variable_env <- env$df
  selected_env <- env$var
  
  
  mapServer("env_map", data = variable_env, selected = selected_env)
  barplotServer("env_bar", data = variable_env)
  
  ### Health ----
  hel <- dataServer("health", data = health)
  variable_hel <- hel$df
  selected_hel <- hel$var
  
  
  mapServer("hel_map", data = variable_hel, selected = selected_hel)
  barplotServer("hel_bar", data = variable_hel)
  ### Social ----
  soc <- dataServer("social", data = austin_map)
  variable_soc <- soc$df
  selected_soc <- soc$var
  
  
  mapServer("soc_map", data = variable_soc, selected = selected_soc)
  barplotServer("soc_bar", data = variable_soc)
  
  ### Definitions ----
  
  #Definition Table
  output$definitions <- renderDataTable(DT::datatable(definitions,
                                                      options = list(pageLength = 10)))
  
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)