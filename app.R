# Libraries ----
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

# Data & Scripts ----

options(scipen = 999)
source("data/key.r")
set_key(key = key)

#Modules
source("modules/datamod_v2.r")
source("modules/barplot.r")
source("modules/mapmod.r")

aq <- readRDS("./data/aq.rds")
env <- readRDS("./data/environment.rds")
soc <- readRDS("./data/soc.rds")
health <- readRDS("./data/asthma_for_app.rds")

#Data sources not in modules
definitions <- read_csv("data/definitions.csv") |>
  dplyr::select(-c("Additional Information")) |>
  mutate(Units = replace_na(Units, ""))


# UI ----
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


ui = dashboardPage(
  skin = "green",
  title = "A2SI",
  header = shinydashboard::dashboardHeader(title = tagList(
    span(
      class = "logo-lg",
      style = "text-align: center;",
      tags$img(src = 'images/logo_skinny2.png', width =
                 '50%', heigh = '100%')
    ),
    img(src = 'images/arrow.png', width = '150%', heigh = '100%')
  )),
  sidebar = shinydashboard::dashboardSidebar(
    useShinyjs(),
    shinyjs::extendShinyjs(text = jsToggleFS, functions = "toggleFullScreen"),
    introjsUI(),
    collapsed = FALSE,
    ## Sidebar ----
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome",
               tabName = "welcome",
               icon = icon("search")),
      conditionalPanel(condition = "input.tabs == 'welcome'"),
      menuItem("Air Quality",
               tabName = "air",
               icon = icon("wind")),
      conditionalPanel(condition = "input.tabs == 'air'"),
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
      menuItem(
        "Attributions",
        tabName = "attributions",
        icon = icon("info-circle")
      ),
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
        '// Sets the name next to navbar
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass", style="color:white"><b>Austin Area Sustainability Indicators </b></span>\');
      }),
      $(document).ready(function(){
  $("a[data-toggle=tab]").on("show.bs.tab", function(e){
    Shiny.setInputValue("activeTab", $(this).attr("data-value"));
  });
});
     '
      )
    ),
    tabItems(
      ### Welcome ----
      tabItem(tabName = "welcome",
              column(
                width = 12,
                fluidRow(
                  shinydashboard::box(
                    title = NULL,
                    width = 12,
                    solidHeader = TRUE,
                    background = "green",
                    status = "success",
                    br(),
                    h1(strong("Welcome to the A2SI Dashboard!"),
                       style = "font-size:5em;"),
                    h3(
                      "This project is brought to you by the Austin Area Sustainability Indicators
                     project (A2SI) in collaborations with the UT Austin Center for Health Environment Education Research (CHEER),
                     the City of Austin Office of Sustainability,
                     and the Capital Area Council of Governments (CAPCOG)."
                    ),  
                    hr(),
                    div(
                      style = "text-align: center;",
                    actionButton("welcome_bt", label = "Welcome Guide", width = '25%',
                                 icon = icon("book-open"))
                    )
                  )
                ),
                column(6,
                       fluidRow(
                         br(),
                         br(),
                         div(
                           id = "image1",
                           style = "text-align: center;",
                           a(
                             href = "http://www.austinindicators.org/",
                             img(
                               src = "images/AASI_logo_v1b-01.png",
                               width = "80%",
                               height = "100%"
                             )
                           )
                         )
                       ),
                       fluidRow(
                         br(),
                         br(),
                         div(
                           id = "image2",
                           style = "text-align: center;",
                           a(
                             href = "https://dellmed.utexas.edu/units/center-for-health-environment-education-research",
                             img(
                               src = "images/cheer.png",
                               width = "100%",
                               height = "100%"
                             )
                           )
                         )
                       )),
                column(6,
                       fluidRow(
                         br(),
                         br(),
                         div(
                           id = "image3",
                           style = "text-align: center;",
                           a(href = "https://www.austintexas.gov/department/sustainability",
                             img(
                               src = "images/aos.png",
                               width = "100%",
                               height = "100%"
                             ))
                         )
                       )
                       ,
                       fluidRow(
                         br(),
                         br(),
                         div(
                           id = "image4",
                           style = "text-align: center;",
                           a(
                             href = "https://www.capcog.org/",
                             img(
                               src = "images/capcog.png",
                               width = "35%",
                               height = "100%"
                             )
                           )
                         )
                       ))
              )),
      ### AQ ----
      tabItem(tabName = "air",
              column(
                width = 12,
                offset = 0,
                fluidRow(
                  column(style = 'padding-left:30px;padding-bottom: 15px;padding-top: 0px;',
                         width = 9,
                         h1(strong("Air Quality"))),
                  column(style = 'padding-right:30px;padding-bottom: 15px;padding-top: 30px;',
                         width = 3,
                         actionButton("aq_bt", label = "Guide", width = '100%',
                                      icon = icon("book-open"))
                )),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "success",
                    dataUI(
                      #Selector UI
                      "air",
                      choices = list(
                        "Ozone - CAPCOG",
                        "PM2.5",
                        "PM2.5 - CAPCOG",
                        "Percentile for PM2.5 level in air"
                      ),
                      selected = "PM2.5"
                    ),
                    mapUI("air_map", height = "650") #Map UI
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "success",
                    br(),
                    br(),
                    plotsUI("aq_bar")
                  )
                  
                ))
              )),
      ### Environment ----
      tabItem(tabName = "environment",
              column(
                width = 12,
                offset = 0,
                fluidRow(
                  column(style = 'padding-left:30px;padding-bottom: 15px;padding-top: 0px;',
                         width = 6,
                         h1(strong("Environment"))),
                  column(width = 4)
                ),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "success",
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
                    mapUI("env_map", height = "650")
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "success",
                    br(),
                    br(),
                    plotsUI("env_bar")
                  )
                  
                ))
              )),
      ### Health ----
      tabItem(tabName = "health",
              column(
                width = 12,
                offset = 0,
                fluidRow(
                  column(style = 'padding-left:30px;padding-bottom: 15px;padding-top: 0px;',
                         width = 6,
                         h1(strong("Health"))),
                  column(width = 4)
                ),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "success",
                    dataUI(
                      "health",
                      choices = list(
                        "Asthma Prevalence",
                        "Asthma Prevalence in Children",
                        "Asthma Prevalence in Adults"
                      ),
                      selected = "Asthma Prevalence"
                    ),
                    mapUI("hel_map", height = "650")
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "success",
                    plotsUI("hel_bar")
                  )
                  
                ))
              )),
      ### Social ----
      tabItem(tabName = "social",
              column(
                width = 12,
                offset = 0,
                fluidRow(
                  column(style = 'padding-left:30px;padding-bottom: 15px;padding-top: 0px;',
                         width = 6,
                         h1(strong(
                           "Social Vulnerability"
                         ))),
                  column(width = 4)
                ),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "success",
                    dataUI(
                      "social",
                      choices = list(
                        "Log Population Density",
                        "% White-Alone",
                        "% Black-Alone",
                        "% Asian-Alone",
                        "% Hispanic",
                        "% low-income",
                        "% under age 5",
                        "% over age 64",
                        "Average Vehicles per person",
                        "Percent of households without a car",
                        "Population Sensitivity"
                      ),
                      selected = "Log Population Density"
                    ),
                    mapUI("soc_map", height = "650")
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "success",
                    plotsUI("soc_bar")
                  )
                  
                )),
              )),
      ### Definitions ----
      tabItem(
        tabName = "definitions",
        column(
          width = 10,
          offset = 1,
          style = 'padding-left:30px;padding-bottom: 15px;padding-top: 0px;',
          h1(strong("Variables and Definitions")),
          fluidRow(
            shinydashboard::box(
              width = 12,
              solidHeader = FALSE,
              status = "success",
              dataTableOutput("definitions", height = "600px")
            )
          ),
          
        )
      ),
      ### Attributions ----
      tabItem(
        tabName = "attributions",
        column(
          width = 10,
          offset = 1,
          style = 'padding-left:30px;padding-bottom: 15px;padding-top: 0px;',
          h1(strong("Attributions")),
          fluidRow(
            userBox(
              title = userDescription(
                title = "Phoebe Romero",
                subtitle = "Environmental Program Coordinator",
                image = "images/Phoebe 2.jpg",
                type = 2
              ),
              footer = "City of Austin Office of Sustainability",
              status = "success",
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
              status = "success",
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
              status = "success",
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
              status = "success",
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
              status = "success",
              footer = "Capital Area Council of Governments",
              "Christiane Heggelund is the Regional Planning and Services Program Coordinator for the Capital Area Council of Governments (CAPCOG). In CAPCOG’s Air Quality Program, Christiane works on air quality monitoring, air quality education and outreach, and air quality analyses. She is a New Orleans, Louisiana, native, who has lived in Texas for 12 years. She holds a Bachelor of Science degree in Geology from Tulane University."
            )
          )
          
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  ### AQ ----
  
  #Variable to visualize
  air <- dataServer("air", data = aq)
  variable_air <- air$df
  selected_air <- air$var
  
  
  mapServer("air_map", data = variable_air, selected = selected_air)
  plotsServer("aq_bar", data = variable_air)
  
  ### Environment ----
  
  #Environment Variables to visualize
  env <- dataServer("environment", data = env)
  variable_env <- env$df
  selected_env <- env$var
  
  
  mapServer("env_map", data = variable_env, selected = selected_env)
  plotsServer("env_bar", data = variable_env)
  
  ### Health ----
  hel <- dataServer("health", data = health)
  variable_hel <- hel$df
  selected_hel <- hel$var
  
  
  mapServer("hel_map", data = variable_hel, selected = selected_hel)
  plotsServer("hel_bar", data = variable_hel)
  ### Social ----
  soc <- dataServer("social", data = soc)
  variable_soc <- soc$df
  selected_soc <- soc$var
  
  
  mapServer("soc_map", data = variable_soc, selected = selected_soc)
  plotsServer("soc_bar", data = variable_soc)
  
  ### Definitions ----
  
  #Definition Table
  output$definitions <- renderDataTable(DT::datatable(definitions,
                                                      options = list(pageLength = 10)))
  
  #######################################Guide buttons 
  observeEvent(
    input$welcome_bt, {
      showModal(modalDialog(
        title = "Welcome Guide",
        includeHTML(knitr::knit2html("tooltips/welcome_guide.md", fragment.only = TRUE)), #must knit
        easyClose = TRUE,
        size = "l",
        fade = TRUE
        
      ))
    }
    
  )
  
}

# Run App ----
shinyApp(ui = ui, server = server)