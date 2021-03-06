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
library(shinyhelper)
library(readxl)
library(rhandsontable)
library(profvis)

# Data & Scripts ----
options(scipen = 999)
source("data/key.r")
set_key(key = key)

#Modules
source("modules/datamod_v2.r")
source("modules/barplot.r")
source("modules/barplot_asthma.r")
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
                  #profvis_ui("profiler"),
                  shinydashboard::box(
                    title = NULL,
                    width = 12,
                    solidHeader = TRUE,
                    background = "green",
                    status = "success",
                    br(),
                    fluidRow(
                      column(
                        width = 9,
                    h1(strong("Welcome to the A2SI Dashboard!"),
                       style = "font-size:5em;")),
                    column(
                      width = 3,
                    div(
                      style = "text-align: center;padding-right:30px;
                      padding-bottom: 15px;padding-top: 30px;", 
                      appButton(
                        inputId = "welcome_bt",
                        label = "Welcome Guide",
                        enable_badge = TRUE, 
                        icon = icon("book-open"),
                        badgeColor = "red", 
                        badgeLabel = 3,
                        width = '100%'
                      ))
                    ))
                  )
                ),
                fluidRow(
                  column(12,
                         style = "padding-left:30px;",
                  h1("Brought to you by:"))
                ),
                column(6,
                       fluidRow(
                         br(),
                         br(),
                         div(
                           id = "image1",
                           style = "text-align: center;",
                           a(
                             target="_blank",
                             href = "http://www.austinindicators.org/",
                             img(
                               src = "images/AASI_logo_v1b-01.png",
                               width = "70%",
                               height = "90%"
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
                           a(target="_blank",
                             href = "https://dellmed.utexas.edu/units/center-for-health-environment-education-research",
                             img(
                               src = "images/cheer.png",
                               width = "90%",
                               height = "90%"
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
                             target="_blank",
                             img(
                               src = "images/aos.png",
                               width = "90%",
                               height = "90%"
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
                           a(target="_blank",
                             href = "https://www.capcog.org/",
                             img(
                               src = "images/capcog.png",
                               width = "30%",
                               height = "90%"
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
                         h1(strong("Air Quality")))),
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
                        "O3",
                        "Ozone - CAPCOG",
                        "PM2.5",
                        "PM2.5 - CAPCOG",
                        "Percentile for PM2.5"
                      ),
                      selected = "PM2.5"
                    ),
                    mapUI("air_map", height = "650") #Map UI
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "success",
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
                         width = 9,
                         h1(strong("Environment")))),
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
                        "Multihazard and Population Sensitivity",
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
                         width = 9,
                         h1(strong("Health")))),
                fluidRow(column(
                  width = 12,
                  shinydashboard::box(
                    width = 8,
                    solidHeader = FALSE,
                    status = "success",
                    dataUI(
                      "health",
                      choices = list(
                        "Asthma ED incidence",
                        "Asthma ED incidence Children",
                        "Asthma ED incidence Adults"
                      ),
                      selected = "Asthma ED incidence"
                    ),
                    mapUI("hel_map", height = "650")
                  ),
                  shinydashboard::box(
                    width = 4,
                    solidHeader = FALSE,
                    status = "success",
                    plotsUI2("hel_bar")
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
                         width = 9,
                         h1(strong(
                           "Social Vulnerability"
                         )))),
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
                        "Households without a car",
                        "Population Sensitivity"
                      ),
                      selected = "% low-income"
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
              "Ethan is a data scientist and project manager at the RGK Center for Philanthropy and Community at the University of Texas at Austin. As part of this project, he built the application using the R programming language, and hopes it can be used to help help policy makers and community to make informed decisions. He holds a masters degree in Global Policy Studies from the LBJ School of Public Affairs specializing in Data Science for Policy Analysis."
            ),
            userBox(
              title = userDescription(
                title = "Patrick Bixler",
                subtitle = "Assistant Professor at the University of Texas at Austin",
                image = "images/thumbnail_Bixler Headshot.jpg",
                type = 2
              ),
              status = "success",
              footer = "Austin Area Sustainability Indicators",
              "Patrick Bixler is an Assistant Professor at the LBJ School of Public Affairs, core faculty at the RGK Center for Philanthropy and Community Service, and has a joint appointment in the Community and Regional Planning program at the University of Texas. He directs the Austin Area Sustainability Indicators project and co-leads a Planet Texas 2050 Flagship initiative."
            )),
          fluidRow(
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
            ),
            userBox(
              title = userDescription(
                title = "Rebecca A. Zarate",
                subtitle = "PhD student in Quantitative Methods UT Austin",
                image = "images/rebecca_zarate.jpg",
                type = 2
              ),
              status = "success",
              footer = "Center for Health and Environment, Education, and Research",
              "Rebecca A. Zarate (aka RAZ) is a PhD student in Quantitative Methods at the University of Texas at Austin pursuing studies in spatial statistics. She works with Dr. Elizabeth Matsui in the Department of Population Health at Dell Medical School focusing on asthma incidence rates and factors that may contribute to these rates in the state of Texas. She also teaches a Statistical Literacy course to undergraduate students at UT-Austin. "
            )
          ),
          fluidRow(
            userBox(
              title = userDescription(
                title = "Elizabeth Matsui",
                subtitle = "Professor of population health and pediatrics",
                image = "images/elizabeth.jpg",
                type = 2
              ),
              status = "success",
              footer = "Center for Health and Environment, Education, and Research",
              "Elizabeth Matsui, M.D., M.H.S, is the associate chair for research in the Department of Population Health, professor of population health and pediatrics, director of the Center for Health and Environment: Education and Research, and associate director of the Health Transformation Research Institute at Dell Medical School. She is a pediatric allergist-immunologist and epidemiologist and a leading international expert on environmental exposures and their effects on asthma and other allergic conditions."
            )
        )
      )
    )
  )
))

# Server ----
server <- function(input, output, session) {
  
  #callModule(profvis_server, "profiler")
  ### AQ ----
  
  #Variable to visualize
  air <- dataServer("air", data = aq, info = "aq")
  variable_air <- air$df
  selected_air <- air$var
  
  
  mapServer("air_map", data = variable_air, selected = selected_air)
  plotsServer("aq_bar", data = variable_air)
  
  ### Environment ----
  
  #Environment Variables to visualize
  env <- dataServer("environment", data = env, info = "env")
  variable_env <- env$df
  selected_env <- env$var
  
  
  mapServer("env_map", data = variable_env, selected = selected_env)
  plotsServer("env_bar", data = variable_env)
  
  ### Health ----
  hel <- dataServer("health", data = health, info = "hel")
  variable_hel <- hel$df
  selected_hel <- hel$var
  
  
  mapServer("hel_map", data = variable_hel, selected = selected_hel)
  plotsServer2("hel_bar", data = variable_hel)
  ### Social ----
  soc <- dataServer("social", data = soc, info = "soc")
  variable_soc <- soc$df
  selected_soc <- soc$var
  
  
  mapServer("soc_map", data = variable_soc, selected = selected_soc)
  plotsServer("soc_bar", data = variable_soc)
  
  ### Information Buttons ----
  
  #Definition Table
  output$definitions <- renderDataTable(DT::datatable(definitions,
                                                      options = list(pageLength = 10)))

  ####################################### Welcome Button 
  observeEvent(
    input$welcome_bt, {
      showModal(modalDialog(
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

