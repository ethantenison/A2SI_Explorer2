# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #


library(shiny)
library(shinyjs)
library(RColorBrewer)
library(dplyr)
library(readr)
library(tidyr)
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

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #

options(scipen = 999)

austin_map <- readRDS("./data/austin_composite.rds")
austin_map <- as.data.frame(austin_map)
austin_map <- st_as_sf(austin_map)
austin_map <-
  st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
austin_map$value <- as.numeric(austin_map$value)


austin_map <-
  austin_map %>% filter(GEOID_ != 480559601011 &
                          GEOID_ != 480559601012 &
                          GEOID_ != 484910203012)

var_choices <- unique(austin_map$var)

definitions <- read_csv("data/definitions.csv")

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



ui = shinydashboard::dashboardPage(
  skin = "black",
  header = shinydashboard::dashboardHeader(title = tagList(
    span(class = "logo-lg", tags$img(src = 'images/logo_skinny.png', width =
                                       '50%')),
    img(src = 'images/arrow.png', width = '150%')
  )),
  sidebar = shinydashboard::dashboardSidebar(
    useShinyjs(),
    shinyjs::extendShinyjs(text = jsToggleFS, functions = "toggleFullScreen"),
    collapsed = TRUE,
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Data Explorer",
        tabName = "data",
        icon = icon("project-diagram")
      ),
      conditionalPanel(condition = "input.tabs == 'data'"),
      menuItem(
        "Variable Definitions",
        tabName = "definitions",
        icon = icon("book")
      ),
      conditionalPanel(condition = "input.tabs == 'definitions'"),
      menuItem("About",
               tabName = "about",
               icon = icon("question")),
      conditionalPanel(condition = "input.tabs == 'about'"),
      menuItem("Attributions",
               tabName = "attributions",
               icon = icon("users")),
      conditionalPanel(condition = "input.tabs == 'attributions'"),
      hr(style = "margin-top: 5px; margin-bottom: 5px; width:90%"),
      HTML(
        "<button type='button' class='btn btn-default action-button shiny-bound-input' style='display: block;
        margin: 6px 5px 6px 15px; width: 200px;color: #152934;' onclick = 'shinyjs.toggleFullScreen();
        '><i class='fa fa-expand fa-pull-left'></i> Fullscreen</button>"
      )
      
    )
    
  ),
  body = shinydashboard::dashboardBody(
    tags$head(
      tags$style(
        HTML(
          '
.box {margin: 0px;}
# .box.box.box-primary>.box-header {
#   background:#fff
#                     }
#
# .box.box.box-primary{
# border-bottom-color:#29AF7F;
# border-left-color:#29AF7F;
# border-right-color:#29AF7F;
# border-top-color:#29AF7F;
# }
# .box.box-solid.box-primary>.box-header {
#   background:#29AF7F;
#                     }
#
# .box.box-solid.box-primary{
# border-bottom-color:#29AF7F;
# border-left-color:#29AF7F;
# border-right-color:#29AF7F;
# border-top-color:#29AF7F;
# }
#
#
# .box.box-solid.box-warning>.box-header {
#   background:#453781;
#                     }
#
# .box.box-solid.box-warning{
# background:#453781;
# border-bottom-color:#453781;
# border-left-color:#453781;
# border-right-color:#453781;
# border-top-color:#453781;
# }
#
#
# .small-box.bg-green { background-color: #29AF7F !important; color: #ffffff !important; }
# .small-box.bg-yellow { background-color: #DCE319 !important; color: #000000 !important; }
# .small-box.bg-purple { background-color: #453781!important; color: #ffffff !important; }
.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: black;
}
.dropdown-header .text { font-weight: bold }
.dataTables_filter {
display: none;
}

'
        )
      )
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
  tabItem(
    tabName = "data",
    
    fluidRow(
      column(
        width = 12,
        style = 'padding:25px;padding-left:17px;',
        offset = 0,
        shinydashboard::box(
          title = "Select a Variable",
          width = 4,
          height = "100px",
          solidHeader = TRUE,
          status = "success",
          background = "green",
          pickerInput(
            "var",
            label = NULL,
            width = '100%',
            inline = FALSE,
            options = list(`actions-box` = TRUE,
                           size = 10),
            choices =
              list(
                "Environmental Measures" = list(
                  "Wildfire Exposure",
                  "Heat Exposure",
                  "Multihazard Exposure",
                  "Population Sensitivity",
                  "Multihazard Exposure and Population Sensitivity",
                  "Average Impervious Cover",
                  "Average Tree Cover"
                ),
                "Air Pollutants" = list(
                  "CO",
                  "NO2",
                  "SO2",
                  "O3",
                  "Percentile for Ozone level in air",
                  "PM2.5",
                  "Percentile for PM2.5 level in air" ,
                  "PM10"
                ),
                "Demograpic Information" = list(
                  "Total population",
                  "% people of color",
                  "% low-income",
                  "Average Vehicles per person",
                  "Percent of households without a car"
                )
              ),
            selected = "Multihazard Exposure and Population Sensitivity"
          )
        ),
        valueBox(
          "1,342,588",
          "Total Population",
          color = "blue",
          icon = icon("users")
        ),
        valueBoxOutput("highrisk")
      )
    ),
    fluidRow(
      column(
        width = 8,
        style = 'padding:25px;padding-top:0px;padding-left:30px;',
        offset = 0,
        fluidRow(
          shinydashboard::box(
            title = "Austin Area by Census Block Group",
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            leafletOutput("bg", height = 600)
            
          )
        )
        
      ),
      column(
        width = 4,
        style = 'padding-left:25px; padding-top:0px; padding-right:42px;',
        offset = 0,
        fluidRow(
          shinydashboard::box(
            title = "Variable Information",
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            dataTableOutput("varinfo")
            
          )
        ),
        br(),
        br(),
        fluidRow(
          shinydashboard::box(
            title = textOutput("demographic"),
            width = 12,
            solidHeader = FALSE,
            status = "primary",
            plotlyOutput("barplot", height = "245px")
            
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 8,
        style = 'padding:25px;padding-left:17px;',
        offset = 0,
        shinydashboard::box(
          title = "Data for your address",
          width = 12,
          solidHeader = FALSE,
          status = "primary",
          searchInput(
            inputId = "search",
            label = "Click search icon to update or hit 'Enter'",
            placeholder = "A placeholder",
            value = "110 Jacob Fontaine Ln, Austin, TX",
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "100%"
          ),
          verbatimTextOutput(outputId = "address_coord")
        )
      )
    )
  ),
  tabItem(tabName = "definitions",
          fluidRow(
            shinydashboard::box(
              title = "Variables and definitions",
              width = 12,
              solidHeader = FALSE,
              status = "primary",
              dataTableOutput("definitions")
            ),
            
          )),
  tabItem(
    tabName = "about",
    fluidRow(
      shinydashboard::box(
        title = "Goals of this Project",
        width = 6,
        solidHeader = FALSE,
        status = "primary",
      ),
      shinydashboard::box(
        title = "About A2SI",
        width = 6,
        solidHeader = FALSE,
        status = "primary",
      ),
      
    )
  ),
  tabItem(
    tabName = "attributions",
    fluidRow(
      userBox(
        title = userDescription(
          title = "Phoebe Romero",
          subtitle = "Environmental Program Coordinator",
          image = "",
          type = 2
        ),
        status = "primary",
        ""
      ),
      userBox(
        title = userDescription(
          title = "Marc Coudert",
          subtitle = "Environmental Conservation Program Manager",
          image = "",
          type = 2
        ),
        status = "primary",
        ""
      )
    ),
    br(),
    br(),
    fluidRow(
      userBox(
        title = userDescription(
          title = "Ethan Tenison",
          subtitle = "Project Manager for RGK Data Initiaves",
          image = "images/ethan.jpg",
          type = 2
        ),
        status = "primary",
        "Ethan manages and evaluates the RGK Center's data initiatives at the University of Texas at Austin. He holds a masters degree in Global Policy Studies from the LBJ School of Public Affairs specializing in Data Science for Policy Analysis."
      ),
      userBox(
        title = userDescription(
          title = "Patrick Bixler",
          subtitle = "Assistant Professor",
          image = "",
          type = 2
        ),
        status = "primary",
        ""
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

server <- function(input, output, session) {
  #create the map
  output$bg <- renderLeaflet({
    leaflet(austin_map, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng = -97.74,
              lat = 30.30,
              zoom = 10)  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }")
  })
  
  
  
  variable <- reactive({
    austin_map %>% dplyr::filter(var == input$var)
  })
  
  
  #High Risk Value Box
  output$highrisk <- renderValueBox({
    highrisk <- variable() %>% filter(value >= 0.8)
    total <- sum(highrisk$`Total population`)
    
    valueBox(
      format(total, big.mark = ","),
      paste0("Population above 0.8    ", input$var),
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  

  #Color Palette for Map
  pal <- reactive({
    colorNumeric(
      palette = "RdBu",
      n = 10,
      reverse = TRUE,
      domain = variable()$value
    )
  })
  
  
  #Definition Table
  output$definitions <- renderDataTable(definitions)
  
  #Variable info table
  varinfo_reactive <- reactive({
    def <- filter(definitions, Variable == input$var)
    def <- t(def)
    colnames(def) <- " "
    def
  })
  
  output$varinfo <- DT::renderDataTable({
    DT::datatable(
      varinfo_reactive(),
      escape = FALSE,
      options = list(
        lengthChange = FALSE,
        info = FALSE,
        paging = FALSE,
        ordering = FALSE
      )
    )
  })
  
  
  #Map attributes to display
  observe({
    leafletProxy("bg", data = variable()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.7,
        fillColor = ~ pal()(variable()$value),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        label = ~ paste0(variable()$var,
                         ": ",
                         format(variable()$value, digits = 1)),
        
        popup =  ~ paste0(
          "<h5/><b>",
          variable()$var,
          ": ",
          format(variable()$value, digits = 1),
          "<h6/>",
          "Census Block Group: ",
          GEOID_,
          "<h6/>",
          "Total population: ",
          format(variable()$`Total population`, big.mark = ","),
          "<h6/>",
          "People of Color (%): ",
          format(variable()$`% people of color`, digits = 1),
          "<h6/>",
          "Low Income (%): ",
          format(variable()$`% low-income`, digits = 1)
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal(),
        values = ~ variable()$value,
        title = input$var
      )
  })
  
  #Violin Plot of Variable Selected
  output$violin <- renderPlotly({
    plot_ly(
      y = ~ variable()$value,
      type = 'violin',
      box = list(visible = T),
      color = I("#29AF7F"),
      x0 = input$var,
      hoverinfo = "none"
    )  %>%
      layout(yaxis = list(title = "",
                          zeroline = F)) %>%
      config(displayModeBar = FALSE)
    
    
    
  })
  
  output$name <-
    renderText({
      paste0("Variable Name:    ", input$var)
    })
  
  output$def <-
    renderText({
      def <- filter(definitions, Variable == input$var)
      paste0("Definition:    ", def[[2]])
    })
  
  output$source <-
    renderText({
      def <- filter(definitions, Variable == input$var)
      paste0("Source:    ", def[[3]])
    })
  
  #Data for barplot
  bar <- reactive({
    bar <-
      austin_map %>%
      dplyr::filter(var == input$var) %>%
      mutate(
        `> 50% People of Color` = if_else(`% people of color` >= 0.5, 1, 0),
        `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0)
      )
    
    total_av <- mean(bar$value)
    
    poc <- bar %>% filter(`> 50% People of Color` == 1)
    poc_av <- mean(poc$value)
    
    lowincome <- bar %>% filter(`> 50% Low Income` == 1)
    lowincome_av <- mean(lowincome$value)
    
    
    bar_to_plotly <-
      data.frame(
        y = c(total_av, poc_av, lowincome_av),
        x = c("Austin Average",
              "> 50% People of Color",
              "> 50% Low Income")
      )
    
    bar_to_plotly
  })
  
  
  # #Plotly Barplot
  output$barplot <- renderPlotly({
    plot_ly(
      x = bar()$x,
      y = bar()$y,
      color = I("#00a65a"),
      type = 'bar'
      
    ) %>%
      config(displayModeBar = FALSE)
    
  })
  
  
  #Bar Plot header
  output$demographic <-
    renderText({
      paste0(input$var, " By Major Demographic Groups")
    })
  
  #pop up on launch
  query_modal <- modalDialog(title = "Important message",
                             includeMarkdown("tooltips/intro.md"),
                             easyClose = F)
  
  showModal(query_modal)
  
  
  #Address Look up
  
  output$address_coord <- renderPrint({
    register_google(key = "", day_limit = 100000)
    lonlat <- geocode(location = input$search, output = "latlona")
    spatial_point <-
      st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)
    lonlat <- paste0(lonlat[1], ", ", lonlat[2])
    censusblock_tovisualize <- st_join(spatial_point, variable())
    print(paste0(input$var, ": ", censusblock_tovisualize[['value']]))
    print(paste0(input$var, "average: ", mean(censusblock_tovisualize[['value']])))
  })
  
  
  
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
