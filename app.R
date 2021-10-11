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
source("datamod.r")
source("barplot.r")

#Data sources not in modules
definitions <- read_csv("data/definitions.csv") |>
  select(-c("Additional Information")) |>
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
    sidebarMenu(
      id = "tabs",
      ######Welcome
      menuItem("Welcome Page",
               tabName = "welcome",
               icon = icon("search")),
      conditionalPanel(condition = "input.tabs == 'welcome'"),
      ######Air quality
      menuItem("Air Quality",
               tabName = "air",
               icon = icon("wind")),
      conditionalPanel(
        condition = "input.tabs == 'air'",
        actionButton(
          "help",
          "Tutorial",
          icon = icon("book-open",
                      class = "fa-pull-left"),
          width = '200px'
        )
      ),
      ######Environment
      menuItem("Environment",
               tabName = "environment",
               icon = icon("tree")),
      conditionalPanel(condition = "input.tabs == 'environment'"),
      ######Asmtha
      menuItem("Asthma",
               tabName = "asthma",
               icon = icon("plus-square")),
      conditionalPanel(condition = "input.tabs == 'asthma'"),
      ######Social Vulnerability
      menuItem(
        "Social Vulnerability",
        tabName = "social",
        icon = icon("users")
      ),
      conditionalPanel(condition = "input.tabs == 'social'"),
      menuItem(
        "Variable Definitions",
        tabName = "definitions",
        icon = icon("book")
      ),
      conditionalPanel(condition = "input.tabs == 'definitions'"),
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
      ######Welcome
      tabItem(
        tabName = "welcome",
        column(
          width = 10,
          offset = 1,
          fluidRow(
            shinydashboard::box(
              title = NULL,
              width = 12,
              solidHeader = FALSE,
              status = "primary",
              h1(
                "Welcome to the Austin area air pollution and population sensitivity dashboard!"
              ),
              h3(
                "This project is brought to you by the Austin Area Sustainability Indicators
                     project at the University of Texas
                     at Austin in collaborations with the City of Austin Office of Sustainability
                     and the Capital Area Council of Governments (CAPCOG)."
              )
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = NULL,
              width = 6,
              solidHeader = FALSE,
              status = "primary",
              div(style="text-align: center;", 
              img(src = "images/AASI_logo_v1b-01.png",
                  width = "75%"))
            ),
            shinydashboard::box(
              title = "About A2SI",
              width = 6,
              solidHeader = FALSE,
              status = "primary",
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
            
          ),
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
            )
          )
          
        )
      ),
      tabItem(##### Air
        tabName = "air",
        fluidRow(
          column(
            width = 3,
            style = 'padding:25px;padding-top:25px;padding-left:30px;',
            offset = 0,
            fluidRow(
              shinydashboard::box(
                title = "Select a Variable",
                width = 12,
                solidHeader = FALSE,
                status = "primary",
                background = "light-blue",
                div(id = "select_intro",
                    dataUI("data"))
              ),
              shinydashboard::box(
                title = textOutput("demographic"),
                width = 12,
                solidHeader = FALSE,
                status = "success",
                background = "green",
                div(id = "demo_intro",
                    barplotUI("barplot"))
              ),
              shinydashboard::box(
                title = "Find your Census Block Group",
                width = 12,
                solidHeader = FALSE,
                status = "danger",
                background = "red",
                div(
                  id = "my_address_intro",
                  textInput(
                    inputId = "my_address",
                    label = NULL,
                    width = "100%"
                  ),
                  HTML(
                    paste0(
                      " <script>
                function initAutocomplete() {

                 var autocomplete =   new google.maps.places.Autocomplete(document.getElementById('my_address'),{types: ['geocode']});
                 autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                 autocomplete.addListener('place_changed', function() {
                 var place = autocomplete.getPlace();
                 if (!place.geometry) {
                 return;
                 }

                 var addressPretty = place.formatted_address;
                 var address = '';
                 if (place.address_components) {
                 address = [
                 (place.address_components[0] && place.address_components[0].short_name || ''),
                 (place.address_components[1] && place.address_components[1].short_name || ''),
                 (place.address_components[2] && place.address_components[2].short_name || ''),
                 (place.address_components[3] && place.address_components[3].short_name || ''),
                 (place.address_components[4] && place.address_components[4].short_name || ''),
                 (place.address_components[5] && place.address_components[5].short_name || ''),
                 (place.address_components[6] && place.address_components[6].short_name || ''),
                 (place.address_components[7] && place.address_components[7].short_name || '')
                 ].join(' ');
                 }
                 var address_number =''
                 address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                 var coords = place.geometry.location;
                 //console.log(address);
                 Shiny.onInputChange('jsValue', address);
                 Shiny.onInputChange('jsValueAddressNumber', address_number);
                 Shiny.onInputChange('jsValuePretty', addressPretty);
                 Shiny.onInputChange('jsValueCoords', coords);});}
                 </script>
                 <script src='https://maps.googleapis.com/maps/api/js?key=",
                key,
                "&libraries=places&callback=initAutocomplete' async defer></script>"
                    )
                  )
                ),
                
              )
              
            )
          ),
          column(
            width = 9,
            style = 'padding-left:20px; padding-top:25px; padding-right:42px;',
            offset = 0,
            fluidRow(
              shinydashboard::box(
                title = "Austin Area by Census Block Group",
                width = 12,
                solidHeader = FALSE,
                status = "primary",
                background = "light-blue",
                div(id = 'map_intro',
                    leafletOutput("bg", height = 750))
                
              )
            )
            
          )
          
        )),
      ##### Environment
      tabItem(tabName = "environment",
              column(
                width = 10,
                offset = 1,
                fluidRow(
                  shinydashboard::box(
                    title = "Environment!",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary"
                  )
                ),
                
              )),
      ##### Asthma
      tabItem(tabName = "asthma",
              column(
                width = 10,
                offset = 1,
                fluidRow(
                  shinydashboard::box(
                    title = "Asthma!",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary"
                  )
                ),
                
              )),
      ##### Asthma
      tabItem(tabName = "social",
              column(
                width = 10,
                offset = 1,
                fluidRow(
                  shinydashboard::box(
                    title = "Social Vulnerability!",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary"
                  )
                ),
                
              )),
      tabItem(tabName = "definitions",
              column(
                width = 10,
                offset = 1,
                fluidRow(
                  shinydashboard::box(
                    title = "Variables and definitions",
                    width = 12,
                    solidHeader = FALSE,
                    status = "primary",
                    dataTableOutput("definitions")
                  )
                ),
                
              )),
      tabItem(
        tabName = "attributions",
        fluidRow(
          userBox(
            title = userDescription(
              title = "Phoebe Romero",
              subtitle = "Environmental Program Coordinator",
              image = "images/Phoebe 2.jpg",
              type = 2
            ),
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
            status = "primary",
            "As an employee of the City of Austin Office of Sustainability, Marc works with city departments to embed climate change resiliency into long term operation and asset management planning. In this role, he also supports community organizers to increase climate resilience in the Eastern Crescent."
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
              image = "images/thumbnail_Bixler Headshot.jpg",
              type = 2
            ),
            status = "primary",
            "Patrick Bixler is an Assistant Professor at the LBJ School of Public Affairs, core faculty at the RGK Center for Philanthropy and Community Service, and has a joint appointment in the Community and Regional Planning program at the University of Texas. He directs the Austin Area Sustainability Indicators project and co-leads a Planet Texas 2050 Flagship initiative."
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
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help,
               introjs(
                 session,
                 options = list(
                   steps = data.frame(
                     element = c(
                       "#select_intro",
                       "#demo_intro",
                       "#my_address_intro",
                       "#map_intro"
                     ),
                     intro = c(
                       includeMarkdown("tooltips/select_intro.md"),
                       includeMarkdown("tooltips/demographic_intro.md"),
                       includeMarkdown("tooltips/my_address_intro.md"),
                       includeMarkdown("tooltips/map_intro.md")
                     ),
                     position = c("auto",
                                  "auto",
                                  "auto",
                                  "auto")
                   ),
                   "nextLabel" = "Next",
                   "prevLabel" = "Previous",
                   "skipLabel" = "Exit"
                 ),
               ))
  
  #Variable to visualize
  data <- dataServer("data")
  variable <- data$df
  selected <- data$var
  
  
  #create the map
  output$bg <- renderLeaflet({
    leaflet(variable(), options = leafletOptions(zoomControl = FALSE)) |>
      setView(lng = -97.5330332291251,
              lat = 30.282125904548206,
              zoom = 9)  |>
      addProviderTiles(providers$CartoDB.Positron) |>
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }")
  })
  
  
  #Address Look up using googleway
  my_address <- reactive({
    if (!is.null(input$jsValueAddressNumber)) {
      if (length(grep(
        pattern = input$jsValueAddressNumber,
        x = input$jsValuePretty
      )) == 0) {
        final_address <- c(input$jsValueAddressNumber, input$jsValuePretty)
      } else{
        final_address <- input$jsValuePretty
      }
      final_address
    }
  })
  
  #Zoom in on address selected
  zoom_block <- reactive({
    if (!is.null(my_address())) {
      full_blocks <- variable()
      register_google(key = key, day_limit = 100000)
      lonlat <-
        geocode(location = my_address(), output = "latlona")
      spatial_point <-
        st_as_sf(
          lonlat,
          coords = c("lon", "lat"),
          crs = 4326,
          remove = FALSE
        )
      lonlat <- select(lonlat, lon, lat)
      censusblock_tovisualize <-
        st_join(spatial_point, full_blocks)
      censusblock_tovisualize <-
        censusblock_tovisualize[!is.na(censusblock_tovisualize$address),]
      censusblock_tovisualize <-
        censusblock_tovisualize[['id']]
      tovisualize <-
        full_blocks |> filter(id == censusblock_tovisualize) |> bind_cols(lonlat)
      tovisualize
    }
    
  })
  
  #Code to clear out highlighted block if none is selected
  clearaddress <- reactive({
    if (input$my_address == "" | is.na(input$my_address)) {
      clear <- "clear"
    } else {
      clear <- "don't clear"
    }
    
  })
  
  
  #Color Palette for Map
  pal <- reactive({
    if (selected() == "Flood Exposure") {
      colorNumeric(
        palette = "RdBu",
        n = 5,
        reverse = FALSE,
        domain = variable()$value
      )
    } else {
      colorNumeric(
        palette = "RdBu",
        n = 5,
        reverse = TRUE,
        domain = variable()$value
      )
    }
  })
  
  #Definition Table
  output$definitions <- renderDataTable(DT::datatable(definitions,
                                                      options = list(pageLength = 10)))
  
  #Legend Title
  legend_title <- reactive({
    units <- definitions |> dplyr::filter(Variable == selected())
    units <- units$Units
    
    paste0(selected(), "</br>", "<h5>", units, "</h5>")
  })
  
  #Map attributes to display
  observe({
    proxy <- leafletProxy("bg", data = variable()) |>
      clearShapes() |>
      clearControls() |>
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
          id,
          "<h6/>",
          "Total population: ",
          format(variable()$`Population`, big.mark = ","),
          "<h6/>",
          "People of Color (%): ",
          format(variable()$`% people of color`, digits = 1),
          "<h6/>",
          "Low Income (%): ",
          format(variable()$`% low-income`, digits = 1)
        )
      ) |>
      addLegend(
        "bottomright",
        pal = pal(),
        values = ~ variable()$value,
        title = legend_title(),
        na.label = ""
      )
    
    if (!is.null(my_address())) {
      proxy <- proxy |>
        addPolygons(
          layerId = "address_lookup",
          data = zoom_block(),
          color = "red",
          weight = 5,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 0.1,
          fillColor = "black",
          highlightOptions = highlightOptions(
            color = "red",
            weight = 3,
            bringToFront = TRUE
          ),
          label = ~ paste0(
            zoom_block()$var,
            ": ",
            format(zoom_block()$value, digits = 1)
          ),
          
          popup =  ~ paste0(
            "<h5/><b>",
            zoom_block()$var,
            ": ",
            format(zoom_block()$value, digits = 1),
            "<h6/>",
            "Census Block Group: ",
            id,
            "<h6/>",
            "Total population: ",
            format(zoom_block()$`Population`, big.mark = ","),
            "<h6/>",
            "People of Color (%): ",
            format(zoom_block()$`% people of color`, digits = 1),
            "<h6/>",
            "Low Income (%): ",
            format(zoom_block()$`% low-income`, digits = 1)
          )
        ) |>
        setView(lng = zoom_block()$lon,
                lat = zoom_block()$lat,
                zoom = 13)
    }
    
    if (clearaddress() == "clear") {
      proxy <- proxy |>
        removeShape(layerId = "address_lookup")
    } else {
      proxy <- proxy
    }
    
    proxy
    
    
    
  })
  
  #Plotly Barplot
  barplotServer("barplot", data = variable)
  
  #Bar Plot header
  output$demographic <-
    renderText({
      paste0(selected(), " By Major Demographic Groups")
    })
  
  #pop up on launch
  # query_modal <- modalDialog(title = "Important message",
  #                            includeMarkdown("tooltips/intro.md"),
  #                            easyClose = F)
  #
  # showModal(query_modal)
  
  
  #Rendering Text for address look up
  output$full_address <- renderText({
    if (!is.null(my_address())) {
      register_google(key = key, day_limit = 100000)
      lonlat <- geocode(location = my_address(), output = "latlona")
      spatial_point <-
        st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)
      lonlat <- paste0(lonlat[1], ", ", lonlat[2])
      censusblock_tovisualize <-
        st_join(spatial_point, variable(), left = FALSE)
      print(paste0(selected(), ": ", censusblock_tovisualize[['value']]))
      print(paste0(selected(), "average: ", mean(censusblock_tovisualize[['value']])))
    }
  })
  
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)