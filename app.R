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

austin_map <- readRDS("./data/austin_composite.rds")
austin_map <- as.data.frame(austin_map)
austin_map <- st_as_sf(austin_map)
austin_map <-
  st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
austin_map$value <- as.numeric(austin_map$value)


austin_map <-
  austin_map |> filter(GEOID_ != 480559601011 &
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
  body = shinydashboard::dashboardBody(tags$head(tags$style(
    HTML(
      '
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
  )),
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
                    "Flood Exposure",
                    "Multihazard Exposure",
                    "Population Sensitivity",
                    "Multihazard Exposure and Population Sensitivity",
                    "Average Impervious Cover",
                    "Average Tree Cover"
                  ),
                  "Air Hazards" = list(
                    "O3",
                    "Ozone - CAPCOG",
                    "Percentile for Ozone level in air",
                    "PM2.5",
                    "PM2.5 - CAPCOG",
                    "Percentile for PM2.5 level in air"
                  ),
                  "Demograpic Information" = list(
                    "Total population",
                    "Population Density",
                    "% people of color",
                    "% low-income",
                    "% under age 5",
                    "% over age 64",
                    "Average Vehicles per person",
                    "Percent of households without a car"
                  )
                ),
              selected = "Multihazard Exposure and Population Sensitivity"
            ),
           # ("Variable Information"),
            dataTableOutput("varinfo")
          ),
          
          shinydashboard::box(
            title = textOutput("demographic"),
            width = 12,
            solidHeader = FALSE,
            status = "success",
            background = "green",
            plotlyOutput("barplot", height = "300px")
          
          ),
          shinydashboard::box(
            title = "Find your Census Block Group",
            width = 12,
            solidHeader = FALSE,
            status = "danger",
            background = "red",
            div(
              textInput(inputId = "my_address", label = NULL, width = "100%"),
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
            leafletOutput("bg", height = 800)
            
            
          )
        )
        
      )
      
    )),
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
  #create the map
  output$bg <- renderLeaflet({
    leaflet(austin_map, options = leafletOptions(zoomControl = FALSE)) |>
      setView(lng = -97.74,
              lat = 30.30,
              zoom = 10)  |>
      addProviderTiles(providers$CartoDB.Positron) |>
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }")
  })
  
  
  #Variable Select for map 
  variable <- reactive({
    austin_map |> dplyr::filter(var == input$var)
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
      full_blocks <- austin_map |> dplyr::filter(var == input$var)
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
        censusblock_tovisualize[!is.na(censusblock_tovisualize$address), ]
      censusblock_tovisualize <-
        censusblock_tovisualize[['GEOID_']]
      tovisualize <-
        full_blocks |> filter(GEOID_ == censusblock_tovisualize) |> bind_cols(lonlat)
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
    colorNumeric(
      palette = "RdBu",
      n = 10,
      reverse = TRUE,
      domain = variable()$value
    )
  })
  
  labels <- reactive({
    
    mini <- as.character(signif(min(variable()$value)), digits = 2)
    max <- as.character(signif(max(variable()$value)), digits = 2)
    first <- as.character(signif(quantile(variable()$value, 0.25)), digits = 2)
    third <- as.character(signif(quantile(variable()$value, 0.75)), digits = 2)
    med <- as.character(signif(median(variable()$value)), digits = 2)
    
    labels <- c(paste0(mini, " Low"), first, med, third, paste(max, " High"))
    labels
  })
  
  #Definition Table
  output$definitions <- renderDataTable(
    
    DT::datatable(definitions,
                  options = list(
                  pageLength = 25)
    )
    )
  
  #Variable info table
  varinfo_reactive <- reactive({
    def <- definitions |> filter(Variable == input$var) |> 
      select(-c(Variable))
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
      ) |>
      addLegend(
        "bottomright",
        pal = pal(),
        values = ~ variable()$value,
        title = input$var,
        labFormat = function(type, cuts, p) {
          # Here's the trick
          paste0(labels())
        }
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
          label = ~ paste0(zoom_block()$var,
                           ": ",
                           format(zoom_block()$value, digits = 1)),
          
          popup =  ~ paste0(
            "<h5/><b>",
            zoom_block()$var,
            ": ",
            format(zoom_block()$value, digits = 1),
            "<h6/>",
            "Census Block Group: ",
            GEOID_,
            "<h6/>",
            "Total population: ",
            format(zoom_block()$`Total population`, big.mark = ","),
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
  

  
  #Data for barplot
  bar <- reactive({
    bar <-
      austin_map |>
      dplyr::filter(var == input$var) |>
      mutate(
        `> 50% People of Color` = if_else(`% people of color` >= 0.5, 1, 0),
        `> 50% Low Income` = if_else(`% low-income` >= 0.5, 1, 0)
      )
    
    total_av <- mean(bar$value)
    
    poc <- bar |> filter(`> 50% People of Color` == 1)
    poc_av <- mean(poc$value)
    
    lowincome <- bar |> filter(`> 50% Low Income` == 1)
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
      
    ) |>
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
      print(paste0(input$var, ": ", censusblock_tovisualize[['value']]))
      print(paste0(input$var, "average: ", mean(censusblock_tovisualize[['value']])))
    }
  })
  
  
  
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
