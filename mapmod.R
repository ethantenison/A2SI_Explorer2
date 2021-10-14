# Map Module 

mapUI <- function(id, height) {
  #' height = height of map output
  tagList(
    leafletOutput(NS(id, "map"), height = height)
  )
}



mapServer <- function(id, data, selected) {
  moduleServer(id, function(input, output, session) {
    #' data = data from data module
    #' selected = choice that is preselected from data module
    
    #create the map
    #This is where the passed data becomes reactive!
    output$map <- renderLeaflet({
      leaflet(data(), options = leafletOptions(zoomControl = FALSE)) |>
        setView(lng = -97.5330332291251,
                lat = 30.282125904548206,
                zoom = 9)  |>
        addProviderTiles(providers$CartoDB.Positron) |>
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }")
    })
    
    
    #Color Palette for Map
    pal <- reactive({
      if (selected() == "Flood Exposure") {
        colorNumeric(
          palette = "RdBu",
          n = 5,
          reverse = FALSE,
          domain = data()$value
        )
      } else {
        colorNumeric(
          palette = "RdBu",
          n = 5,
          reverse = TRUE,
          domain = data()$value
        )
      }
    })
    
    #Legend Title
    definitions <- read_csv("data/definitions.csv")
    
    legend_title <- reactive({
      units <- definitions |> dplyr::filter(Variable == selected())
      units <- units$Units
      
      paste0(selected(), "</br>", "<h5>", units, "</h5>")
      
    })
    
    
    #Map attributes to display
    observe({
      proxy <- leafletProxy("map", data = data()) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0,
          fillOpacity = 0.7,
          fillColor = ~ pal()(data()$value),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          label = ~ paste0(data()$var,
                           ": ",
                           format(data()$value, digits = 1)),
          
          popup =  ~ paste0(
            "<h5/><b>",
            data()$var,
            ": ",
            format(data()$value, digits = 1),
            "<h6/>",
            "Census Block Group: ",
            id,
            "<h6/>",
            "Total population: ",
            format(data()$`population`, big.mark = ","),
            "<h6/>",
            "People of Color (%): ",
            format(data()$`% people of color`, digits = 1),
            "<h6/>",
            "Low Income (%): ",
            format(data()$`% low-income`, digits = 1)
          )
        ) |>
        addLegend(
          "bottomright",
          pal = pal(),
          values = ~ data()$value,
          title = legend_title(),
          na.label = ""
        )
      
      proxy
    
    
    
    
    })
    
    
    
    
  })
  }

