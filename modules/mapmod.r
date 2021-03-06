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
        setView(lng = -97.75242943917551,
                lat = 30.327729034791303,
                zoom = 10)  |>
        addProviderTiles(providers$CartoDB.Positron) |>
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") |> 
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
            data()$GEOID,
            "<h5/><b>",
            data()$var,
            ": ",
            format(data()$value, digits = 1),
            "<h6/>",
            "Total population: ",
            format(data()$`population`, big.mark = ","),
            "<h6/>",
            "% White: ",
            format(data()$`% White-Alone`, digits = 1),
            "<h6/>",
            "% Black: ",
            format(data()$`% Black-Alone`, digits = 1),
            "<h6/>",
            "% Asian: ",
            format(data()$`% Asian-Alone`, digits = 1),
            "<h6/>",
            "% Hispanic: ",
            format(data()$`% Hispanic`, digits = 1),
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

    })
    
    palwithoutna <- reactive({
      colorNumeric(
        palette = "Blues",
        reverse = FALSE,
        domain = data()$value,
        na.color=rgb(0,0,0,0)
      )
    })
    
    #Map attributes to display
    observe({
      if (selected() == "Flood Exposure"){
        

      flood <- readRDS("./data/flood_polygon.rds")
      
      proxy <- leafletProxy("map", data = data()) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0,
          fillOpacity = 0.9,
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
            "Total population: ",
            format(data()$`population`, big.mark = ","),
            "<h6/>",
            "% White: ",
            format(data()$`% White-Alone`, digits = 1),
            "<h6/>",
            "% Black: ",
            format(data()$`% Black-Alone`, digits = 1),
            "<h6/>",
            "% Asian: ",
            format(data()$`% Asian-Alone`, digits = 1),
            "<h6/>",
            "% Hispanic: ",
            format(data()$`% Hispanic`, digits = 1),
            "<h6/>",
            "Low Income (%): ",
            format(data()$`% low-income`, digits = 1
            )
          )
        ) |>
        addPolygons(
          data = flood,
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0,
          fillOpacity = 0.2,
          fillColor = "#000000") |> 
        addLegend(
          "bottomright",
          pal = palwithoutna(),
          values = ~ data()$value,
          title = legend_title(),
          na.label = ""
        ) 
      
      proxy
      
      }  
      
      
    })
    
#    30.277729034791303, -97.75242943917551
    
    #Color Palette for Map
    pal <- reactive({
      if (selected() == "Flood Exposure") {
        colorNumeric(
          palette = "Blues",
          reverse = FALSE,
          domain = data()$value,
          na.color=rgb(0,0,0,0)
        )
      } else if (selected() == "Average Tree Cover"){
        colorNumeric(
          palette = "Greens",
          reverse = FALSE,
          domain = data()$value,
          na.color = NA #"#D3D3D3"
        )
      } else if (selected() == "Asthma ED incidence" |
                 selected() == "Asthma ED incidence Children" |
                 selected() == "Asthma ED incidence Adults"){
        mybins <- c(0,50,100,150,200,250,300,550)
        colorBin(
          palette = "Reds",
          reverse = FALSE,
          domain = data()$value,
          bins = mybins,
          na.color = NA #"#D3D3D3"
        )
      } 
      else {
        colorNumeric(
          palette = "Reds",
          reverse = FALSE,
          domain = data()$value,
          na.color = NA #"#D3D3D3"
        )
      }
    })
    
    
    #Legend Title
    definitions <- read_csv("data/definitions.csv") |>
      mutate(Units = replace_na(Units, ""))
    
    legend_title <- reactive({
      units <- definitions |> dplyr::filter(Variable == selected())
      units <- units$Units
      
      paste0(selected(), "</br>", "<h5>", units, "</h5>")
      
    })
    
    
    
  })
  }

