#Data Set Module

dataUI <- function(id, choices, selected) {
  #' choices = drop down options
  #' selected = choice that is preselected
  tagList(fluidRow(
    column(
      width = 5,
      pickerInput(
        NS(id, "var"),
        label = div(style = "font-size:25px", "Select Variable:  "),
        width = 'fit',
        inline = "TRUE",
        options = list(`actions-box` = TRUE,
                       size = 10),
        choices = choices,
        selected = selected
      )
    ),
    column(
      width = 5,
      pickerInput(
        NS(id, "geo"),
        label = div(style = "font-size:25px", "Select Counties:  "),
        width = 'fit',
        inline = "TRUE",
        choices = list(
          " Bastrop County",
          " Blanco County",
          " Burnet County",
          " Caldwell County",
          " Fayette County",
          " Hays County",
          " Lee County",
          " Llano County",
          " Travis County",
          " Williamson County"
        ),
        selected = " Travis County",
        multiple = TRUE,
        options = list(`actions-box` = TRUE,
                       size = 10,
                       `selected-text-format` = "count > 1",
                       `count-selected-text`= "{0} Selected",
                       `none-selected-text` = "None Selected")
      )
    )
  ))
}

dataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    #' data = data to be filtered
    
    #Variable Info Table
    definitions <- read_csv("data/definitions.csv")
    
    varinfo_reactive <- reactive({
      def <- definitions |> filter(Variable == input$var) |> 
        dplyr::select(-c(Variable, "Additional Information", Units))
      def <- t(def)
      colnames(def) <- " "
      def
    })
    
    
    #Variable and Data Reactivity 
    austin_map <- data
    austin_map <- as.data.frame(austin_map)
    austin_map <- st_as_sf(austin_map)
    austin_map <-
      st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
    austin_map$value <- as.numeric(austin_map$value)
    austin_map$value[austin_map$value == 0] <- NA
    
    
    to_be <- function(df, test){
      if(test != "Ozone - CAPCOG" & test != "PM2.5 - CAPCOG")
        dplyr::filter(df, county %in% input$geo)
      else df
    }

    
    list(#It's important to wrap outputs as reactive 
      var = reactive(input$var),
      df = reactive(austin_map |>
                      dplyr::filter(var == input$var) |>
                      to_be(test = input$var)
    )
    
    )
  
    
    
    
  })
}