#Data Set Module

dataUI <- function(id, choices, selected) {
  #' choices = drop down options
  #' selected = choice that is preselected
  tagList(
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
  )
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
    
    
    list(#It's important to wrap outputs as reactive 
      var = reactive(input$var),
      df = reactive(austin_map |> dplyr::filter(var == input$var))
    )
    
  
  })
}