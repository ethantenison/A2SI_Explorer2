#Data Set Module

dataUI_v2 <- function(id, choices, selected) {
  tagList(
    pickerInput(
      NS(id, "var"),
      label = "Select:  ",
      width = 'fit',
      inline = "TRUE",
      options = list(`actions-box` = TRUE,
                     size = 10),
      choices = choices,
      selected = selected
    ),
    dataTableOutput(
      NS(id,"varinfo")
    )
  )
}

dataServer_v2 <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #Variable Info Table
    definitions <- read_csv("data/definitions.csv")
    
    varinfo_reactive <- reactive({
      def <- definitions |> filter(Variable == input$var) |> 
        select(-c(Variable, "Additional Information", Units))
      def <- t(def)
      colnames(def) <- " "
      def
    })
    
    # output$varinfo <- DT::renderDataTable({
    #   DT::datatable(
    #     varinfo_reactive(),
    #     escape = FALSE,
    #     options = list(
    #       lengthChange = FALSE,
    #       info = FALSE,
    #       paging = FALSE,
    #       ordering = FALSE
    #     )
    #   )
    # })
    
    
    
    #Variable and Data Reactivity 
    austin_map <- readRDS("./data/austin_composite.rds")
    austin_map <- as.data.frame(austin_map)
    austin_map <- st_as_sf(austin_map)
    austin_map <-
      st_transform(austin_map, "+proj=longlat +ellps=WGS84 +datum=WGS84")
    austin_map$value <- as.numeric(austin_map$value)
    austin_map$value[austin_map$value == 0] <- NA
    
    
    list(
      var = reactive(input$var),
      df = reactive(austin_map |> dplyr::filter(var == input$var))
    )
    
  
  })
}