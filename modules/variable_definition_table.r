#Variable Definition Table 
definitionUI <- function(id) {
  tagList(
    dataTableOutput(NS(id,"varinfo"))
  )
}

definitionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    df <- data
    var <- unique(df$var)
    definitions <- read_csv("data/definitions.csv")
    
    
    varinfo_reactive <- reactive({
      def <- definitions |> filter(Variable == var) |> 
        select(-c(Variable))
      def <- t(def)
      colnames(def) <- " "
      return(def)
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
    
    
  })
}