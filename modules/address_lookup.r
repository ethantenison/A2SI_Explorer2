source("data/key.r")
set_key(key = key)

address_lookup_UI <- function(id) {
  tagList(
    div(
      textInput(inputId = NS(id,"my_address"), label = NULL, width = "100%"),
      HTML(
        paste0(
          " <script>
                function initAutocomplete() {

                 var autocomplete =   new google.maps.places.Autocomplete(document.getElementById(NS(id,'my_address')),{types: ['geocode']});
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
                 Shiny.onInputChange(NS(id,'jsValue'), address);
                 Shiny.onInputChange(NS(id, 'jsValueAddressNumber'), address_number);
                 Shiny.onInputChange(NS(id,'jsValuePretty'), addressPretty);
                 Shiny.onInputChange(NS(id,'jsValueCoords'), coords);});}
                 </script>
                 <script src='https://maps.googleapis.com/maps/api/js?key=",
          key,
          "&libraries=places&callback=initAutocomplete' async defer></script>"
        )
      )
    )
  )
}

address_lookup_Server <- function(id, data, var) {
  moduleServer(id, function(input, output, session) {
    
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
        full_blocks <- data()
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
    
    
    #Rendering Text for address look up
    output$full_address <- renderText({
      if (!is.null(my_address())) {
        register_google(key = key, day_limit = 100000)
        lonlat <- geocode(location = my_address(), output = "latlona")
        spatial_point <-
          st_as_sf(lonlat, coords = c("lon", "lat"), crs = 4326)
        lonlat <- paste0(lonlat[1], ", ", lonlat[2])
        censusblock_tovisualize <-
          st_join(spatial_point, data(), left = FALSE)
        print(paste0(var(), ": ", censusblock_tovisualize[['value']]))
        print(paste0(var(), "average: ", mean(censusblock_tovisualize[['value']])))
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
    
    list(
      zoom_block,
      clearaddress
    )
    
    
  })
}




###############################################################################

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
    HTML(#Address lookup code
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





#part of mapping
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

#added after set view 
if (clearaddress() == "clear") {
  proxy <- proxy |>
    removeShape(layerId = "address_lookup")
} else {
  proxy <- proxy
}

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