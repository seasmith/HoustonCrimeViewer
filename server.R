
# FUNCTION ----------------------------------------------------------------

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  leafData <- reactive({
    # No slider, for now
    filter(hpb_yearly, `Offense Type` == input$offType &
             year == as.integer(input$years))
  })
  
  colorPal <- reactive({
    colorNumeric("viridis", domain = unique(leafData()$rate), na.color = "gray10")
  })
  
  # Create the map
  output$map <- renderLeaflet({
    pal <- colorPal()
    bbox <- st_bbox(hpb_yearly)
    leaflet(filter(hpb_yearly, `Offense Type` == "Assaults" & year == 2017)) %>%
      addProviderTiles(providers$Esri,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      # setView(lng = -95.3, lat = 29.8, zoom = 11)
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) #%>%
    # mapOptions(zoomToLimits = "never")
  })
  
  labels <- reactive({
    sprintf(paste0("Police Beat: %s<br/>Offense Rate: %s<br/>",
                   "Offense Total: %s<br/>Offense Percent: %s<br/>",
                   "Population Total: %s<br/>Population Density: %s / sq mi"),
            leafData()$Beat, pretty(round(leafData()$rate, 0)),
            pretty(leafData()$n_offenses),
            percent(leafData()$prop_off),
            pretty(round(leafData()$pop, 0)),
            pretty(round(leafData()$den, 0))) %>%
      lapply(htmltools::HTML)
  })
  
  # Update map
  observe({
    pal <- colorPal()
    leafletProxy("map", data = leafData()) %>%
      clearShapes() %>%
      addPolygons(stroke = TRUE, weight = 1, color = "gray10",
                  fillOpacity = 0.5, fillColor = ~pal(rate),
                  highlightOptions = highlightOptions(
                    weight = 1.4, fillOpacity = 0.8, bringToFront = TRUE
                  ),
                  label = labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                  )
      )  %>%
      addLegend(position = "bottomright", pal = pal, values = ~rate, title = "Rate")
  })
}
