
# FUNCTION ----------------------------------------------------------------

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  data <- reactiveValues(clickedShape = NULL)
  observeEvent(input$map_shape_click, {data$clickedShape <- input$map_shape_click})
  observeEvent(input$resetBeatSelection, {data$clickedShape <- NULL})
    
  leafData <- reactive({
    # No slider, for now
    filter(hpb_yearly, `Offense Type` == input$offType &
             year == as.integer(input$years))
  })
  
  colorPal <- reactive({
    colorNumeric("viridis", domain = unique(leafData()$rate), na.color = "gray10")
  })
  
  setLat <- -95.5
  setLon <- 29.8
  
  # Create the map
  output$map <- renderLeaflet({
    pal <- colorPal()
    # bbox <- st_bbox(hpb_yearly)
    leaflet(filter(hpb_yearly, `Offense Type` == "Assaults" & year == 2017)) %>%
      addProviderTiles(providers$Esri,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      # fitBounds(-95.77115, 29.52334 - 0.2, -95.01642, 30.11036)
      setView(setLat, setLon, zoom = 10)
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
  
  zoom <- reactive({
    ifelse(is.null(input$map_zoom), 10 , input$map_zoom)
  })
    
  center <- reactive({
    
    if(is.null(input$map_center)){
      return(c(setLat, setLon))
    }else{
      return(input$map_center)
    }
    
  })
  # Update map
  observe({
    # zoom <- ifelse(is.null(input$map_zoom), 10 , input$map_zoom)
    # pal <- colorPal()
    leafletProxy("map", data = leafData()) %>%
      setView(lng = isolate(center())[1],
              lat = isolate(center())[2],
              zoom = isolate(zoom())) %>%
      clearShapes() %>%
      addPolygons(layerId = ~Beat,
                  stroke = TRUE, weight = 1, color = "gray10",
                  fillOpacity = 0.5, fillColor = ~(colorPal())(rate),
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
      addLegend(position = "bottomleft", pal = colorPal(), values = ~rate, title = "Rate")
  })
}
