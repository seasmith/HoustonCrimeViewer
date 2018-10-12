
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
    leaflet() %>%
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
    
    if (is.null(input$map_center))
      c(setLat, setLon)
    else input$map_center
    
  })

    # Update map
  observe({
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
  
  # LINES
  output$yearly_trends <- renderPlotly({
    
    if (is.null(data$clickedShape)) {
      
      ggplotly(
        hpb_yearly_summed %>%
          # setDT() %>%
          .[, .(Year = year, Rate = round(rate, 1))] %>%
          ggplot() +
          geom_line(aes(x = Year,
                        y = Rate),
                    color = "gray50", size = 0.8) +
          facet_wrap(~`Offense Type`, scales = "free_y") +
          scale_x_continuous(NULL, expand = expand_scale(c(0.1, 0.1)), breaks = pretty_breaks(3)) +
          scale_y_continuous(expand = expand_scale(c(0.1, 0.1)), breaks = pretty_breaks(3)) +
          labs(title = paste0("2010-2017 Crime in Houston"),
               x = NULL,
               y = paste0("Crime Rate (Per 100,000)")) +
          theme_minimal(),
        height = 400, width = 600
      ) %>%
        layout(xaxis = list(title = FALSE), hovermode = "compare")
      
    } else {

      ggplotly(
        hpb_yearly %>%
          filter(Beat == input$map_shape_click$id) %>%
          as.data.frame() %>%
          select(-geometry) %>%
          dplyr::union(select(as.data.frame(hpb_yearly_summed), -geometry)) %>%
          as_tibble() %>%
          rename(Year = year, Rate = rate) %>%
          mutate(Rate = round(Rate, 1)) %>%
          ggplot() +
          geom_line(aes(x = Year, y = Rate, color = Beat), size = 0.8) +
          scale_x_continuous(NULL, expand = expand_scale(c(0.1, 0.1)), breaks = pretty_breaks(3)) +
          scale_y_continuous(expand = expand_scale(c(0.1, 0.1)), breaks = pretty_breaks(3)) +
          facet_wrap(~`Offense Type`, scales = "free_y") +
          scale_color_manual(values = c("brown", "gray50")) +
          guides(color = guide_legend(title = "")) +
          labs(title = paste0("Crime in Houston 2010-2017"),
               x = NULL,
               y = paste0("Crime Rate (Per 100,000)")) +
          theme_minimal(),
        height = 400, width = 600
      ) %>%
        layout(xaxis = list(title = FALSE), hovermode = "compare")
    }
    
  })
  
  # DENSITY
  output$density <- renderPlotly({
    
    output_density <- hpb_yearly %>%
      filter(year == input$years) %>%
      rename(Rate = rate) %>%
      mutate(Rate = round(Rate, 1)) %>%
      ggplot() +
      geom_histogram(aes(Rate), fill = "gray50") +
      scale_x_continuous(expand = expand_scale(c(0.1, 0.1)), breaks = pretty_breaks(3)) +
      scale_y_continuous(expand = expand_scale(c(0.1, 0.1)), breaks = pretty_breaks(2)) +
      facet_wrap(~`Offense Type`, scales = "free") +
      theme_minimal()
    
    if (is.null(data$clickedShape)) {
      output_density <- output_density +
        labs(title = paste0(input$years, " City of Houston Crime Distribution"),
             x = paste0("Crime Rate (Per 100,000)"),
             y = paste0("Number of Beats"))
    } else {
      output_density <- output_density +
        geom_vline(aes(xintercept = Rate),
                   data = filter(hpb_yearly, year == input$years, Beat == input$map_shape_click$id) %>%
                     rename(Rate = rate) %>%
                     mutate(Rate = round(Rate, 1)),
                   color = "brown", size = 0.6, alpha = 0.8) +
        # labs(title = paste0(input$years, " Beat ", input$map_shape_click$id, " Crime Distribution"),
        labs(title = paste0(input$years, " City of Houston Crime Distribution"),
             x = paste0("Crime Rate (Per 100,000)"),
             y = paste0("Number of Beats"))
    }
    
    ggplotly(output_density, height = 400, autosize = TRUE, width = 600) %>%
      layout(hovermode = "compare")
      
  })
  
  # DAYS AND HOURS
  output$day_and_hour_trends <- renderPlotly({
    if (is.null(data$clickedShape)) {
      # ggplot2:
      ggplotly(
        hpb_hourly_summed %>%
          filter(`Offense Type` == input$offType &
                   year == input$years) %>%
          group_by(hour, week_day) %>%
          summarize(n_offenses_sum  = sum(n_offenses, na.rm = TRUE),
                    n_offenses_mean = mean(n_offenses, na.rm = TRUE)) %>%
          ungroup() %>%
          ggplot(aes(hour, fct_rev(week_day), fill = n_offenses_sum,
                     text = paste0("Offenses: ", round(n_offenses_sum, 1), "\n",
                                   "Day: ", fct_rev(week_day), "\n",
                                   "Hour: ", x_labs[hour]))) +
          geom_tile() +
          scale_fill_viridis_c("Number of Offenses", option = "B") +
          scale_x_continuous(NULL, expand = expand_scale(),
                             breaks = seq(0, 20, 5), labels = x_labs[seq(1, 21, 5)]) +
          scale_y_discrete(NULL, expand = expand_scale()) +
          labs(title = paste0(input$years, " Day-Time Distribution of ", input$offType, " in Houston"),
               x = NULL, y = NULL) +
          theme_minimal(),
        height = 400, autosize = TRUE, width = 600, tooltip = "text"
      )
    } else {
      # ggplot2:
      ggplotly(
        hpb_hourly_summed %>%
          filter(`Offense Type` == input$offType &
                   Beat == input$map_shape_click$id &
                   year == input$years) %>%
          group_by(hour, week_day) %>%
          summarize(n_offenses_sum  = sum(n_offenses, na.rm = TRUE),
                    n_offenses_mean = mean(n_offenses, na.rm = TRUE)) %>%
          ungroup() %>%
          ggplot(aes(hour, fct_rev(week_day), fill = n_offenses_sum,
                     text = paste0("Offenses: ", round(n_offenses_sum, 1), "\n",
                                   "Day: ", fct_rev(week_day), "\n",
                                   "Hour: ", x_labs[hour]))) +
          geom_tile() +
          scale_fill_viridis_c("Number of Offenses", option = "B") +
          scale_x_continuous(NULL, expand = expand_scale(),
                             breaks = seq(0, 20, 5), labels = x_labs[seq(1, 21, 5)]) +
          scale_y_discrete(NULL, expand = expand_scale()) +
          labs(title = paste0(input$years, " Day-Time Distribution of ", input$offType, " in Beat ", input$map_shape_click$id),
               x = NULL, y = NULL) +
          theme_minimal(),
        height = 400, autosize = TRUE, width = 600
      )
    }
  })
  
}
