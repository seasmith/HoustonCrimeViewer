# Template from: https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R

library(leaflet)

# Choices for drop-downs
crime_types <- c(
  "Aggravated Assaults" = "Aggravated Assaults",
  "Auto Thefts"         = "Auto Thefts",
  "Burglaries"          = "Burglaries",
  "Murders"             = "Murders",
  "Other Thefts"        = "Other Thefts",
  "Rapes"               = "Rapes",
  "Robberies"           = "Robberies"
)

year_values <- as.character(2010:2017)
names(year_values) <- as.character(2010:2017)


navbarPage("Houston Crime Data", id = "nav",
           
           tabPanel("Police Beat Map",
                    div(class = "outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("www/styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        absolutePanel(id    = "controls", class     = "panel panel-default",
                                      fixed = TRUE      , draggable = TRUE,
                                      top   = 60        , left      = "auto",
                                      right = 20        , bottom    = "auto",
                                      width = 330       , height    = "auto",
                                      
                                      h2("Crime Explorer"),
                                      
                                      selectInput("offType", "Crime Category", crime_types, "Aggravated Assaults"),
                                      selectInput("years", "Years", year_values, 2017),
                                      # sliderInput("years", "Years", 2010, 2017, value = c(2017)), # one year for now
                                      
                                      plotOutput("lineChart", height = 200),
                                      plotOutput("weekdayChart", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Source: City of Houston')
                    )
           ),
           
           tabPanel("Data",
                    fluidRow(tags$p("Not all data in the dataset is mapable. There are nearly {this_many}
                                    records left out the map.")
                    )),
           
           conditionalPanel("false", icon("crosshair"))
)
