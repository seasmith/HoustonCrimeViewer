
# HEADER ------------------------------------------------------------------

# Header
header <- dashboardHeader(
  title = "Houston Crime Viewer"
)


# SIDEBAR -----------------------------------------------------------------

# Define 'Crime Category' inputs
defaultCrimeType <- "Aggravated Assaults"
crimeTypes <- c(
  "Aggravated Assaults" = "Aggravated Assaults",
  "Auto Thefts"         = "Auto Thefts",
  "Burglaries"          = "Burglaries",
  "Murders"             = "Murders",
  "Other Thefts"        = "Other Thefts",
  "Rapes"               = "Rapes",
  "Robberies"           = "Robberies"
)

# Define 'Year' inputs
defaultYearValue <- 2017
yearValue <- as.character(2010:2017) %>% setNames(nm = .)

# Set variables for use through out the ui
mapText <- "Crime Map"
databriefText <- "Data Brief"
mapTabName <- "map"
databriefTabName <- "databrief"

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(mapText, tabName = mapTabName),
    menuItem(databriefText, tabName = databriefTabName),
    selectInput("offType", "Crime Category", crimeTypes, defaultCrimeType),
    selectInput("years", "Year", yearValue, defaultYearValue),
    actionButton("resetBeatSelection", "Reset Map Selection")
  )
)


# BODY --------------------------------------------------------------------

# Set row height
mapRowHeight  <- "750px"

# Body
body <- dashboardBody(
  tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
  tabItems(
    tabItem(
      tabName = mapTabName,
      title = "Crime Map",
      fluidRow(
        box(
          leafletOutput("map", width = "100%", height = "100%"), width = 7
        ),
        box(
          width = 5, height = "100%",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "All Crimes",
              plotlyOutput("yearly_trends", height = 400),
              plotlyOutput("density", height = 400)
              
            ),
            tabPanel(
              "Selected Crime",
              plotlyOutput("day_and_hour_trends", height = 400)    
            )
          )
        )
      )
    ),
    tabItem(
      tabName = databriefTabName, h2(databriefText)
    )
  )
)


# UI ----------------------------------------------------------------------

# ui
ui <- dashboardPage(header, sidebar, body, skin = "purple")
