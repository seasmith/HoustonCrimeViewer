
# HEADER ------------------------------------------------------------------

# Header
header <- dashboardHeader(
  title = "Houston Crime Viewer"
)


# SIDEBAR -----------------------------------------------------------------

# Define 'Crime Category' inputs
defaultCrimeType <- "Aggravated Assault"
crimeTypes <- c(
  "Aggravated Assaults" = "Aggravated Assault",
  "Auto Thefts"         = "Auto Theft",
  "Burglaries"          = "Burglary",
  "Murders"             = "Murder",
  "Other Thefts"        = "Other Theft",
  "Rapes"               = "Rape",
  "Robberies"           = "Robbery"
)

# Define 'Year' inputs
defaultYearValue <- 2019
yearValue <- as.character(2010:2020) %>% setNames(nm = .)

# Set variables for use through out the ui
mapText <- "Crime Map"
databriefText <- "Data Brief"
mapTabName <- "map"
databriefTabName <- "databrief"

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(mapText, tabName = mapTabName),
    # menuItem(databriefText, tabName = databriefTabName),
    selectInput("offType", "Crime Category", crimeTypes, defaultCrimeType),
    selectInput("years", "Year", yearValue, defaultYearValue),
    actionButton("resetBeatSelection", "Reset Map Selection")
  )
)


# BODY --------------------------------------------------------------------

# Body
body <- dashboardBody(
  tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
  tabItems(
    tabItem(
      tabName = mapTabName,
      title = "Crime Map",
      fluidRow(
        box(
          width = 7,
          leafletOutput("map", width = "100%", height = "100%"),
          tags$div(id="cite",
                   "Data sources: City of Houston; NASA SEDAC | ",
                   tags$a("Project Homepage", href = "https://github.com/seasmith/HoustonCrimeViewer")
          )
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
    )#,
    # tabItem(
    #   tabName = databriefTabName, h2(databriefText)
    # )
  )
)


# UI ----------------------------------------------------------------------

# ui
ui <- dashboardPage(header, sidebar, body, skin = "purple")
