
# HEADER ------------------------------------------------------------------

# Header
header <- dashboardHeader(
  title = "Houston Crime Viewer"
)


# SIDEBAR -----------------------------------------------------------------

# Define 'Crime Category' inputs
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
yearValues <- as.character(2010:2017) %>% setNames(nm = .)

# Set variables for use through out the ui
mapText <- "Crime Map Explorer"
databookText <- "Data Book"
mapTabName   <- "map"
databookTabName <- "databook"

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(mapText, tabName = mapTabName),
    menuItem(databookText, tabName = databookTabName),
    selectInput("offType", "Crime Category", crimeTypes, "Aggravated Assaults"),
    selectInput("years", "Year", yearValues, 2017)
  )
)


# BODY --------------------------------------------------------------------


# Set first row height to 500px...for now
mapRowHeight  <- "750px"
# plotRowHeight <- "300px"  # not using this, for now

# Body
body <- dashboardBody(
  
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  tabItems(
    tabItem(
      tabName = mapTabName, h2(mapText),
      fluidRow(
        box(
          title = "Houston Crime Data Map", width = 8, height = mapRowHeight,
          leafletOutput("map", width = "100%", height = "680px")
        ),
        box(
          title = "Police Beat Report", width = 4, height = mapRowHeight,
          plotOutput("yearly_trends"),
          plotOutput("day_and_hour_trends")
        )#,
        # box(
        #   title = "Filters", width = 4, height = mapRowHeight,
        #   selectInput("offType", "Crime Category", crimeTypes, "Aggravated Assaults"),
        #   selectInput("years", "Year", yearValues, 2017)
        # )
      )#,
      # fluidRow(
      #   box(
      #     title = "Police Beat Report",
      #     plotOutput("yearly_trends"),
      #     plotOutput("day_and_hour_trends")
      #   )
      # )
    ),
    tabItem(
      tabName = databookTabName, h2(databookText)
    )
  )
)

# Experimental concept:
# firstRow <- apply(tst, 1, function(x) set_box(x))  # cannot be used inside `fluidRow()`
# fluidRow(apply(tst, 1, function(x) set_box(x)))
# 
# map_inputs    <- list(leafletOutput("map", width = "100%", height = "100%"))
# select_inputs <- list(selectInput("offType", "Crime Category", crimeTypes, "Aggravated Assaults"),
#                       selectInput("years", "Year", yearValues, 2017))
# plot_outputs  <- list(plotOutput("yearly_trends"),
#                       plotOutput("day_and_hour_trends"))
# 
# tst <- tibble::tribble(~title                  , ~width, ~height, ~input,
#                        "Houston Crime Data Map",      8, "500px", map_inputs,
#                        "Filters"               ,      4, "500px", select_inputs,
#                        "Police Beat Report"    ,     12, "500px", plot_inputs)
# 
# set_box  <- function(r) box(title  = r[["title"]],  width = r[["width"]],
#                             height = r[["height"]], eval(r[["input"]]))
# 
# tabItem(
#   tabName = mapTabName, h2(mapText),
#   fluidRow(apply(tst[1:2, ], 1, function(x) set_box(x))),
#   fluidRow(apply(tst[3,   ], 1, function(x) set_box(x)))
# )

# UI ----------------------------------------------------------------------

# ui
ui <- dashboardPage(header, sidebar, body)
