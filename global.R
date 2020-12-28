
# DEPENDENCIES AND FUNCTIONS ----------------------------------------------

library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(sf)
library(dplyr)
library(scales)
library(forcats)
library(data.table)
pretty <- function(x) prettyNum(x, big.mark = ",")
x_labs <- c(paste0(c(12, 1:11), "am"),
            paste0(c(12, 1:11), "pm"))

# LOAD DATA ---------------------------------------------------------------

# Houston population data
load("data/hou_pop.RData")
# Beat sumarized (yearly) crime data
load("data/hpb_yearly.RData")
# Whole city summarize (yearly) crime data
load("data/hpb_yearly_summed.RData")
# Hourly beat data (unsummarize)
load("data/hpb_hourly.RData")
# Hourly beat data (summarize)
load("data/hpb_hourly_summed.RData")
# Houston police beats
load("data/map_pol_beat_simp.RData")


# ASSEMBLE DATA -----------------------------------------------------------

# For plotly, an example:
# hpb_yearly %>%
#   filter(year == 2017 &
#            `Offense Type` == "Murders") %>%
#   split(.$Beat) %>%
#   {
#     purrr::map(., st_coordinates) %>%
#       purrr::map(tibble::as_tibble) %>%
#       dplyr::bind_rows(.id = "Beat")
#   }

# REMOVING POLICE BEATS ---------------------------------------------------

rm_beats <- function(threshold, measure = c("population", "density")) {
  
  if (measure[1] == "density") {
    
    filter(hou_pop,
           UN_2010_DS < !!enquo(threshold),
           UN_2020_DS < !!enquo(threshold)) %>%
      pull(Beats) %>%
      as.character()
    
  } else {
    
    filter(hou_pop,
           UN_2010_E < !!enquo(threshold),
           UN_2020_E < !!enquo(threshold)) %>%
      pull(Beats) %>%
      as.character()
    
  }
}

# This beat is next to Hobby and skews the 'Auto Thefts': 13D30

rm_hobby <- "13D30"
rm_int <- "21I50"

hpb_yearly <- hpb_yearly  %>%
  mutate(rate = if_else(beat %in% c(rm_beats(100), rm_hobby), NA_real_, rate)) %>%
  mutate(rate = if_else(beat %in% c(rm_beats(100), rm_int), NA_real_, rate)) %>%
