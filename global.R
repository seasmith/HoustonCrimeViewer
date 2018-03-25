
# DEPENDENCIES AND FUNCTIONS ----------------------------------------------

library(leaflet)
library(sf)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
pretty <- function(x) prettyNum(x, big.mark = ",")


# LOAD DATA ---------------------------------------------------------------

# Houston population data
load("../../../../data/hou_pop.RData")
# Sumarized (yearly) crime data
load("../../../../data/hpb_yearly.RData")
# Houston police beats
load("../../../../data/map_pol_beat_simp.RData")


# ASSEMBLE DATA -----------------------------------------------------------


# REMOVING POLICE BEATS ---------------------------------------------------


rm_beats <- function(threshold, measure = c("population", "density")) {
  
  if (measure[1] == "density") {
    
    filter(hou_pop,
           UN_2010_DS < !!enquo(threshold),
           UN_2017_DS < !!enquo(threshold)) %>%
      pull(Beats) %>%
      as.character()
    
  } else {
    
    filter(hou_pop,
           UN_2010_E < !!enquo(threshold),
           UN_2017_E < !!enquo(threshold)) %>%
      pull(Beats) %>%
      as.character()
    
  }
}

# This beat is next to Hobby and skews the 'Auto Thefts': 13D30

rm_hobby <- "13D30"

hpb_yearly <- hpb_yearly  %>%
  mutate(rate = if_else(Beat %in% c(rm_beats(100), rm_hobby), NA_real_, rate))
