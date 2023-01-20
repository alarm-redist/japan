###############################################################################
# Download and prepare data for `11_saitama` analysis
# © ALARM Project, May 2022
###############################################################################

suppressMessages({
    library(dplyr)
    library(readr)
    library(sf)
    library(redist)
    library(geomander)
    library(cli)
    library(here)
    library(tidyverse)
    library(nngeo)
    devtools::load_all() # load utilities
})

# Pull functions
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
setwd("..")

# TODO: Define parameters for simulation
sim_type <- "smc"
nsims <- 12500 # Set so that the number of valid plans > 5,000
pref_code <- 11
pref_name <- "saitama"
lakes_removed <- c()
ndists_new <- 16
ndists_old <- 15
sq_max_to_min <- 1.442
sq_max_to_tottori2 <- 2.010
sq_mun_splits <- 8
sq_gun_splits <- 1
sq_koiki_splits <- 0
pop_tol <- 0.20

# Code of municipalities that are split under the newly enacted plan
mun_not_freeze <- c(11203)

# Code of 郡 that are split under the status quo
gun_exception <- c(11320) # Iruma (11324, 11326, 11327)

# Change time limit
options(timeout = 300)

# Download and clean 2020 Census shapefile
pref_shp_2020 <- download_shp(pref_code)
pref_shp_cleaned <- pref_shp_2020 %>%
    clean_jcdf()
# Note that S_NAME shows the name of the first entry of the areas grouped
# in the same KIHON-1 unit (i.e. disregard --丁目,字--)

# remove lake if needed
"ifelse(is.null(lakes_removed),
       pref_shp_cleaned <- pref_shp_cleaned,
       pref_shp_cleaned <- remove_lake(pref_shp_cleaned, lakes_removed))"

####2. Urban Prefectures########
# Municipalities that are not split under the newly enacted plan
pref_freeze <- pref_shp_cleaned %>%
  dplyr::filter(code %in% mun_not_freeze == FALSE) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry),
                   mun_name = dplyr::first(CITY_NAME),
                   pop = sum(JINKO))

# Finalize pref object
pref <- bind_rows(
  pref_shp_cleaned %>%
    dplyr::filter(code %in% mun_not_freeze) %>%
    dplyr::rename(sub_code = KIHON1,
                  mun_name = CITY_NAME,
                  sub_name = S_NAME,
                  pop = JINKO),
  pref_freeze) %>%
  arrange(code, sub_code) %>%
  sf::st_as_sf()

### Create the shapefile for the status quo
# status quo
sq_pref <- status_quo_match(pref_shp_cleaned, pref_code)
sq_pref <- sf::st_transform(sq_pref, crs = sf::st_crs(4612)) %>%
  dplyr::rename(sub_code = KIHON1,
                mun_name = CITY_NAME,
                sub_name = S_NAME,
                pop = JINKO,
                dist = ku)
