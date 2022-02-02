###############################################################################
# Download and prepare data for `[TODO]` analysis
# © ALARM Project, November 2021
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

set.seed(12345)

# Pull functions
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
setwd("..")

# TODO: Define parameters for simulation
sim_type <- "smc"
nsims <- 25000
pref_code <- 0
pref_name <- ""
lakes_removed <- c()
ndists_new <- 0
ndists_old <- 0
sq_max_to_min <- 1
sq_max_to_tottori2 <- 1
sq_mun_splits <- 0
sq_gun_splits <- 0
sq_koiki_splits <- 0

# Code of 郡 that are split under the status quo
gun_exception <- c()

# Change time limit
options(timeout = 300)

# Download 2015 Census shapefile
pref_raw <- download_shp(pref_code)
# Download 2015 population data: Only for urban prefectures
dem_pops <- download_pop_demographics(pref_code)

# Download and clean 2020 census data
census2020 <- clean_2020_census(pref_code)

# Download data from old boundaries (pre-平成の大合併): Only for rural prefectures
old_boundary <- download_old_shp(pref_code)

# Make a status quo matched data
sq_pref <- status_quo_match(pref_raw = pref_raw)
sq_pref <- sf::st_transform(sq_pref , crs = sf::st_crs(4612)) %>%
    dplyr::group_by(ku) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))
# remove lake if needed
ifelse(is.null(lakes_removed),
       sq_pref <- sq_pref,
       sq_pref <- remove_lake(sq_pref, lakes_removed))
