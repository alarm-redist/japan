###############################################################################
# Download and prepare data for `30_wakayama` analysis
# © ALARM Project, February 2022
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
pref_code <- 30
pref_name <- "wakayama"
lakes_removed <- c()
ndists_new <- 2
ndists_old <- 3
sq_max_to_min <- 1.273
sq_max_to_tottori2 <- 1.291
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

# clean census data
pref_cleaned <- pref_raw %>%
    clean_jcdf()

# remove lake if needed
ifelse(is.null(lakes_removed),
       pref_cleaned <- pref_cleaned,
       pref_cleaned <- remove_lake(pref_cleaned, lakes_removed))

# create sf_data frame
sq_pref <- status_quo_match(pref_cleaned, pref_code)
sq_pref <- sf::st_transform(sq_pref , crs = sf::st_crs(4612)) %>%
    dplyr::group_by(ku) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))

