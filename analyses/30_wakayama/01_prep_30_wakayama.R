###############################################################################
# Download and prepare data for `30_wakayama` analysis
# © ALARM Project, March 2022
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
pref_shp_2015 <- download_shp(pref_code)
# Clean 2015 Census shapefile
pref_shp_cleaned <- pref_shp_2015 %>%
    clean_jcdf()
# Note that S_NAME shows the name of the first entry of the areas grouped
# in the same KIHON-1 unit (i.e. disregard --丁目,字--)

# Download 2020 Census data at 小地域-level
pref_pop_2020 <- download_pop_2020(pref_code)

# Clean 2020 Census data at the 小地域-level
pref_pop_2020 <- clean_pref_pop_2020(pref_pop_2020)

# remove lake if needed
#ifelse(is.null(lakes_removed),
#       pref_shp_cleaned <- pref_shp_cleaned,
#       pref_shp_cleaned <- remove_lake(pref_shp_cleaned, lakes_removed))

# status quo
sq_pref <- status_quo_match(pref_shp_cleaned, pref_code)
sq_pref <- sf::st_transform(sq_pref , crs = sf::st_crs(4612)) %>%
    dplyr::group_by(ku) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))

####1. Rural Prefectures########
# Download and clean 2020 census data at municipality/old-munipality-level
census_mun_old_2020 <- clean_2020_census(pref_code)
# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition (total population - foreign population)

# Download data from old boundaries (pre-平成の大合併)
old_mun <- download_old_shp(pref_code)

# custom data for the analysis
pop <- pref_pop_2020 %>%
    dplyr::group_by(mun_code) %>%
    dplyr::summarise(pop = sum(pop)) %>%
    dplyr::rename(code = mun_code)

geom <- pref_shp_cleaned %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    dplyr::select(code, geometry)

# Combine data frames
pref <- merge(pop, geom, by = "code")
pref <- sf::st_as_sf(pref)

# Confirm that the population figure matches that of the redistricting committee
sum(pref$pop)
