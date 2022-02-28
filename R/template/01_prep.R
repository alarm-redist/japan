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
pref_shp_2015 <- download_shp(pref_code)
# Clean 2015 Census shapefile
pref_shp_cleaned <- pref_shp_2015 %>%
    clean_jcdf()

# Download 2020 Census data at 小地域-level
pref_pop_2020 <- download_pop_2020(pref_code)

# Clean 2020 Census data at the 小地域-level
pref_pop_2020 <- clean_pref_pop_2020(pref_pop_2020)

# remove lake if needed
ifelse(is.null(lakes_removed),
       pref_shp_cleaned <- pref_shp_cleaned,
       pref_shp_cleaned <- remove_lake(pref_shp_cleaned, lakes_removed))

# Match data and clean
# Combine municipality code with sub-code
pref_shp_cleaned <- pref_shp_cleaned %>%
    mutate(code = str_c(code, KIHON1))
# Combine municipality code with sub-code
pref_pop_2020 <- pref_pop_2020 %>%
    mutate(code = str_c(mun_code, sub_code))
pref <- pref_pop_2020 %>%
    left_join(pref_shp_cleaned, by = "code") %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code)

# Remove rows with empty geometry and 0 population
pref <- pref[!(sf::st_is_empty(pref$geometry) & pref$pop == 0),]
# Convert to sf object
pref <- sf::st_as_sf(pref)

## Only for rural prefectures: data needed to calculate pop. by old municipality
# Download and clean 2020 census data
census_mun_old_2020 <- clean_2020_census(pref_code)
# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition (total population - foreign population)

# Download data from old boundaries (pre-平成の大合併)
old_boundary <- download_old_shp(pref_code)

# create sf_data frame
sq_pref <- status_quo_match(pref_shp_cleaned, pref_code)
sq_pref <- sf::st_transform(sq_pref , crs = sf::st_crs(4612)) %>%
    dplyr::group_by(ku) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))

