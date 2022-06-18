###############################################################################
# Download and prepare data for `00_pref` analysis
# © ALARM Project, April 2021
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
nsims <- 25000 # Set so that the number of valid plans > 5,000
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
pop_tol <- 0.10

# Code of minicipalities that are split under the status quo
mun_not_freeze <- c()
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

# remove lake if needed
"ifelse(is.null(lakes_removed),
       pref_shp_cleaned <- pref_shp_cleaned,
       pref_shp_cleaned <- remove_lake(pref_shp_cleaned, lakes_removed))"

# status quo
sq_pref <- status_quo_match(pref_shp_cleaned, pref_code)
sq_pref <- sf::st_transform(sq_pref , crs = sf::st_crs(4612)) %>%
    dplyr::group_by(ku) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))

####1. Rural Prefectures########
# Clean 2020 Census data at the 小地域-level
pref_pop_2020 <- clean_pref_pop_2020(pref_pop_2020)

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

####2. Urban Prefectures########
# Clean 2020 Census data at the 小地域-level
pref_pop_2020 <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE)

# Match 2015 shapefile (`pref_shp_cleaned`) with 2020 Census data (`pref_pop_2020`)
# Combine municipality code with sub-code
pref_shp_cleaned <- mutate(pref_shp_cleaned, mun_code = code, code = str_c(code, KIHON1))
# Combine municipality code with sub-code
pref_pop_2020 <- mutate(pref_pop_2020, code = str_c(mun_code, str_pad(sub_code, 4, pad = "0")))

### Municipalities without splits (freeze) ###
pop <- pref_pop_2020 %>%
  dplyr::filter(mun_code %in% mun_not_freeze == FALSE) %>%
  dplyr::group_by(mun_code) %>%
  dplyr::summarise(pop = sum(pop)) %>%
  dplyr::rename(code = mun_code)

geom <- pref_shp_cleaned %>%
  dplyr::filter(mun_code %in% mun_not_freeze == FALSE) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::select(code, geometry)

# Combine data frames
pref_freeze <- merge(pop, geom, by = "code")

# TODO Match areas that do not exist in either `pref_shp_cleaned` or `pref_pop_2020`
# 1. Areas that are accounted for in both dataframes
pref_mutual <- merge(dplyr::filter(pref_pop_2020, mun_code %in% mun_not_freeze),
                     dplyr::filter(pref_shp_cleaned, mun_code %in% mun_not_freeze),
                     by = "code")

# 2. Areas that exist only in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only <- merge(dplyr::filter(pref_pop_2020, mun_code %in% mun_not_freeze),
                        dplyr::filter(pref_shp_cleaned, mun_code %in% mun_not_freeze),
                        by = "code", all.x = TRUE)
pref_geom_only <- setdiff(pref_geom_only, pref_mutual)

# 3. Areas that exist only in 2020 Census data (`pref_pop_2020`)
pref_pop_only <- merge(dplyr::filter(pref_pop_2020, mun_code %in% mun_not_freeze),
                       dplyr::filter(pref_shp_cleaned, mun_code %in% mun_not_freeze),
                       by = "code", all.x = TRUE)
pref_pop_only <- setdiff(pref_pop_only, pref_mutual) %>%
  filter(pop > 0)

# Match or combine data so that every single census block is taken into account
# Add municipality code, sub_code, sub_name to areas that only exist in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only$pop <- 0
pref_geom_only$mun_code <- substr(pref_geom_only$code, start = 1, stop = 5)
# Match or combine areas so that each area in `pref_pop_only` is matched with an existing area
"Example
# Assign 港区 to 港区台場
pref_mutual[pref_mutual$code == "131030300",]$pop <- # 港区台場
    pref_mutual[pref_mutual$code == "131030300",]$pop + # 港区台場
    pref_pop_only[pref_pop_only$code == "131030310",]$pop

# Assign 新宿区四谷 to 新宿区四谷
pref_mutual[pref_mutual$code == "131040010",]$pop <- # 新宿区四谷
    pref_mutual[pref_mutual$code == "131040010",]$pop + # 新宿区四谷
    pref_pop_only[pref_pop_only$code == "131040011",]$pop +
    pref_pop_only[pref_pop_only$code == "131040012",]$pop

# Assign 昭島市もくせいの杜 to 昭島市福島町
pref_mutual[pref_mutual$code == "132070051",]$pop <- # 昭島市福島町
    pref_mutual[pref_mutual$code == "132070051",]$pop + # 昭島市福島町
    pref_pop_only[pref_pop_only$code == "132070230",]$pop　# 昭島市もくせいの杜

# Assign 町田市南町田(pop: 450) to 町田市鶴間、小川
pref_geom_only_1 <- pref_geom_only %>%
    filter(code %in% c("132090040", "132090200")) %>% # 町田市鶴間, 町田市小川
    group_by(mun_code) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code == "132090040"]$pop <-
    pref_pop_only[pref_pop_only$code == "132090450",]$pop　# 町田市南町田
# Add sub_code
pref_geom_only_1$sub_code = 450

# Group together 町田市木曽西
pref_mutual[pref_mutual$code == "132090112",]$geometry <-
    sf::st_union(filter(pref_mutual, code == "132090112")$geometry,
                 filter(pref_geom_only, code == "132090114")$geometry)"

# Finalize pref object
pref <- bind_rows(
  pref_mutual %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code),
  pref_freeze)  %>%
  sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
