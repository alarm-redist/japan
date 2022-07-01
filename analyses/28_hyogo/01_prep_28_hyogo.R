###############################################################################
# Download and prepare data for `28_hyogo` analysis
# © ALARM Project, July 2022
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
nsims <- 10000 # Set so that the number of valid plans > 5,000
pref_code <- 28
pref_name <- "hyogo"
lakes_removed <- c()
ndists_new <- 12
ndists_old <- 12
sq_max_to_min <- 1.644
sq_max_to_tottori2 <- 1.970
sq_mun_splits <- 3
sq_gun_splits <- 0
sq_koiki_splits <- 1
pop_tol <- 0.25

# Codes of municipalities that are split under the status quo
mun_not_freeze <- c(28201, 28204, 28217)

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

####2. Urban Prefectures########
# Clean 2020 Census data at the 小地域-level
pref_pop_2020 <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE)

# Match 2015 shapefile (`pref_shp_cleaned`) with 2020 Census data (`pref_pop_2020`)
# Combine municipality code with sub-code
pref_shp_cleaned <- mutate(pref_shp_cleaned, mun_code = code, code = str_c(code, KIHON1))
# Combine municipality code with sub-code
pref_pop_2020 <- mutate(pref_pop_2020, code = str_c(mun_code, str_pad(sub_code, 4, pad = "0")))

### Municipalities that not split under the status quo ###
# Match at the municipality level
pop <- pref_pop_2020 %>%
  dplyr::filter(mun_code %in% mun_not_freeze == FALSE) %>%
  dplyr::group_by(mun_code) %>%
  dplyr::summarise(pop = sum(pop)) %>%
  dplyr::rename(code = mun_code)

geom <- pref_shp_cleaned %>%
  dplyr::filter(mun_code %in% mun_not_freeze == FALSE) %>%
  dplyr::group_by(mun_code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::select(mun_code, geometry) %>%
  dplyr::rename(code = mun_code)

# Combine data frames
pref_freeze <- merge(pop, geom, by = "code")

### Municipalities that split under the status quo ###
# Match at the 小地域 level
# TODO Match areas that do not exist in either `pref_shp_cleaned` or `pref_pop_2020`
# 1. Areas that are accounted for in both data frames
pref_mutual <- merge(dplyr::filter(pref_pop_2020, mun_code %in% mun_not_freeze),
                     dplyr::filter(pref_shp_cleaned, mun_code %in% mun_not_freeze),
                     by = "code")

# 2. Areas that exist only in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only <- merge(dplyr::filter(pref_shp_cleaned, mun_code %in% mun_not_freeze),
                        dplyr::filter(pref_pop_2020, mun_code %in% mun_not_freeze),
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

# Treat 西宮市山口町下山口 as one unit
# Merge together 西宮市山口町下山口(sub_code: 6020 and 6021)
# Then assign the population of 西宮市山口下山口(sub_code: 6021 and 6022)
pref_mutual[pref_mutual$code == "282046021",]$geometry <-
  sf::st_union(pref_mutual[pref_mutual$code == "282046021",]$geometry,
               pref_geom_only[pref_geom_only$code == "282046020",]$geometry)

pref_mutual[pref_mutual$code == "282046021",]$pop <-
  pref_mutual[pref_mutual$code == "282046021",]$pop +
  pref_pop_only[pref_pop_only$code == "282046022",]$pop

# Assign the population of 西宮市山口町中野(sub_code: 6041, 6042) to 山口町中野(sub_code: 6040)
pref_geom_only_1 <- filter(pref_geom_only, code == "282046040")
pref_geom_only_1$pop <-
  pref_pop_only[pref_pop_only$code == "282046041",]$pop +
  pref_pop_only[pref_pop_only$code == "282046042",]$pop
pref_geom_only_1$sub_code <- 6040

# Merge 西宮市神呪字中谷(sub_code: 4580) with 西宮市上甲東園(sub_code: 4220)
pref_mutual[pref_mutual$code == "282044220",]$geometry <-
  sf::st_union(pref_mutual[pref_mutual$code == "282044220",]$geometry,
               pref_geom_only[pref_geom_only$code == "282044580",]$geometry)

# Treat 西宮市山口町金仙寺 as one unit
# Merge together 西宮市山口町金仙寺(sub_code: 6060, 6061, and 6062)
# Then assign the population of 西宮市山口町金仙寺(sub_code: 6061 and 6062)

pref_mutual[pref_mutual$code == "282046061",]$geometry <-
  sf::st_make_valid(
    sf::st_union(sf::st_union(pref_mutual[pref_mutual$code == "282046061",]$geometry,
                              pref_mutual[pref_mutual$code == "282046062",]$geometry),
                 pref_geom_only[pref_geom_only$code == "282046060",]$geometry)
  )

pref_mutual[pref_mutual$code == "282046061",]$pop <-
  pref_mutual[pref_mutual$code == "282046061",]$pop +
  pref_mutual[pref_mutual$code == "282046062",]$pop

pref_mutual <- pref_mutual[-which(pref_mutual$code == "282046062"),] # remove duplicated row

# Finalize pref object
pref <- bind_rows(
  pref_mutual %>%
    select(mun_code.x, sub_code, pop, geometry) %>%
    rename(code = mun_code.x) %>%
    mutate(code = as.numeric(code)),

  pref_freeze,

  pref_geom_only_1 %>%
    select(mun_code, sub_code, pop) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code))) %>%

  arrange(code, sub_code) %>%
  sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
