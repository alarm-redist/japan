###############################################################################
# Download and prepare data for `13_tokyo` analysis
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
nsims_special_wards <- 10000 # Set so that the number of valid plans > 5,000
nsims_tama <- 60000
pref_code <- 13
pref_name <- "tokyo"
lakes_removed <- c()
ndists_new <- 30
ndists_old <- 25
sq_max_to_min <- 1.190
sq_max_to_tottori2 <- 2.096
sq_mun_splits <- 17
sq_gun_splits <- 0
sq_koiki_splits <- 0
pop_tol_special_wards <- 0.06
pop_tol_tama <- 0.08
  # Population tolerance is set separately for the 23 special wards area and Tama area

# Codes of municipalities that are split under the status quo
mun_not_freeze <- c(13103, 13104, 13106, 13109, 13110, 13111, 13112, 13114, 13115,
                    13116, 13119, 13120, 13121, 13123,
                    13201, 13224, 13225)

# Code of 郡 that are split under the status quo
gun_exception <- c()

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

# obtain status quo map for special wards area and Tama area
sq_special_wards <- sf::st_transform(sq_pref , crs = sf::st_crs(4612)) %>%
  filter(ku <= 17, code <= 13123) %>% # filter out special wards and drop islands
  dplyr::group_by(ku) %>%
  dplyr::summarise(geometry = sf::st_union(geometry))
sq_tama <- sf::st_transform(sq_pref , crs = sf::st_crs(4612)) %>%
  filter(ku >= 18) %>%
  dplyr::group_by(ku) %>%
  dplyr::summarise(geometry = sf::st_union(geometry))

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
# There are no areas that fall under this category

# 3. Areas that exist only in 2020 Census data (`pref_pop_2020`)
pref_pop_only <- merge(dplyr::filter(pref_pop_2020, mun_code %in% mun_not_freeze),
                       dplyr::filter(pref_shp_cleaned, mun_code %in% mun_not_freeze),
                       by = "code", all.x = TRUE)
pref_pop_only <- setdiff(pref_pop_only, pref_mutual) %>%
  filter(pop > 0)

# Match or combine areas so that each area in `pref_pop_only` is matched with an existing area
# Assign 港区(sub_code: 310) to 港区台場
pref_mutual[pref_mutual$code == "131030300",]$pop <- # 港区台場
  pref_mutual[pref_mutual$code == "131030300",]$pop + # 港区台場
  pref_pop_only[pref_pop_only$code == "131030310",]$pop # 港区(sub_code: 310)

# Assign 新宿区四谷(sub_code: 11, 12) to 新宿区四谷(sub_code: 10)
pref_mutual[pref_mutual$code == "131040010",]$pop <- # 新宿区四谷(sub_code: 10)
    pref_mutual[pref_mutual$code == "131040010",]$pop + # 新宿区四谷(sub_code: 10)
    pref_pop_only[pref_pop_only$code == "131040011",]$pop + # 新宿区四谷(sub_code: 11)
    pref_pop_only[pref_pop_only$code == "131040012",]$pop # 新宿区四谷(sub_code: 12)

# Finalize pref object
pref <- bind_rows(
  pref_mutual %>%
    select(mun_code.x, sub_code, pop, geometry) %>%
    rename(code = mun_code.x) %>%
    mutate(code = as.numeric(code)),

  pref_freeze

) %>%
  arrange(code, sub_code) %>%
  sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
