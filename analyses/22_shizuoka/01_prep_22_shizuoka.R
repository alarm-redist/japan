###############################################################################
# Download and prepare data for `22_shizuoka` analysis
# © ALARM Project, March 2023
###############################################################################

# Set up packages
library(redist)
library(geomander)
library(sf)
library(tidyverse)
library(here)

# Pull functions
setwd(here("function"))
files.sources <- list.files()
sapply(files.sources, source)
rm(files.sources)
setwd(here())

# TODO: Define parameters for simulation
sim_type <- "smc"
nsims <- 10000 # Set so that the number of valid plans > 5,000
pref_code <- 22
pref_name <- "shizuoka"
lakes_removed <- c("浜名湖")
ndists_new <- 8
ndists_old <- 8
pop_tol <- 0.15
lh_old_max_to_min <- 1.453
lh_old_mun_split <- 9
lh_old_gun_split <- 1
lh_old_koiki_split <- 1
lh_2022_max_to_min <- 1.415
lh_2022_mun_split <- 1
lh_2022_gun_split <- 1 # 駿東郡
lh_2022_koiki_split <- 1

# Split the municipalities that are split under the status quo
split_code <- c(22210, 22223, 22225)
# 富士市、御前崎市、伊豆の国市

# The following wards are split under the status quo.
# 静岡市葵区(22101), 静岡市駿河区(22102), 静岡市清水区(22103),
# 浜松市中区(22131), 浜松市南区(22134), 浜松市天竜区(22137)
# However, they will not be split in the simulation because the boundaries of the
# "old municipalities" do not necessarily match the boundaries of the wards that
# belong to Shizuoka/Hamamatsu City.

# Code of 郡 that are split under the status quo
gun_exception <- c(22340) # 駿東郡

# Change time limit
options(timeout = 300)

# Download Census shapefile
pref_shp_2020 <- download_shp(pref_code)

# Clean Census shapefile
pref_shp_cleaned <- pref_shp_2020 %>%
  clean_jcdf() %>%
  # remove 浜名湖
  remove_lake(lakes_removed)
# Note that S_NAME shows the name of the first entry of the areas grouped
# in the same KIHON-1 unit (i.e. disregard --丁目,字--)

# Download 2020 Census data at 小地域-level (size of Japanese population)
pref_pop_2020 <- download_pop_2020(pref_code)

# Download 2019 House of Councillors election data (Proportional Representation)
pref_2019_HoC_PR <- download_2019_HoC_PR(pref_code)

# Download 2022 House of Councillors election data (Proportional Representation)
pref_2022_HoC_PR <- download_2022_HoC_PR(pref_code)

####1. Rural Prefectures########

# Clean 2020 Census data
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020)

# Download and clean 2020 census data at municipality/old-munipality-level
census_mun_old_2020 <- clean_2020_census(pref_code)
# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition by the redistricting committee.
# (Japanese population) = (total population) - (foreign population)

# Clean 2019 House of Councillors election data
pref_2019_HoC_PR_cleaned <- clean_pref_2019_HoC_PR(pref_2019_HoC_PR)

# Clean 2022 House of Councillors election data
pref_2022_HoC_PR_cleaned <- clean_pref_2022_HoC_PR(pref_2022_HoC_PR)

# Estimate baseline votes
pref_HoC_PR <- clean_pref_HoC_PR(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned)

# Download data from old boundaries (pre-平成の大合併)
old_mun <- download_old_shp(pref_code)

# custom data for the analysis
pop <- pref_pop_cleaned %>%
  dplyr::rename(code = mun_code)

geom <- pref_shp_cleaned %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::select(code, geometry)

# Combine data frames
pref_mun <- full_join(pop, geom, by = "code") %>%
  full_join(pref_HoC_PR, by = "mun_name")
pref_mun <- sf::st_as_sf(pref_mun)

# Confirm that the population figure matches that of the redistricting committee
sum(pref_mun$pop)
