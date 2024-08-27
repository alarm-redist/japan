###############################################################################
# Download and prepare data for `40_fukuoka` analysis
# © ALARM Project, May 2023
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
# nsims: Set so that the number of valid plans > 5,000
nsims <- 15000
pref_code <- 40
pref_name <- "fukuoka"
lakes_removed <- c()
ndists_new <- 11
ndists_old <- 11
pop_tol <- 0.15
lh_old_max_to_min <- 1.871
lh_old_mun_split <- 2
lh_old_gun_split <- 0
lh_old_koiki_split <- 1
lh_2022_max_to_min <- 1.836
lh_2022_mun_split <- 3
lh_2022_gun_split <- 0
lh_2022_koiki_split <- 1

# Split the municipalities that are split under the status quo
split_code <- c(40134, 40136)
# 福岡市南区、城南区

# Municipalities that are split under the newly enacted plan
split_code_lh_2022 <- c(40134, 40136, 40131)
# 福岡市南区、城南区、東区

# Code of 郡 that are split under the status quo
gun_exception <- c()

# Change time limit
options(timeout = 300)

# Download Census shapefile
pref_shp_2020 <- download_shp(pref_code)

# Clean Census shapefile
pref_shp_cleaned <- pref_shp_2020 %>% # Fixed error in the shapefile
  clean_jcdf()
# Note that S_NAME shows the name of the first entry of the areas grouped
# in the same KIHON-1 unit (i.e. disregard --丁目,字--)

# Download 2020 Census data at 小地域-level (size of Japanese population)
pref_pop_2020 <- download_pop_2020(pref_code)

# Download 2019 House of Councillors election data (Proportional Representation)
pref_2019_HoC_PR <- download_2019_HoC_PR(pref_code)

# Download 2022 House of Councillors election data (Proportional Representation)
pref_2022_HoC_PR <- download_2022_HoC_PR(pref_code)

####2. Urban Prefectures########

# Clean 2020 Census data at the 小地域-level
pref_pop_cleaned <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE) %>%
  rename(code = mun_code)

# Clean 2019 House of Councillors election data
pref_2019_HoC_PR_cleaned <- clean_pref_2019_HoC_PR(pref_2019_HoC_PR)

# Clean 2022 House of Councillors election data
pref_2022_HoC_PR_cleaned <- clean_pref_2022_HoC_PR(pref_2022_HoC_PR)

# Estimate baseline votes
pref_HoC_PR <- clean_pref_HoC_PR(pref_2019_HoC_PR_cleaned, pref_2022_HoC_PR_cleaned)

# Match `pref_shp_cleaned` with `pref_pop_cleaned`
pref_join <- pref_shp_cleaned %>%
  dplyr::mutate(sub_code = as.numeric(KIHON1)) %>%
  dplyr::left_join(pref_pop_cleaned, by = c("code", "sub_code")) %>%
  dplyr::select(code, mun_name, sub_code, sub_name, pop, geometry)

# Freeze municipalities except for `split_code` and `split_code_lh_2022`
# Calculate the baseline votes per municipality
pref_mun <- dplyr::bind_rows(
  # Municipalities without splits
  pref_join %>%
    dplyr::filter(code %in% c(split_code, split_code_lh_2022) == FALSE) %>%
    dplyr::group_by(code, mun_name) %>%
    dplyr::summarise(sub_code = first(sub_code),
                     sub_name = "-",
                     pop = sum(pop),
                     geometry = sf::st_union(geometry)) %>%
    dplyr::left_join(pref_HoC_PR, by = "mun_name"),
  # Municipalities with splits
  pref_join %>%
    dplyr::filter(code %in% c(split_code, split_code_lh_2022)) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(pop_ratio = pop / sum(pop)) %>%
    dplyr::left_join(pref_HoC_PR, by = "mun_name") %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("nv"), ~ .x * pop_ratio)) %>%
    dplyr::select(-pop_ratio)
)

# Confirm that the population figure matches that of the redistricting committee
sum(pref_mun$pop)
sum(pref_mun$nv_ldp)
sum(pref_HoC_PR$nv_ldp)
