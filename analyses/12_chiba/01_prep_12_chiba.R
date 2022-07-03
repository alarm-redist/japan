###############################################################################
# Download and prepare data for `12_chiba` analysis
# © ALARM Project, June 2022
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
pref_code <- 12
pref_name <- "chiba"
lakes_removed <- c()
ndists_new <- 14
ndists_old <- 13
sq_max_to_min <-  1.392
sq_max_to_tottori2 <- 1.994
sq_mun_splits <- 5
sq_gun_splits <- 1
sq_koiki_splits <- 0
pop_tol <- 0.20

# Code of minicipalities that are split under the status quo
mun_not_freeze <- c(12203,
                    12204,
                    12207,
                    12217,
                    12410)

# Code of 郡 that are split under the status quo
gun_exception <- c(12400 # Sanbu  (12403, 12409, 12410)
                   )

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
# Assign 松戸市南花島向町 and 南花島中町 into 南花島
pref_mutual[pref_mutual$code == "122071290",]$pop <-
    pref_mutual[pref_mutual$code == "122071290",]$pop +
    pref_pop_only[pref_pop_only$code == "122071640",]$pop +
    pref_pop_only[pref_pop_only$code == "122071650",]$pop

# Assign 柏市 柏インター東 and 柏インター南 in to 大青田, and merge four blocks (大青田 船戸 新十余二 ⻘⽥新⽥⾶地) together.
# This is because 柏市 柏インター東 and 柏インター南 was newly created in 2016 and 2018  by combining parts of four blocks.
pref_mutual[pref_mutual$code == "122170140",]$pop <-
    pref_mutual[pref_mutual$code == "122170140",]$pop +
    pref_pop_only[pref_pop_only$code == "122171360",]$pop +
    pref_pop_only[pref_pop_only$code == "122171370",]$pop
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
    dplyr::filter(code %in% c("122170140",
                              "122170870",
                              "122170420",
                              "122170010")) %>%
    dplyr::summarise(code = first(code),
                     mun_code.x = first(mun_code.x),
                     sub_code = first(sub_code),
                     sub_name = "combined",
                     pop = sum(pop),
                     CITY_NAME = first(CITY_NAME),
                     S_NAME = first(S_NAME),
                     KIHON1 = first(KIHON1),
                     JINKO = sum(JINKO),
                     geometry = sf::st_union(geometry))
# Data frame expect those four blocks
pref_mutual_without_new_address <- pref_mutual %>%
    dplyr::filter(!code %in% c("122170140",
                               "122170870",
                               "122170420",
                               "122170010"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
    dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 柏市染井入新田 to 鷲野谷
pref_mutual[pref_mutual$code == "122171180",]$pop <-
    pref_mutual[pref_mutual$code == "122171180",]$pop +
    pref_pop_only[pref_pop_only$code == "122171380",]$pop

# Assign remaining `pref_geom` into `pref_mutual`
# Assign 松戸市 八ケ崎 of `pref_geom_only` into 松戸市 八ケ崎 of `pref_mutual`
geom_only_code <- c("122071072")
pref_geom_only_1 <- pref_geom_only %>%
    filter(code %in% geom_only_code) %>%
    group_by(mun_code) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 72
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122071071",]$geometry <-
    sf::st_union(filter(pref_mutual, code == "122071071")$geometry,
                 filter(pref_geom_only_1, code == "122071072")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
    filter(code %in% geom_only_code == FALSE)

# Assign 柏市 柏１丁目 of `pref_geom_only` into 柏市 柏 of `pref_mutual`
geom_only_code <- c("122170190")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 190
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122170180",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122170180")$geometry,
               filter(pref_geom_only_1, code == "122170190")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 柏市 酒井根 of `pref_geom_only` into 柏市 酒井根 of `pref_mutual`
geom_only_code <- c("122170312")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 312
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122170311",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122170311")$geometry,
               filter(pref_geom_only_1, code == "122170312")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 柏市 逆井 of `pref_geom_only` into 柏市 逆井 of `pref_mutual`
geom_only_code <- c("122170322")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 322
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122171100",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122171100")$geometry,
               filter(pref_geom_only_1, code == "122170322")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 柏市 中原 of `pref_geom_only` into 柏市 中原 of `pref_mutual`
geom_only_code <- c("122170620")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 620
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122170630",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122170630")$geometry,
               filter(pref_geom_only_1, code == "122170620")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 柏市 名戸ケ谷 of `pref_geom_only` into 柏市 名戸ケ谷 of `pref_mutual`
geom_only_code <- c("122170650")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 650
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122170640",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122170640")$geometry,
               filter(pref_geom_only_1, code == "122170650")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 柏市 藤心 of `pref_geom_only` into 柏市 藤心 of `pref_mutual`
geom_only_code <- c("122170830")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 830
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122171080",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122171080")$geometry,
               filter(pref_geom_only_1, code == "122170830")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 柏市 増尾 of `pref_geom_only` into 柏市 増尾 of `pref_mutual`
geom_only_code <- c("122170900")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 900
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122171070",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122171070")$geometry,
               filter(pref_geom_only_1, code == "122170900")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 柏市 南増尾 of `pref_geom_only` into 柏市 南増尾 of `pref_mutual`
geom_only_code <- c("122170983")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 983
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122170982",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122170982")$geometry,
               filter(pref_geom_only_1, code == "122170983")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

#############################
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
sum(pref$pop)
