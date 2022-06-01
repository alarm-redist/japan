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
nsims <- 25000 # Set so that the number of valid plans > 5,000
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
pop_tol <- 0.10

# Code of 郡 that are split under the status quo
# For Chiba, which has many discontinuity of gun,
# we will set those gun as `gun_exception` to use in the `02_sim` process.
gun_exception <- c(12320, # Inba (12322, 12329)
                   12340, # Katori (12342, 12347, 12349)
                   12400, # Sanbu A (12403, 12409, 12410)
                   12440 # Kodama (12441, 12443)
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
pref_shp_cleaned <- mutate(pref_shp_cleaned, code = str_c(code, KIHON1))
# Combine municipality code with sub-code
pref_pop_2020 <- mutate(pref_pop_2020, code = str_c(mun_code, str_pad(sub_code, 4, pad = "0")))

# TODO Match areas that do not exist in either `pref_shp_cleaned` or `pref_pop_2020`

### Unique Method for Chiba ###
# Since 野田市 (12208) and 四街道市 (12228) changed KIHON1 code and
# assign a new number that duplicates the old number of other 小地域
# between the 2015 and 2020 censuses, we set 野田市 and 四街道市 aside
# 1. Areas that are accounted for in both dataframes
pref_mutual <- merge(pref_pop_2020, pref_shp_cleaned, by = "code") %>%
    dplyr::filter(mun_code %in% c(12208, 12228) == FALSE)
# 2. Areas that exist only in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only <- merge(pref_shp_cleaned, pref_pop_2020, by = "code", all.x = TRUE)
pref_geom_only <- setdiff(pref_geom_only, pref_mutual)

# 3. Areas that exist only in 2020 Census data (`pref_pop_2020`)
pref_pop_only <- merge(pref_pop_2020, pref_shp_cleaned, by = "code", all.x = TRUE)
pref_pop_only <- setdiff(pref_pop_only, pref_mutual) %>%
    filter(pop > 0) %>%
    dplyr::filter(mun_code %in% c(12208, 12228) == FALSE)

# Match or combine data so that every single census block is taken into account
# Add municipality code, sub_code, sub_name to areas that only exist in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only$pop <- 0
pref_geom_only$mun_code <- substr(pref_geom_only$code, start = 1, stop = 5)
pref_geom_only <- pref_geom_only %>%
    dplyr::filter(mun_code %in% c(12208, 12228) == FALSE)
###############################
### Unique Method for Chiba ###
###############################
# Match population data and shape file by S_NAME for 野田市 (12208) and 四街道市 (12228).
# Subset the shape file
pref_noda_yotsukaido_shp <- pref_shp_cleaned %>%
    dplyr::mutate(mun_code = as.numeric(code) %/% 10000) %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::group_by(S_NAME) %>%
    dplyr::summarise(
        mun_code = first(mun_code),
        code = first(code),
        CITY_NAME = first(CITY_NAME),
        S_NAME = first(S_NAME),
        KIHON1 = first(KIHON1),
        JINKO = sum(JINKO),
        geometry = sf::st_union(geometry)) %>%
    dplyr::mutate(sub_name = S_NAME)
# Match by S_NAME (sub_name)
pref_noda_yotsukaido_mutual <- pref_pop_2020 %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::group_by(sub_name) %>%
    dplyr::summarise(
        mun_code = first(mun_code),
        sub_code = first(sub_code),
        sub_name = first(sub_name),
        pop = sum(pop),
        code = first(code)
    ) %>%
    dplyr::inner_join(pref_noda_yotsukaido_shp, by = "sub_name")

pref_noda_yotsukaido_pop_only <- pref_pop_2020 %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::group_by(sub_name) %>%
    dplyr::summarise(
        mun_code = first(mun_code),
        sub_code = first(sub_code),
        sub_name = first(sub_name),
        pop = sum(pop),
        code = first(code)
    ) %>%
    dplyr::left_join(pref_noda_yotsukaido_shp, by = "sub_name") %>%
    dplyr::setdiff(pref_noda_yotsukaido_mutual)

pref_noda_yotsukaido_geom_only <- pref_pop_2020 %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::group_by(sub_name) %>%
    dplyr::summarise(
        mun_code = first(mun_code),
        sub_code = first(sub_code),
        sub_name = first(sub_name),
        pop = sum(pop),
        code = first(code)
    ) %>%
    dplyr::right_join(pref_noda_yotsukaido_shp, by = "sub_name") %>%
    dplyr::setdiff(pref_noda_yotsukaido_mutual)
# Match or combine data so that every single census block is taken into account
# Add municipality code, sub_code, sub_name to areas that only exist in 2015 shapefile (`pref_shp_cleaned`)
pref_noda_yotsukaido_geom_only$pop <- 0

# Match or combine areas so that each area in `pref_pop_only` is matched with an existing area
# Assign 清水公園東 (pop) to 清水公園東一丁目 and 清水公園東二丁目 (geom)
geom_only_code <- c("122080700", "122080710")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122080700"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080080",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 700
pref_geom_only_1$mun_code.y = 12208
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 桜の里 (pop) to 桜の里一丁目 桜の里二丁目 and 桜の里三丁目 (geom)
geom_only_code <- c("122080720", "122080730", "122080740")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122080720"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080090",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 700
pref_geom_only_1$mun_code.y = 12208
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign つつみ野 (pop) to つつみ野一丁目 and つつみ野二丁目 (geom)
geom_only_code <- c("122080750", "122080760")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122080750"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080100",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 700
pref_geom_only_1$mun_code.y = 12208
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign みずき (pop) to みずき一丁目
geom_only_code <- c("122080690")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122080690"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080260",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 700
pref_geom_only_1$mun_code.y = 12208
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 泉 (pop) to 泉一丁目 泉二丁目 and 泉三丁目 (geom)
geom_only_code <- c("122080770", "122080780", "122080790")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122080770"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080400",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 700
pref_geom_only_1$mun_code.y = 12208
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 光葉町 (pop) to 光葉町一丁目
geom_only_code <- c("122080800")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122080800"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080410",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 700
pref_geom_only_1$mun_code.y = 12208
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign なみき (pop) to 次木 (mutual), and merge six blocks (次木 親野井 古布内) together.
# This is because なみき is newly created in 2016 by combining parts of those three blocks.
pref_noda_yotsukaido_mutual[pref_noda_yotsukaido_mutual$code.y == "122080630",]$pop <-
    pref_noda_yotsukaido_mutual[pref_noda_yotsukaido_mutual$code.y == "122080630",]$pop +
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080810",]$pop
# Combine the four blocks
pref_mutual_new_address <- pref_noda_yotsukaido_mutual %>%
    dplyr::filter(code.y %in% c("122080620",
                              "122080610")) %>%
    dplyr::summarise(code.y = first(code.y),
                     mun_code.y = first(mun_code.y),
                     sub_code = first(sub_code),
                     sub_name = "combined",
                     pop = sum(pop),
                     CITY_NAME = first(CITY_NAME),
                     S_NAME = first(S_NAME),
                     KIHON1 = first(KIHON1),
                     JINKO = sum(JINKO),
                     geometry = sf::st_union(geometry))
# Data frame expect those four blocks
pref_mutual_without_new_address <- pref_noda_yotsukaido_mutual %>%
    dplyr::filter(!code.y %in% c("122080620",
                                 "122080610"))
# Combine them together
pref_noda_yotsukaido_mutual <- pref_mutual_new_address %>%
    dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 丸井 (pop) to 岡田 (mutual)
pref_noda_yotsukaido_mutual[pref_noda_yotsukaido_mutual$code.y == "122080670",]$pop <-
    pref_noda_yotsukaido_mutual[pref_noda_yotsukaido_mutual$code.y == "122080670",]$pop +
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122080840",]$pop

# Assign 千代田 (pop) to 千代田一丁目 (geom)
geom_only_code <- c("122280260")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280260"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280230",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 260
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 旭ケ丘 (pop) to 旭ケ丘一丁目 (geom)
geom_only_code <- c("122280310")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280310"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280240",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 310
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign みそら (pop) to みそら一丁目 (geom)
geom_only_code <- c("122280360")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280360"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280250",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 360
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign つくし座 (pop) to つくし座一丁目 (geom)
geom_only_code <- c("122280400")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280400"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280260",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 400
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign さちが丘 (pop) to さちが丘一丁目 (geom)
geom_only_code <- c("122280440")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280440"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280280",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 440
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 美しが丘 (pop) to 美しが丘一丁目 (geom)
geom_only_code <- c("122280460")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280460"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280290",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 460
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign めいわ (pop) to めいわ一丁目 (geom)
geom_only_code <- c("122280181")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280181"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280300",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 181
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 池花 (pop) to 池花一丁目 (geom)
geom_only_code <- c("122280051")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280051"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280310",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 51
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 鷹の台 (pop) to 鷹の台一丁目 (geom)
geom_only_code <- c("122280171")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280171"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280320",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 171
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign もねの里 (pop) to もねの里2丁目 (geom)
geom_only_code <- c("122280021")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code.y == "122280021"]$pop <-
    pref_noda_yotsukaido_pop_only[pref_noda_yotsukaido_pop_only$code.x == "122280330",]$pop
# Add sub_code and mun_code
pref_geom_only_1$sub_code = 21
pref_geom_only_1$mun_code.y = 12228
# bind it with. mutual data frame
pref_noda_yotsukaido_mutual <- rbind(pref_noda_yotsukaido_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

### Assign remaining `pref_noda_yotsukaido_geom_only` into `pref_noda_yotsukaido_mutual` ###
# Assign 四街道市 四街道１丁目 of `pref_geom_only` into 四街道市 四街道 of `pref_mutual`
geom_only_code <- c("122280090")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 90
# Group together with `pref_mutual`
pref_mutual[pref_noda_yotsukaido_mutual$code.y == "122280080",]$geometry <-
    sf::st_union(filter(pref_noda_yotsukaido_mutual, code.y == "122280080")$geometry,
                 filter(pref_geom_only_1, code.y == "122280090")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 野田市 岩名一丁目 of `pref_geom_only` into 野田市 岩名 of `pref_mutual`
geom_only_code <- c("122080310", "122080320")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 310
# Group together with `pref_mutual`
pref_mutual[pref_noda_yotsukaido_mutual$code.y == "122080230",]$geometry <-
    sf::st_union(filter(pref_noda_yotsukaido_mutual, code.y == "122080230")$geometry,
                 filter(pref_geom_only_1, code.y == "122080310")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Assign 野田市 花井一丁目 of `pref_geom_only` into 野田市 花井 of `pref_mutual`
geom_only_code <- c("122080191")
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code) %>%
    group_by(mun_code.y) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 191
# Group together with `pref_mutual`
pref_mutual[pref_noda_yotsukaido_mutual$code.y == "122080192",]$geometry <-
    sf::st_union(filter(pref_noda_yotsukaido_mutual, code.y == "122080192")$geometry,
                 filter(pref_geom_only_1, code.y == "122080191")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_noda_yotsukaido_geom_only <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% geom_only_code == FALSE)

# Combine `pref_noda_yotsukaido_mutual` into `pref_mutual`
# To do so, rename the columns of pref_noda_yotsukaido_mutual.
# Also, remove the irrelevant columns.
pref_noda_yotsukaido_mutual <- pref_noda_yotsukaido_mutual %>%
    dplyr::rename(code = code.y) %>%
    dplyr::rename(mun_code = mun_code.y) %>%
    dplyr::select(-c(code.x, mun_code.x))
# Then, bind rows of pref_noda_yotsukaido_mutual into pref_mutual
pref_mutual <- rbind(pref_mutual, pref_noda_yotsukaido_mutual)

######################
### General Method ###
######################
# Match or combine areas so that each area in `pref_pop_only` is matched with an existing area
# Assign 木更津市千束台 to 請西
pref_mutual[pref_mutual$code == "122061160",]$pop <-
    pref_mutual[pref_mutual$code == "122061160",]$pop +
    pref_pop_only[pref_pop_only$code == "122061500",]$pop

# Assign 木更津市北浜町 to 畔戸
pref_mutual[pref_mutual$code == "122066640",]$pop <-
    pref_mutual[pref_mutual$code == "122066640",]$pop +
    pref_pop_only[pref_pop_only$code == "122066670",]$pop

# Assign 松戸市南花島向町 and 南花島中町 into 南花島
pref_mutual[pref_mutual$code == "122071290",]$pop <-
    pref_mutual[pref_mutual$code == "122071290",]$pop +
    pref_pop_only[pref_pop_only$code == "122071640",]$pop +
    pref_pop_only[pref_pop_only$code == "122071650",]$pop

# Assign 茂原市小林飛地 to 小林
pref_mutual[pref_mutual$code == "122100050",]$pop <-
    pref_mutual[pref_mutual$code == "122100050",]$pop +
    pref_pop_only[pref_pop_only$code == "122100790",]$pop

# Assign 茂原市中之郷飛地 and 川島飛地 into 六ツ野
pref_mutual[pref_mutual$code == "122100210",]$pop <-
    pref_mutual[pref_mutual$code == "122100210",]$pop +
    pref_pop_only[pref_pop_only$code == "122100800",]$pop +
    pref_pop_only[pref_pop_only$code == "122100810",]$pop

# Assign 茂原市小萱場 to 萱場
pref_mutual[pref_mutual$code == "122100710",]$pop <-
    pref_mutual[pref_mutual$code == "122100710",]$pop +
    pref_pop_only[pref_pop_only$code == "122100820",]$pop

# Assign 茂原市吉井上 to 吉井下
pref_mutual[pref_mutual$code == "122100700",]$pop <-
    pref_mutual[pref_mutual$code == "122100700",]$pop +
    pref_pop_only[pref_pop_only$code == "122100830",]$pop

# Assign 佐倉市 寺崎北 to 寺崎, and merge two blocks (六崎 寺崎) together.
# This is because 佐倉市 寺崎北 is newly created in 2015 by combining parts of those two blocks.
pref_mutual[pref_mutual$code == "122120980",]$pop <-
    pref_mutual[pref_mutual$code == "122120980",]$pop +
    pref_pop_only[pref_pop_only$code == "122120981",]$pop
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
    dplyr::filter(code %in% c("122120980",
                              "122120972")) %>%
    dplyr::summarise(code = first(code),
                     mun_code = first(mun_code),
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
    dplyr::filter(!code %in% c("122120980",
                               "122120972"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
    dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 東金市八坂台 to 松之郷
pref_mutual[pref_mutual$code == "122130150",]$pop <-
    pref_mutual[pref_mutual$code == "122130150",]$pop +
    pref_pop_only[pref_pop_only$code == "122130720",]$pop

# Assign 柏市 柏インター東 and 柏インター南 in to 大青田, and merge four blocks (大青田 船戸 新十余二 ⻘⽥新⽥⾶地) together.
# This is because 柏市 柏インター東 and 柏インター南 is newly created in 2016 and 2018  by combining parts of four blocks.
pref_mutual[pref_mutual$code == "122170140",]$pop <-
    pref_mutual[pref_mutual$code == "122170140",]$pop +
    pref_pop_only[pref_pop_only$code == "122171360",]$pop +
    pref_pop_only[pref_pop_only$code == "122171370",]$pop
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
    dplyr::filter(code %in% c("122170140",
                              "122170870",
                              "122170420")) %>%
    dplyr::summarise(code = first(code),
                     mun_code = first(mun_code),
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
                               "122170420"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
    dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 柏市染井入新田 to 鷲野谷
pref_mutual[pref_mutual$code == "122171180",]$pop <-
    pref_mutual[pref_mutual$code == "122171180",]$pop +
    pref_pop_only[pref_pop_only$code == "122171380",]$pop

# Assign 市原市姉崎東 to 姉崎
pref_mutual[pref_mutual$code == "122191100",]$pop <-
    pref_mutual[pref_mutual$code == "122191100",]$pop +
    pref_pop_only[pref_pop_only$code == "122192970",]$pop

# Assign 流山市 おおたかの森東 and おおたかの森西 in to 十太夫 (geom),
# and merge those blocks (大字流山, 駒木, 東初石, 十太夫,
# 西初石, 市野谷 大字大畔 大字三輪野山 野々下, 美田) together.
# Moreover, since おおたかの森北 is assigned to 美原 in the `pref_mutual` ,
# we will assigned the population of おおたかの森北 together
# This is because おおたかの森 is newly created in 2019 by combining parts of those blocks.
geom_only_code <- c("122200560")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code)
pref_geom_only_1[pref_geom_only_1$code == "122200560"]$pop <-
  pref_mutual[pref_mutual$code == "122200650",]$pop +
  pref_pop_only[pref_pop_only$code == "122200670",]$pop +
  pref_pop_only[pref_pop_only$code == "122200680",]$pop
# Add sub_code
pref_geom_only_1$sub_code = 560
# bind it with. mutual data frame
pref_mutual <- rbind(pref_mutual, pref_geom_only_1)
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("122200560",
                            "122200660",
                            "122200530",
                            "122200600",
                            "122200610",
                            "122200470",
                            "122200310",
                            "122200030",
                            "122200480",
                            "122200570")) %>%
  dplyr::summarise(code = first(code),
                   mun_code = first(mun_code),
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
  dplyr::filter(!code %in% c("122200560",
                             "122200660",
                             "122200530",
                             "122200600",
                             "122200610",
                             "122200470",
                             "122200310",
                             "122200030",
                             "122200480",
                             "122200570"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 流山市美原 to 美原
# As explained above, the old code for 美原 is used in the 2020 census.
# Thus, instead of adding, we will replace the population of `pref_mutual`
pref_mutual[pref_mutual$code == "122200650",]$pop <-
    pref_pop_only[pref_pop_only$code == "122200200",]$pop

# Assign 袖ケ浦市 袖ケ浦駅前 to 坂戸市場, and merge two blocks (坂戸市場 奈良輪) together.
# This is because 袖ケ浦市 袖ケ浦駅前 is newly created in 2019 by combining parts of two blocks.
pref_mutual[pref_mutual$code == "122290010",]$pop <-
    pref_mutual[pref_mutual$code == "122290010",]$pop +
    pref_pop_only[pref_pop_only$code == "122290069",]$pop
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
    dplyr::filter(code %in% c("122290010",
                              "122290021",
                              "122290021" # 奈良輪 has two codes
                              )) %>%
    dplyr::summarise(code = first(code),
                     mun_code = first(mun_code),
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
    dplyr::filter(!code %in% c("122290010",
                               "122290021",
                               "122290021" # 奈良輪 has two codes
                               ))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
    dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 八街市 八街は to 八街は (geom).
geom_only_code <- c("122300091", "122300092", "122300093")
pref_geom_only_1 <- pref_geom_only %>%
    filter(code %in% geom_only_code) %>%
    group_by(mun_code) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1[pref_geom_only_1$code == "122300091"]$pop <-
    pref_pop_only[pref_pop_only$code == "122300090",]$pop
# Add sub_code
pref_geom_only_1$sub_code = 90
# bind it with. mutual data frame
pref_mutual <- rbind(pref_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
    filter(code %in% geom_only_code == FALSE)

# Assign 多古町 大門 to 高津原
pref_mutual[pref_mutual$code == "123470290",]$pop <-
    pref_mutual[pref_mutual$code == "123470290",]$pop +
    pref_pop_only[pref_pop_only$code == "123470301",]$pop

# Assign 御宿町 御宿台 to 御宿台
pref_mutual[pref_mutual$code == "124430100",]$pop <-
    pref_mutual[pref_mutual$code == "124430100",]$pop +
    pref_pop_only[pref_pop_only$code == "124430110",]$pop

### Assign remaining `pref_geom` into `pref_mutual` ###
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
pref_geom_only_1$sub_code = 312
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

# Assign 市原市 岩崎 of `pref_geom_only` into 市原市 岩崎 of `pref_mutual`
geom_only_code <- c("122190122")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 122
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122190121",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122190121")$geometry,
               filter(pref_geom_only_1, code == "122190122")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 八街市 八街ほ of `pref_geom_only` into 八街市 八街ほ of `pref_mutual`
geom_only_code <- c("122300030",
                    "122300040",
                    "122300050",
                    "122300500")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 30
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122300010",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122300010")$geometry,
               filter(pref_geom_only_1, code == "122300030")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 八街市 八街に of `pref_geom_only` into 八街市 八街に of `pref_mutual`
geom_only_code <- c("122300070",
                    "122300490")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 70
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122300020",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122300020")$geometry,
               filter(pref_geom_only_1, code == "122300070")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 八街市 八街ろ of `pref_geom_only` into 八街市 八街ろ of `pref_mutual`
geom_only_code <- c("122300400")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 400
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122300130",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122300130")$geometry,
               filter(pref_geom_only_1, code == "122300400")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 八街市 吉倉 of `pref_geom_only` into 八街市 吉倉 of `pref_mutual`
geom_only_code <- c("122300440")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 440
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122300280",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122300280")$geometry,
               filter(pref_geom_only_1, code == "122300440")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 八街市 八街へ of `pref_geom_only` into 八街市 八街へ of `pref_mutual`
geom_only_code <- c("122300520")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 440
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "122300060",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "122300060")$geometry,
               filter(pref_geom_only_1, code == "122300520")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 多古町 多古 of `pref_geom_only` into 多古町 多古 of `pref_mutual`
geom_only_code <- c("123470020",
                    "123470030",
                    "123470040",
                    "123470050",
                    "123470060",
                    "123470070",
                    "123470080",
                    "123470090",
                    "123470100",
                    "123470110",
                    "123470120",
                    "123470130",
                    "123470500")
pref_geom_only_1 <- pref_geom_only %>%
    filter(code %in% geom_only_code) %>%
    group_by(mun_code) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 20
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "123470010",]$geometry <-
    sf::st_union(filter(pref_mutual, code == "123470010")$geometry,
                 filter(pref_geom_only_1, code == "123470020")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
    filter(code %in% geom_only_code == FALSE)

# Assign 多古町 間倉 of `pref_geom_only` into 多古町 間倉 of `pref_mutual`
geom_only_code <- c("123470210")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 210
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "123470200",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "123470200")$geometry,
               filter(pref_geom_only_1, code == "123470210")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 多古町 十余三 of `pref_geom_only` into 多古町 十余三 of `pref_mutual`
geom_only_code <- c("123470360")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 360
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "123470350",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "123470350")$geometry,
               filter(pref_geom_only_1, code == "123470360")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 多古町 東松崎 of `pref_geom_only` into 多古町 東松崎 of `pref_mutual`
geom_only_code <- c("123470390")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 390
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "123470380",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "123470380")$geometry,
               filter(pref_geom_only_1, code == "123470390")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 多古町 南玉造 of `pref_geom_only` into 多古町 南玉造 of `pref_mutual`
geom_only_code <- c("123470430")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 430
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "123470420",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "123470420")$geometry,
               filter(pref_geom_only_1, code == "123470430")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 多古町 南中 of `pref_geom_only` into 多古町 南中 of `pref_mutual`
geom_only_code <- c("123470450")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 450
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "123470440",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "123470440")$geometry,
               filter(pref_geom_only_1, code == "123470450")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長生村 小泉 of `pref_geom_only` into 長生村 小泉 of `pref_mutual`
geom_only_code <- c("124230210",
                    "124230220")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 210
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124230100",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124230100")$geometry,
               filter(pref_geom_only_1, code == "124230210")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長生村 本郷 of `pref_geom_only` into 長生村 本郷 of `pref_mutual`
geom_only_code <- c("124230130",
                    "124230140",
                    "124230150",
                    "124230190",
                    "124230200")
pref_geom_only_1 <- pref_geom_only %>%
    filter(code %in% geom_only_code) %>%
    group_by(mun_code) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 130
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124230080",]$geometry <-
    sf::st_union(filter(pref_mutual, code == "124230080")$geometry,
                 filter(pref_geom_only_1, code == "124230130")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
    filter(code %in% geom_only_code == FALSE)

# Assign 長生村 一松 of `pref_geom_only` into 長生村 一松 of `pref_mutual`
geom_only_code <- c("124230230",
                    "124230240",
                    "124230250",
                    "124230260",
                    "124230270",
                    "124230280",
                    "124230290",
                    "124230300",
                    "124230310",
                    "124230320",
                    "124230330",
                    "124230350")
pref_geom_only_1 <- pref_geom_only %>%
    filter(code %in% geom_only_code) %>%
    group_by(mun_code) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 230
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124230160",]$geometry <-
    sf::st_union(filter(pref_mutual, code == "124230160")$geometry,
                 filter(pref_geom_only_1, code == "124230230")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
    filter(code %in% geom_only_code == FALSE)

# Assign 長生村 驚 of `pref_geom_only` into 長生村 驚 of `pref_mutual`
geom_only_code <- c("124230340")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 340
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124230180",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124230180")$geometry,
               filter(pref_geom_only_1, code == "124230340")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 鴇谷 of `pref_geom_only` into 長柄町 鴇谷 of `pref_mutual`
geom_only_code <- c("124260270",
                    "124260280",
                    "124260480")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 270
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260180",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260180")$geometry,
               filter(pref_geom_only_1, code == "124260270")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 立鳥 of `pref_geom_only` into 長柄町 立鳥 of `pref_mutual`
geom_only_code <- c("124260290")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 290
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260190",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260190")$geometry,
               filter(pref_geom_only_1, code == "124260290")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 針ケ谷 of `pref_geom_only` into 長柄町 針ケ谷 of `pref_mutual`
geom_only_code <- c("124260300",
                    "124260310")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 300
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260200",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260200")$geometry,
               filter(pref_geom_only_1, code == "124260300")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 高山 of `pref_geom_only` into 長柄町 高山 of `pref_mutual`
geom_only_code <- c("124260320")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 320
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260210",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260210")$geometry,
               filter(pref_geom_only_1, code == "124260320")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 大庭 of `pref_geom_only` into 長柄町 大庭 of `pref_mutual`
geom_only_code <- c("124260330")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 330
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260220",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260220")$geometry,
               filter(pref_geom_only_1, code == "124260330")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 大津倉 of `pref_geom_only` into 長柄町 大津倉 of `pref_mutual`
geom_only_code <- c("124260340",
                    "124260350",
                    "124260360")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 340
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260230",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260230")$geometry,
               filter(pref_geom_only_1, code == "124260340")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 田代 of `pref_geom_only` into 長柄町 田代 of `pref_mutual`
geom_only_code <- c("124260370")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 370
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260240",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260240")$geometry,
               filter(pref_geom_only_1, code == "124260370")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 刑部 of `pref_geom_only` into 長柄町 刑部 of `pref_mutual`
geom_only_code <- c("124260380",
                    "124260390",
                    "124260400",
                    "124260410",
                    "124260420",
                    "124260430",
                    "124260460")
pref_geom_only_1 <- pref_geom_only %>%
    filter(code %in% geom_only_code) %>%
    group_by(mun_code) %>%
    mutate(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    slice(1)
pref_geom_only_1$sub_code = 380
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260250",]$geometry <-
    sf::st_union(filter(pref_mutual, code == "124260250")$geometry,
                 filter(pref_geom_only_1, code == "124260380")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
    filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 金谷 of `pref_geom_only` into 長柄町 金谷 of `pref_mutual`
geom_only_code <- c("124260440")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 440
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260260",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260260")$geometry,
               filter(pref_geom_only_1, code == "124260440")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 味庄 of `pref_geom_only` into 長柄町 味庄 of `pref_mutual`
geom_only_code <- c("124260450")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 450
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260050",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260050")$geometry,
               filter(pref_geom_only_1, code == "124260450")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 上野 of `pref_geom_only` into 長柄町 上野 of `pref_mutual`
geom_only_code <- c("124260470")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 470
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260080",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260080")$geometry,
               filter(pref_geom_only_1, code == "124260470")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 長柄町 山根 of `pref_geom_only` into 長柄町 山根 of `pref_mutual`
geom_only_code <- c("124260490")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 490
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "124260030",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "124260030")$geometry,
               filter(pref_geom_only_1, code == "124260490")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

#############################
# Finalize pref object
pref <- pref_mutual %>%
    bind_rows(pref_geom_only) %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code) %>%
    sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
sum(pref$pop)
