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

set.seed(12345)

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
gun_exception <- c(12400) # Sanbu-gun (12403 12409, 12410)

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
pref_geom_only <- setdiff(pref_geom_only, pref_mutual) %>%
    dplyr::filter(mun_code %in% c(12208, 12228) == FALSE)

# 3. Areas that exist only in 2020 Census data (`pref_pop_2020`)
pref_pop_only <- merge(pref_pop_2020, pref_shp_cleaned, by = "code", all.x = TRUE)
pref_pop_only <- setdiff(pref_pop_only, pref_mutual) %>%
    filter(pop > 0) %>%
    dplyr::filter(mun_code %in% c(12208, 12228) == FALSE)

# Match or combine data so that every single census block is taken into account
# Add municipality code, sub_code, sub_name to areas that only exist in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only$pop <- 0
pref_geom_only$mun_code <- substr(pref_geom_only$code, start = 1, stop = 5)

### Unique Method for Chiba ###
# Match population data and shape file by S_NAME for 野田市 (12208) and 四街道市 (12228).
# Subset the shape file
pref_noda_yotsukaido_shp <- pref_shp_cleaned %>%
    dplyr::mutate(mun_code = as.numeric(code) %/% 10000) %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::mutate(sub_name = S_NAME)
# Match by S_NAME (sub_name)
pref_noda_yotsukaido_mutual <- pref_pop_2020 %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::inner_join(pref_noda_yotsukaido_shp, by = "sub_name")
pref_noda_yotsukaido_pop_only <- pref_pop_2020 %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::left_join(pref_noda_yotsukaido_shp, by = "sub_name") %>%
    dplyr::setdiff(pref_noda_yotsukaido_mutual)
pref_noda_yotsukaido_geom_only <- pref_pop_2020 %>%
    dplyr::filter(mun_code %in% c(12208, 12228)) %>%
    dplyr::right_join(pref_noda_yotsukaido_shp, by = "sub_name") %>%
    dplyr::setdiff(pref_noda_yotsukaido_mutual)

# Match or combine areas so that each area in `pref_pop_only` is matched with an existing area
# Assign 清水公園東 (pop) to 清水公園東一丁目 and 清水公園東二丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122080700", "122080710")) %>%
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

# Assign 桜の里 (pop) to 桜の里一丁目 桜の里二丁目 and 桜の里三丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122080720", "122080730", "122080740")) %>%
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

# Assign つつみ野 (pop) to つつみ野一丁目 and つつみ野二丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122080750", "122080760")) %>%
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

# Assign みずき (pop) to みずき一丁目
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122080690")) %>%
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

# Assign 泉 (pop) to 泉一丁目 泉二丁目 and 泉三丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122080770", "122080780", "122080790")) %>%
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

# Assign 光葉町 (pop) to 光葉町一丁目
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122080800")) %>%
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
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280260")) %>%
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

# Assign 旭ケ丘 (pop) to 旭ケ丘一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280310")) %>%
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

# Assign みそら (pop) to みそら一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280360")) %>%
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

# Assign つくし座 (pop) to つくし座一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280400")) %>%
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

# Assign さちが丘 (pop) to さちが丘一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280440")) %>%
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

# Assign 美しが丘 (pop) to 美しが丘一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280460")) %>%
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

# Assign めいわ (pop) to めいわ一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280181")) %>%
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

# Assign 池花 (pop) to 池花一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280051")) %>%
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

# Assign 鷹の台 (pop) to 鷹の台一丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280171")) %>%
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

# Assign もねの里 (pop) to もねの里2丁目 (geom)
pref_geom_only_1 <- pref_noda_yotsukaido_geom_only %>%
    filter(code.y %in% c("122280021")) %>%
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

# Assign 柏市 柏インター東 and 柏インター南 in to 大青田, and merge two blocks (大青田 船戸 新十余二 ⻘⽥新⽥⾶地) together.
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

#12220
"美原"
"おおたかの森東"
"おおたかの森西"

#12229
"袖ケ浦駅前"

#12230
"八街は"

#12347
"大門"

#12443
"御宿台"

# Finalize pref object
pref <- pref %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code) %>%
    sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
sum(pref$pop)
