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

"旭ケ丘"
"みそら"
"つくし座"
"さちが丘"
"美しが丘"
"めいわ"
"池花"
"鷹の台"
"もねの里"

### General Method ###
# Match or combine areas so that each area in `pref_pop_only` is matched with an existing area

#12206 木更津市
"千束台"

"北浜町"

#12207 松戸市
# merge to 122071290 南花島

"南花島向町"
"南花島中町"

# 茂原
#12210
"小林飛地" # Number only? merge with Kanbayashi to avoid tobochi?

"中之郷飛地" # Number only? or assign to 中之郷 or 六ツ野　

"川島飛地" # Number only? or assign to 川島 or 六ツ野　
"小萱場"
"吉井上"

#12212
"寺崎北"
#12213
"八坂台"

#12217
"柏インター東"
"柏インター南"
"染井入新田"

#12219
"姉崎東"

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
pref <- pref %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code) %>%
    sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
