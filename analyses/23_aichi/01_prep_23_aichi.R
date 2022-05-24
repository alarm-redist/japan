###############################################################################
# Download and prepare data for `23_aichi` analysis
# © ALARM Project, May 2021
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
pref_code <- 23
pref_name <- "aichi"
lakes_removed <- c()
ndists_new <- 16
ndists_old <- 15
sq_max_to_min <- 1.574
sq_max_to_tottori2 <- 2.012
sq_mun_splits <- 3
sq_gun_splits <- 0
sq_koiki_splits <- 1
pop_tol <- 0.20

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

# Clean 2020 Census data at the 小地域-level
pref_pop_2020 <- clean_pref_pop_2020(pref_pop_2020, sub_code = TRUE)

# Match 2015 shapefile (`pref_shp_cleaned`) with 2020 Census data (`pref_pop_2020`)
# Combine municipality code with sub-code
pref_shp_cleaned <- mutate(pref_shp_cleaned, code = str_c(code, KIHON1))
# Combine municipality code with sub-code
pref_pop_2020 <- mutate(pref_pop_2020, code = str_c(mun_code, str_pad(sub_code, 4, pad = "0")))

# TODO Match areas that do not exist in either `pref_shp_cleaned` or `pref_pop_2020`
# 1. Areas that are accounted for in both dataframes
pref_mutual <- merge(pref_pop_2020, pref_shp_cleaned, by = "code")

# 2. Areas that exist only in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only <- merge(pref_shp_cleaned, pref_pop_2020, by = "code", all.x = TRUE)
pref_geom_only <- setdiff(pref_geom_only, pref_mutual)

# 3. Areas that exist only in 2020 Census data (`pref_pop_2020`)
pref_pop_only <- merge(pref_pop_2020, pref_shp_cleaned, by = "code", all.x = TRUE)
pref_pop_only <- setdiff(pref_pop_only, pref_mutual) %>%
    filter(pop > 0)

# Match or combine data so that every single census block is taken into account
# Add municipality code, sub_code, sub_name to areas that only exist in 2015 shapefile (`pref_shp_cleaned`)
pref_geom_only$pop <- 0
pref_geom_only$mun_code <- substr(pref_geom_only$code, start = 1, stop = 5)

# Match or combine areas so that each area in `pref_pop_only` is matched with an existing area

# Assign 春日井市庄名町 to 春日井市庄名町
pref_geom_only_1 <- filter(pref_geom_only, code == "232061200")
pref_geom_only_1[pref_geom_only_1$code == "232061200"]$pop <-
  pref_pop_only[pref_pop_only$code == "232061201",]$pop +
  pref_pop_only[pref_pop_only$code == "232061202",]$pop
# Add sub_code
pref_geom_only_1$sub_code = 1201

# Assign 豊田市新盛町&永野町 to 豊田市新盛町
pref_geom_only_2 <- filter(pref_geom_only, code == "232113390")  # 豊田市新盛町
pref_geom_only_2[pref_geom_only_2$code == "232113390"]$pop <-
  pref_pop_only[pref_pop_only$code == "232113391",]$pop + # 豊田市新盛町
  pref_pop_only[pref_pop_only$code == "232113392",]$pop # 豊田市永野町
# Add sub_code
pref_geom_only_2$sub_code = 3391

# Assign 豊田市井ノ口町&田振町&近岡町&東大島町 to 豊田市井ノ口町
pref_geom_only_3 <- filter(pref_geom_only, code == "232113840")  # 豊田市井ノ口町
pref_geom_only_3[pref_geom_only_3$code == "232113840"]$pop <-
  pref_pop_only[pref_pop_only$code == "232113841",]$pop + # 豊田市井ノ口町
  pref_pop_only[pref_pop_only$code == "232113842",]$pop + # 豊田市田振町
  pref_pop_only[pref_pop_only$code == "232113843",]$pop + # 豊田市近岡町
  pref_pop_only[pref_pop_only$code == "232113844",]$pop   # 豊田市東大島町
# Add sub_code
pref_geom_only_3$sub_code = 3841

# Assign 豊田市篭林町&野林町 to 豊田市篭林町
pref_geom_only_4 <- filter(pref_geom_only, code == "232113860")  # 豊田市篭林町
pref_geom_only_4[pref_geom_only_4$code == "232113860"]$pop <-
  pref_pop_only[pref_pop_only$code == "232113861",]$pop + # 豊田市篭林町
  pref_pop_only[pref_pop_only$code == "232113862",]$pop # 豊田市野林町
# Add sub_code
pref_geom_only_4$sub_code = 3861

# Assign 豊田市下国谷町&白倉町 to 豊田市下国谷町
pref_geom_only_5 <- filter(pref_geom_only, code == "232113950")  # 豊田市下国谷町
pref_geom_only_5[pref_geom_only_5$code == "232113950"]$pop <-
  pref_pop_only[pref_pop_only$code == "232113951",]$pop + # 豊田市下国谷町
  pref_pop_only[pref_pop_only$code == "232113952",]$pop # 豊田市白倉町
# Add sub_code
pref_geom_only_5$sub_code = 3951

# Assign 日進市竹の山 to 日進市竹の山
pref_geom_only_6 <- filter(pref_geom_only, code == "232300170")  # 日進市竹の山
pref_geom_only_6[pref_geom_only_6$code == "232300170"]$pop <-
  pref_pop_only[pref_pop_only$code == "232300360",]$pop  # 日進市竹の山
# Add sub_code
pref_geom_only_6$sub_code = 360

# Assign 蟹江町大字鍋蓋新田&南 to 蟹江町大字鍋蓋新田
pref_geom_only_7 <- filter(pref_geom_only, code == "234250080")  # 蟹江町大字鍋蓋新田
pref_geom_only_7[pref_geom_only_7$code == "234250080"]$pop <-
  pref_pop_only[pref_pop_only$code == "234250081",]$pop + # 蟹江町大字鍋蓋新田
  pref_pop_only[pref_pop_only$code == "234250082",]$pop  # 蟹江町南
# Add sub_code
pref_geom_only_7$sub_code = 81

# Assign 長久手市深廻間 (pop: 0)
pref_geom_only_8 <- filter(pref_geom_only, code == "232380150")
pref_geom_only_8$sub_code = 150

# Assign 日進市浅田平子 to 日進市浅田町
pref_mutual[pref_mutual$code == "232300020",]$pop <- # 日進市浅田町
    pref_mutual[pref_mutual$code == "232300020",]$pop + # 日進市浅田町
    pref_pop_only[pref_pop_only$code == "232300330",]$pop # 日進市浅田平子

# Assign 日進市米野木台 to 日進市米野木町
pref_mutual[pref_mutual$code == "232300080",]$pop <- # 日進市米野木町
  pref_mutual[pref_mutual$code == "232300080",]$pop + # 日進市米野木町
  pref_pop_only[pref_pop_only$code == "232300370",]$pop # 日進市米野木台

# Assign 港区南陽町協和&畑中&六軒屋 to 港区南陽町大字西福田
pref_mutual[pref_mutual$code == "231111400",]$pop <- # 南陽町大字西福田
  pref_mutual[pref_mutual$code == "231111400",]$pop + # 南陽町大字西福田
  pref_pop_only[pref_pop_only$code == "231112080",]$pop + # 南陽町協和
  pref_pop_only[pref_pop_only$code == "231112090",]$pop + # 南陽町畑中
  pref_pop_only[pref_pop_only$code == "231112100",]$pop # 南陽町六軒屋

# Assign 港区福前 to 港区南陽町大字福田前新田
pref_mutual[pref_mutual$code == "231112060",]$pop <- # 南陽町大字福田前新田
  pref_mutual[pref_mutual$code == "231112060",]$pop + # 南陽町大字福田前新田
  pref_pop_only[pref_pop_only$code == "231112110",]$pop #港区福前

# Assign 緑区有松町有松幕山&桶狭間西&桶狭間巻山  to 緑区有松町大字有松・桶狭間
pref_mutual[pref_mutual$code == "231143010",]$pop <- # 緑区有松町大字有松・桶狭間
  pref_mutual[pref_mutual$code == "231143010",]$pop + # 緑区有松町大字有松・桶狭間
  pref_pop_only[pref_pop_only$code == "231141570",]$pop + # 有松町有松幕山
  pref_pop_only[pref_pop_only$code == "231141580",]$pop + # 有松町桶狭間西
  pref_pop_only[pref_pop_only$code == "231141590",]$pop # 有松町桶狭間巻山

# Assign 緑区徳の前&元徳重 to 緑区鳴海町
pref_mutual[pref_mutual$code == "231142010",]$pop <- # 緑区鳴海町
  pref_mutual[pref_mutual$code == "231142010",]$pop + # 緑区鳴海町
  pref_pop_only[pref_pop_only$code == "231141600",]$pop + # 緑区徳の前
  pref_pop_only[pref_pop_only$code == "231141620",]$pop  # 緑区元徳重

# Assign 西尾市羽塚西ノ山 to 西尾市羽塚町
pref_mutual[pref_mutual$code == "232132030",]$pop <- # 西尾市羽塚町
  pref_mutual[pref_mutual$code == "232132030",]$pop + # 西尾市羽塚町
  pref_pop_only[pref_pop_only$code == "232137530",]$pop  # 西尾市羽塚西ノ山

# Assign 西尾市富山 to 西尾市富山町
pref_mutual[pref_mutual$code == "232132070",]$pop <- # 西尾市富山町
  pref_mutual[pref_mutual$code == "232132070",]$pop + # 西尾市富山町
  pref_pop_only[pref_pop_only$code == "232137540",]$pop  # 西尾市富山

# Assign 西尾市矢田 to 西尾市上矢田町
pref_mutual[pref_mutual$code == "232132050",]$pop <- # 西尾市上矢田町
  pref_mutual[pref_mutual$code == "232132050",]$pop + # 西尾市上矢田町
  pref_pop_only[pref_pop_only$code == "232137550",]$pop  # 西尾市矢田

# Assign 常滑市虹の丘 to 常滑市大鳥町
pref_mutual[pref_mutual$code == "232160430",]$pop <- # 常滑市大鳥町
  pref_mutual[pref_mutual$code == "232160430",]$pop + # 常滑市大鳥町
  pref_pop_only[pref_pop_only$code == "232160490",]$pop  # 常滑市虹の丘

# Assign 常滑市大谷朝陽ヶ丘 to 常滑市大谷
pref_mutual[pref_mutual$code == "232160910",]$pop <- # 常滑市大谷
  pref_mutual[pref_mutual$code == "232160910",]$pop + # 常滑市大谷
  pref_pop_only[pref_pop_only$code == "232160900",]$pop  # 常滑市大谷朝陽ヶ丘

# Assign 小牧市文津 to 小牧市大字文津
pref_mutual[pref_mutual$code == "232190360",]$pop <- # 小牧市大字文津
  pref_mutual[pref_mutual$code == "232190360",]$pop + # 小牧市大字文津
  pref_pop_only[pref_pop_only$code == "232191000",]$pop  # 小牧市文津

# Assign 小牧市小松寺 to 小牧市大字小松寺
pref_mutual[pref_mutual$code == "232190370",]$pop <- # 小牧市大字小松寺
  pref_mutual[pref_mutual$code == "232190370",]$pop + # 小牧市大字小松寺
  pref_pop_only[pref_pop_only$code == "232191010",]$pop  # 小牧市小松寺

# Assign 小牧市久保 to 小牧市大字久保一色
pref_mutual[pref_mutual$code == "232190410",]$pop <- # 小牧市大字久保一色
  pref_mutual[pref_mutual$code == "232190410",]$pop + # 小牧市大字久保一色
  pref_pop_only[pref_pop_only$code == "232191020",]$pop  # 小牧市久保

# Assign 稲沢市東畑 to 稲沢市稲島町
pref_mutual[pref_mutual$code == "232201101",]$pop <- # 稲沢市稲島町
  pref_mutual[pref_mutual$code == "232201101",]$pop + # 稲沢市稲島町
  pref_pop_only[pref_pop_only$code == "232201103",]$pop  # 稲沢市東畑

# Assign 稲沢市島北浦町&島本郷町&島寺西町&島新田町   to 稲沢市島町
pref_mutual[pref_mutual$code == "232202141",]$pop <- # 稲沢市島町
  pref_mutual[pref_mutual$code == "232202141",]$pop + # 稲沢市島町
  pref_pop_only[pref_pop_only$code == "232202143",]$pop +  # 稲沢市東畑
  pref_pop_only[pref_pop_only$code == "232202144",]$pop +  # 稲沢市東畑
  pref_pop_only[pref_pop_only$code == "232202146",]$pop +  # 稲沢市東畑
  pref_pop_only[pref_pop_only$code == "232202147",]$pop   # 稲沢市東畑

# Assign 北名古屋市西春駅前 to 北名古屋市九之坪
pref_mutual[pref_mutual$code == "232340120",]$pop <- # 北名古屋市九之坪
  pref_mutual[pref_mutual$code == "232340120",]$pop + # 北名古屋市九之坪
  pref_pop_only[pref_pop_only$code == "232340260",]$pop #北名古屋市西春駅前

# Assign 東郷町兵庫&清水 to 東郷町三ツ池
pref_mutual[pref_mutual$code == "233020160",]$pop <- # 東郷町三ツ池
  pref_mutual[pref_mutual$code == "233020160",]$pop + # 東郷町三ツ池
  pref_pop_only[pref_pop_only$code == "233020170",]$pop + # 東郷町兵庫
  pref_pop_only[pref_pop_only$code == "233020180",]$pop # 東郷町清水

# Assign 東郷町大字春木 to 東郷町大字春木
pref_mutual[pref_mutual$code == "233020150",]$pop <- # 東郷町大字春木
  pref_mutual[pref_mutual$code == "233020150",]$pop + # 東郷町大字春木
  pref_pop_only[pref_pop_only$code == "233020190",]$pop # 東郷町大字春木

# Finalize pref object
pref <- rbind(pref_mutual, pref_geom_only_1, pref_geom_only_2,
              pref_geom_only_3, pref_geom_only_4, pref_geom_only_5,
              pref_geom_only_6, pref_geom_only_7, pref_geom_only_8)
pref <- pref %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code) %>%
    sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
