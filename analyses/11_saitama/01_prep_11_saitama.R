###############################################################################
# Download and prepare data for `11_saitama` analysis
# © ALARM Project, May 2022
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
nsims <- 30000 # Set so that the number of valid plans > 5,000
pref_code <- 11
pref_name <- "saitama"
lakes_removed <- c()
ndists_new <- 16
ndists_old <- 15
sq_max_to_min <- 1.442
sq_max_to_tottori2 <- 2.010
sq_mun_splits <- 8
sq_gun_splits <- 1
sq_koiki_splits <- 0
pop_tol <- 0.20

# Out of 8 total gun in Saitama, 5 guns are discontinued.
# Thus, e will set gun with discontinuity as `gun_exception` to process in the `02_sim` process.
gun_exception <- c(11320, # Iruma (11324, 11326, 11327)
                   11340, # Hiki (11341, 11342, 11343, 11346, 11347, 11348, 11349)
                   11360, # Chichibu (11361, 11362,11363, 11365, 11369)
                   11380, # Kodama (11381, 11383, 11385)
                   11460 # Kitakatsushika (11464, 11465)
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

# Match or combine areas so that each area in `pref_pop_only` (n=24) is matched with an existing area
# Assign さいたま市 西区 西大宮 to 大字指扇, and merge six blocks (西区大字指扇、清河寺、高木、中釘、宮前町、三橋) together.
# This is because さいたま市 西区 西大宮 was newly created in 2016 by combining parts of those six blocks.
pref_mutual[pref_mutual$code == "111010030",]$pop <- # 大字指扇
  pref_mutual[pref_mutual$code == "111010030",]$pop + # 大字指扇
  pref_pop_only[pref_pop_only$code == "111010310",]$pop # 西大宮
# Combine the six blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("111010010",
                            "111010020",
                            "111010030",
                            "111010080",
                            "111010090",
                            "111010100")) %>%
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
# Data frame expect those six blocks
pref_mutual_without_new_address <- pref_mutual %>%
  dplyr::filter(!code %in% c("111010010",
                             "111010020",
                             "111010030",
                             "111010080",
                             "111010090",
                             "111010100"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)

# Assign さいたま市 緑区 美園 to 大字上野田, and merge nine blocks (緑区大字上野田、玄蕃新田、下野田、大門、高畑、寺山、中野田、南部領辻、大崎) together.
# This is because さいたま市 緑区 美園 was newly created in 2017 by combining parts of those nine blocks.
pref_mutual[pref_mutual$code == "111090160",]$pop <- # 大字上野田
  pref_mutual[pref_mutual$code == "111090160",]$pop + # 大字上野田
  pref_pop_only[pref_pop_only$code == "111090350",]$pop # 美園
# Combine the nine blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("111090160",
                            "111090250",
                            "111090240",
                            "111090210",
                            "111090170",
                            "111090190",
                            "111090150",
                            "111090140",
                            "111090130")) %>%
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
# Data frame expect those six blocks
pref_mutual_without_new_address <- pref_mutual %>%
  dplyr::filter(!code %in% c("111090160",
                             "111090250",
                             "111090240",
                             "111090210",
                             "111090170",
                             "111090190",
                             "111090150",
                             "111090140",
                             "111090130"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 大間木 to 大字大間木
pref_mutual[pref_mutual$code == "111090050",]$pop <- # 大字大間木
  pref_mutual[pref_mutual$code == "111090050",]$pop + # 大字大間木
  pref_pop_only[pref_pop_only$code == "111090360",]$pop #大間木

# Assign さいたま市 岩槻区 美園東 to 大字尾ヶ崎, and merge four blocks (岩槻区大字尾ヶ崎、尾ヶ崎新田、釣上、釣上新田) together.
# This is because さいたま市 岩槻区 美園 was newly created in 2017 by combining parts of those four blocks.
pref_mutual[pref_mutual$code == "111100360",]$pop <- # 大字尾ヶ崎
  pref_mutual[pref_mutual$code == "111100360",]$pop + # 大字尾ヶ崎
  pref_pop_only[pref_pop_only$code == "111100700",]$pop # 美園東
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("111100360",
                            "111100370",
                            "111100380",
                            "111100390")) %>%
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
  dplyr::filter(!code %in% c("111100360",
                             "111100370",
                             "111100380",
                             "111100390"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 川口市 藤倉など（町丁字コード1620の計）and 豊田本など（町丁字コード1630の計）to 大字藤倉 of `pref_geom_only`
geom_only_code <- c("112011080")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) # 大字藤倉
pref_geom_only_1[pref_geom_only_1$code == "112011080"]$pop <-
  pref_pop_only[pref_pop_only$code == "112011620",]$pop + # 町丁字コード1620の計
  pref_pop_only[pref_pop_only$code == "112011630",]$pop # 町丁字コード1630の計
# Add sub_code
pref_geom_only_1$sub_code = 1080
# bind it with. mutual data frame
pref_mutual <- rbind(pref_mutual, pref_geom_only_1)
# Then, combine 大字藤倉 with  大袋新田, 大袋, 山城, 増形, 豊田本, 小室, 野田, and 池辺
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("112011080",
                            "112011040",
                            "112011030",
                            "112011110",
                            "112011092",
                            "112011480",
                            "112010490",
                            "112010510",
                            "112011020")) %>%
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
# Data frame expect those six blocks
pref_mutual_without_new_address <- pref_mutual %>%
  dplyr::filter(!code %in% c("112011080",
                             "112011040",
                             "112011030",
                             "112011110",
                             "112011092",
                             "112011480",
                             "112010490",
                             "112010510",
                             "112011020"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 川口市 本前川 to 前川町, and merge three blocks (前川町, 大字安行領根岸, 大字伊刈) together.
# This is because 川口市 本前川 was newly created in 2016 by combining parts of those three blocks.
pref_mutual[pref_mutual$code == "112030260",]$pop <- # 前川町
  pref_mutual[pref_mutual$code == "112030260",]$pop + # 前川町
  pref_pop_only[pref_pop_only$code == "112031190",]$pop # 本前川
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("112030260",
                            "112030490",
                            "112030650")) %>%
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
  dplyr::filter(!code %in% c("112030260",
                             "112030490",
                             "112030650"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 加須市 三俣 to 上三俣, and merge three blocks (上三俣 下三俣 不動岡) together.
# This is because 加須市 三俣 was newly created in 2016 by combining parts of those three blocks.
pref_mutual[pref_mutual$code == "112100180",]$pop <- # 上三俣
  pref_mutual[pref_mutual$code == "112100180",]$pop + # 上三俣
  pref_pop_only[pref_pop_only$code == "112100191",]$pop # 三俣
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("112100180",
                            "112100190",
                            "112100090", # 不動岡 has two numbers
                            "112100100")) %>%
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
  dplyr::filter(!code %in% c("112100180",
                             "112100190",
                             "112100090", # 不動岡 has two numbers
                             "112100100"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 加須市 旗井(new) and 北下新井 (new) to 旗井, and merge three blocks (旗井 北下新井 琴寄) together.
# This is because 加須市 旗井(new) and 北下新井 (new) was newly created in 2016 by combining parts of those three blocks.
pref_mutual[pref_mutual$code == "112100900",]$pop <- # 旗井
  pref_mutual[pref_mutual$code == "112100900",]$pop + # 旗井
  pref_pop_only[pref_pop_only$code == "112100901",]$pop + # 旗井(new)
  pref_pop_only[pref_pop_only$code == "112101001",]$pop # 北下新井(new)
# Combine the three blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("112100900",
                            "112101000",
                            "112100990")) %>%
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
# Data frame expect those three blocks
pref_mutual_without_new_address <- pref_mutual %>%
  dplyr::filter(!code %in% c("112100900",
                             "112101000",
                             "112100990"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 東松山市 美原町 and 大字松山 (new) in to 大字松山 (old) and 市の川区画整理事業区域 of `pref_geom_only`
# This is because 東松山市 美原町 and 大字松山 (new) are newly created in 2018 by combining parts of following four blocks.
# (大字松山,  加美町, 大字市ノ川, and 市の川区画整理事業区域)
geom_only_code <- c("112120090", "112120580")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1[pref_geom_only_1$code == "112120090"]$pop <-
  pref_pop_only[pref_pop_only$code == "112120091",]$pop +
  pref_pop_only[pref_pop_only$code == "112120600",]$pop
# Add sub_code
pref_geom_only_1$sub_code = 90
# bind it with. mutual data frame
pref_mutual <- rbind(pref_mutual, pref_geom_only_1)
# Then, combine 大字松山 with  加美町 and 大字市ノ川
# Firstly, merge 大字松山 into 加美町
pref_mutual[pref_mutual$code == "112120070",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "112120070")$geometry,
               filter(pref_mutual, code == "112120090")$geometry)
# Combine with 大字市ノ川
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("112120070",
                            "112120130")) %>%
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
# Data frame expect those two blocks
pref_mutual_without_new_address <- pref_mutual %>%
  dplyr::filter(!code %in% c("112120070",
                             "112120130"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 戸田市 新越谷 to 七左町
pref_mutual[pref_mutual$code == "112220440",]$pop <- # 七左町
  pref_mutual[pref_mutual$code == "112220440",]$pop + # 七左町
  pref_pop_only[pref_pop_only$code == "112220860",]$pop # 新越谷

# Assign 戸田市 大字下笹目 to 笹目
pref_mutual[pref_mutual$code == "112240180",]$pop <- # 笹目
  pref_mutual[pref_mutual$code == "112240180",]$pop + # 笹目
  pref_pop_only[pref_pop_only$code == "112240190",]$pop # 大字下笹目

# Assign 入間市 狭山台 to 宮寺, and merge six blocks (宮寺 大字新久 大字狭山ケ原 大字狭山台 大字根岸 大字中神) together.
# This is because 入間市 狭山台 was newly created in 2018 by combining parts of those six blocks.
pref_mutual[pref_mutual$code == "112250320",]$pop <- # 宮寺
 pref_mutual[pref_mutual$code == "112250320",]$pop + # 宮寺
 pref_pop_only[pref_pop_only$code == "112250430",]$pop # 狭山台
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
 dplyr::filter(code %in% c("112250320",
                           "112250210",
                           "112250220",
                           "112250340",
                           "112250310",
                           "112250300")) %>%
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
 dplyr::filter(!code %in% c("112250320",
                            "112250210",
                            "112250220",
                            "112250340",
                            "112250310",
                            "112250300"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
 dplyr::bind_rows(pref_mutual_without_new_address)

# Assign 桶川市 下日出谷西 and 大字下日出谷 (new) to 大字下日出谷 (old)
geom_only_code <- c("112310130")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code)
pref_geom_only_1[pref_geom_only_1$code == "112310130"]$pop <-
  pref_pop_only[pref_pop_only$code == "112310131",]$pop +
  pref_pop_only[pref_pop_only$code == "112310132",]$pop
# Add sub_code
pref_geom_only_1$sub_code = 130
# bind it with. mutual data frame
pref_mutual <- rbind(pref_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 桶川市 坂田東, 坂田西, and 大字坂田 (new) to 大字坂田 (old)
geom_only_code <- c("112310140")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code)
pref_geom_only_1[pref_geom_only_1$code == "112310140"]$pop <-
  pref_pop_only[pref_pop_only$code == "112310141",]$pop +
  pref_pop_only[pref_pop_only$code == "112310142",]$pop +
  pref_pop_only[pref_pop_only$code == "112310143",]$pop
# Add sub_code
pref_geom_only_1$sub_code = 140
# bind it with. mutual data frame
pref_mutual <- rbind(pref_mutual, pref_geom_only_1)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# 北本市 下石戸 was newly created in 2016, by combining parts of 大字北本宿, 下石戸上, and 下石戸下,
# Then from those 3 old blocks, remaining part was assigned to 緑
# Assign 北本市 下石戸 to 大字北本宿 of `pop_geom_only`
geom_only_code <- c("112330220")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code)
pref_geom_only_1[pref_geom_only_1$code == "112330220"]$pop <-
  pref_pop_only[pref_pop_only$code == "112330250",]$pop
# Add sub_code
pref_geom_only_1$sub_code = 220
# bind it with. mutual data frame
pref_mutual <- rbind(pref_mutual, pref_geom_only_1)
# Then, combine 大字北本宿 with 下石戸上, 下石戸下, and 緑
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("112330220",
                            "112330180",
                            "112330190",
                            "112330150")) %>%
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
  dplyr::filter(!code %in% c("112330220",
                             "112330180",
                             "112330190",
                             "112330150"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 蓮田市 藤ノ木 to 大字黒浜
# This is because 蓮田市 藤ノ木 was newly created from parts of 大字黒浜
pref_mutual[pref_mutual$code == "112380120",]$pop <-
  pref_mutual[pref_mutual$code == "112380120",]$pop +
  pref_pop_only[pref_pop_only$code == "112380320",]$pop

# Assign 吉川市 大字会野谷 to 会野谷
# This is because 吉川市 大字会野谷 was grouped together with 会野谷 in 2015 census
pref_mutual[pref_mutual$code == "112430160",]$pop <-
  pref_mutual[pref_mutual$code == "112430160",]$pop +
  pref_pop_only[pref_pop_only$code == "112430161",]$pop

# Assign 宮代町 道佛 to 字道佛, and merge with 宮代 together.
# This is because 宮代町 道佛 was newly created in 2017 from parts of 字道佛.
# A part of remaining was assigned to 宮代
pref_mutual[pref_mutual$code == "114420110",]$pop <-
 pref_mutual[pref_mutual$code == "114420110",]$pop +
 pref_pop_only[pref_pop_only$code == "114420280",]$pop
# Combine the four blocks
pref_mutual_new_address <- pref_mutual %>%
 dplyr::filter(code %in% c("114420110",
                           "114420150")) %>%
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
 dplyr::filter(!code %in% c("114420110",
                            "114420150"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
 dplyr::bind_rows(pref_mutual_without_new_address)

### Assign remaining `pref_geom` into `pref_mutual` ###
# Assign さいたま市北区 土呂町 of `pref_geom_only` into さいたま市北区 土呂町 of `pref_mutual`
geom_only_code <- c("111020120")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 120
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "111020110",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "111020110")$geometry,
               filter(pref_geom_only_1, code == "111020120")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 日高市 大字旭ケ丘 of `pref_geom_only` into 日高市 大字旭ケ丘 of `pref_mutual`
geom_only_code <- c("112420230")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 230
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "112420360",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "112420360")$geometry,
               filter(pref_geom_only_1, code == "112420230")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

# Assign 吉川市 美南 of `pref_geom_only` into 吉川市 美南 of `pref_mutual`
geom_only_code <- c("112430710",
                    "112430720",
                    "112430730",
                    "112430740")
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% geom_only_code) %>%
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$sub_code = 710
# Group together with `pref_mutual`
pref_mutual[pref_mutual$code == "112430700",]$geometry <-
  sf::st_union(filter(pref_mutual, code == "112430700")$geometry,
               filter(pref_geom_only_1, code == "112430710")$geometry)
# remove the `geom_only_code` from the `geom_only`
pref_geom_only <- pref_geom_only %>%
  filter(code %in% geom_only_code == FALSE)

#############################
# Finalize pref object
pref <- pref_mutual %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code) %>%
    sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
sum(pref$pop)
