###############################################################################
# Download and prepare data for `27_osaka` analysis
# © ALARM Project, June 2021
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
nsims <- 5000 # Set so that the number of valid plans > 5,000
pref_code <- 27
pref_name <- "osaka"
lakes_removed <- c()
ndists_new <- 19
ndists_old <- 19
sq_max_to_min <- 1.530
sq_max_to_tottori2 <- 2.005
sq_mun_splits <- 0
sq_gun_splits <- 0
sq_koiki_splits <- 0
pop_tol <- 0.35

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

# Assign 岸和田市尾生町(sub_code: 1321, 1322) to 岸和田市尾生町(sub_code: 1250, 1320)
pref_geom_only_1 <- pref_geom_only %>%
  filter(code %in% c("272021250", "272021320")) %>% #岸和田市尾生町(sub_code: 1250, 1320)
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_1$pop <-
  pref_pop_only[pref_pop_only$code == "272021321",]$pop + #尾生町(sub_code: 1321)
  pref_pop_only[pref_pop_only$code == "272021322",]$pop #尾生町(sub_code: 1322)
# Add sub_code
pref_geom_only_1$sub_code = 1321

# Assign 岸和田市作才町(sub_code: 1331, 1332) to 岸和田市作才町(sub_code: 430, 1330)
pref_geom_only_2 <- pref_geom_only %>%
  filter(code %in% c("272020430", "272021330")) %>% #岸和田市作才町(sub_code: 430, 1330)
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_2$pop <-
  pref_pop_only[pref_pop_only$code == "272021331",]$pop + #作才町(sub_code: 1331)
  pref_pop_only[pref_pop_only$code == "272021332",]$pop #作才町(sub_code: 1332)
# Add sub_code
pref_geom_only_2$sub_code = 1331

# Assign 豊中市東豊中町(sub_code: 281, 282) to 豊中市東豊中町(sub_code: 280)
pref_geom_only_3 <- filter(pref_geom_only, code == "272030280")  #豊中市東豊中町(sub_code: 280)
pref_geom_only_3$pop <-
  pref_pop_only[pref_pop_only$code == "272030281",]$pop + #東豊中町(sub_code: 281)
  pref_pop_only[pref_pop_only$code == "272030282",]$pop  #東豊中町(sub_code: 282)
# Add sub_code
pref_geom_only_3$sub_code = 281

# Assign 豊中市勝部(sub_code: 491, 492) to 豊中市勝部 (sub_code: 490)
pref_geom_only_4 <- filter(pref_geom_only, code == "272030490")  #豊中市東豊中町 (sub_code: 490)
pref_geom_only_4$pop <-
  pref_pop_only[pref_pop_only$code == "272030491",]$pop + #東豊中町 (sub_code: 491)
  pref_pop_only[pref_pop_only$code == "272030492",]$pop  #東豊中町 (sub_code: 492)
# Add sub_code
pref_geom_only_4$sub_code = 491

# Assign 豊中市上新田(sub_code: 1051, 1052) to 豊中市上新田(sub_code: 1050)
pref_geom_only_5 <- filter(pref_geom_only, code == "272031050")  # 豊中市上新田(sub_code: 1050)
pref_geom_only_5$pop <-
  pref_pop_only[pref_pop_only$code == "272031051",]$pop + # 豊中市上新田(sub_code: 1051)
  pref_pop_only[pref_pop_only$code == "272031052",]$pop  # 豊中市上新田(sub_code: 1052)
# Add sub_code
pref_geom_only_5$sub_code = 1051

# Assign 高槻市日吉台一番町~七番町 to 高槻市日吉台
pref_geom_only_6 <- filter(pref_geom_only, code == "272071720")  # 高槻市日吉台
pref_geom_only_6$pop <-
  pref_pop_only[pref_pop_only$code == "272072130",]$pop + # 高槻市日吉台一番町
  pref_pop_only[pref_pop_only$code == "272072140",]$pop + # 高槻市日吉台二番町
  pref_pop_only[pref_pop_only$code == "272072150",]$pop + # 高槻市日吉台三番町
  pref_pop_only[pref_pop_only$code == "272072160",]$pop + # 高槻市日吉台四番町
  pref_pop_only[pref_pop_only$code == "272072170",]$pop + # 高槻市日吉台五番町
  pref_pop_only[pref_pop_only$code == "272072180",]$pop + # 高槻市日吉台六番町
  pref_pop_only[pref_pop_only$code == "272072190",]$pop  # 高槻市日吉台七番町
# Add sub_code
pref_geom_only_6$sub_code = 2130

# Assign 河内長野市原町(sub_code: 821, 822) to 河内長野市原町(sub_code: 440, 820)
pref_geom_only_7 <- pref_geom_only %>%
  filter(code %in% c("272160440", "272160820")) %>% #河内長野市原町(sub_code: 440, 820)
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_7$pop <-
  pref_pop_only[pref_pop_only$code == "272160821",]$pop + #岸和田市作才町(sub_code: 821)
  pref_pop_only[pref_pop_only$code == "272160822",]$pop #岸和田市作才町(sub_code: 822)
# Add sub_code
pref_geom_only_7$sub_code = 821

# Assign 箕面市下止々呂美 to 箕面市下止々呂美
pref_geom_only_8 <- filter(pref_geom_only, code == "272200280")  #箕面市下止々呂美
pref_geom_only_8$pop <-
  pref_pop_only[pref_pop_only$code == "272200281",]$pop  #箕面市下止々呂美
# Add sub_code
pref_geom_only_8$sub_code = 281

# Assign 羽曳野市古市(sub_code:114)&南古市(sub_code:751,752) to 羽曳野市古市(sub_code:113)&南古市(sub_code:112)
pref_geom_only_9 <- pref_geom_only %>%
  filter(code %in% c("272220112", "272220113")) %>% #羽曳野市古市(sub_code:113)&南古市(sub_code:112)
  group_by(mun_code) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  slice(1)
pref_geom_only_9$pop <-
  pref_pop_only[pref_pop_only$code == "272220114",]$pop + #羽曳野市古市(sub_code:114)
  pref_pop_only[pref_pop_only$code == "272220751",]$pop + #羽曳野市南古市(sub_code:751)
  pref_pop_only[pref_pop_only$code == "272220752",]$pop  #羽曳野市南古市(sub_code:752)
# Add sub_code
pref_geom_only_9$sub_code = 114

# Assign 交野市寺(sub_code: 1141, 1142) to 交野市寺(sub_code: 1140)
pref_geom_only_10 <- filter(pref_geom_only, code == "272301140")  #交野市寺(sub_code: 1140)
pref_geom_only_10$pop <-
  pref_pop_only[pref_pop_only$code == "272301141",]$pop +  #交野市寺(sub_code: 1141)
  pref_pop_only[pref_pop_only$code == "272301142",]$pop  #交野市寺(sub_code: 1142)
# Add sub_code
pref_geom_only_10$sub_code = 1141

# Assign 茨木市大字清水: pop 0
pref_geom_only_11 <- filter(pref_geom_only, code == "272111670")  #茨木市大字清

# Assign 岸和田市行遇町(sub_code:1360) to 岸和田市極楽寺町・行遇町（sub_code:451）
pref_mutual[pref_mutual$code == "272020451",]$pop <- #岸和田市極楽寺町・行遇町（sub_code:451）
  pref_mutual[pref_mutual$code == "272020451",]$pop + #岸和田市極楽寺町・行遇町（sub_code:451）
  pref_pop_only[pref_pop_only$code == "272021360",]$pop #岸和田市行遇町(sub_code:1360)

# Assign 枚方市楠葉中之芝(sub_code:1601, 1602) to 枚方市楠葉中之芝(sub_code:1600)
pref_mutual[pref_mutual$code == "272101600",]$pop <- #枚方市楠葉中之芝(sub_code:1600)
  pref_mutual[pref_mutual$code == "272101600",]$pop + #枚方市楠葉中之芝(sub_code:1600)
  pref_pop_only[pref_pop_only$code == "272101601",]$pop + #枚方市楠葉中之芝(sub_code:1601)
  pref_pop_only[pref_pop_only$code == "272101602",]$pop #枚方市楠葉中之芝(sub_code:1602)

# Assign 八尾市大字楽音寺 to 八尾市楽音寺
pref_mutual[pref_mutual$code == "272121490",]$pop <- #八尾市楽音寺
  pref_mutual[pref_mutual$code == "272121490",]$pop + #八尾市楽音寺
  pref_pop_only[pref_pop_only$code == "272120320",]$pop #八尾市大字楽音寺

# Assign 門真市大字野口他 to 門真市大字横地
pref_mutual[pref_mutual$code == "272230180",]$pop <- #門真市大字横地
  pref_mutual[pref_mutual$code == "272230180",]$pop + #門真市大字横地
  pref_pop_only[pref_pop_only$code == "272230182",]$pop #門真市大字野口他

# Merge 岸和田市稲葉町 and 三ケ山町, then assign the population of 岸の丘町
# 岸和田市岸の丘町 was created by combining parts of 岸和田市稲葉町 and 三ケ山町
pref_mutual_new_address <- pref_mutual %>%
  dplyr::filter(code %in% c("272021150",
                            "272021260")) %>%
  dplyr::summarise(code = first(code),
                   mun_code = first(mun_code),
                   sub_code = first(sub_code),
                   sub_name = "combined",
                   pop = sum(pop) +
                     pref_pop_only[pref_pop_only$code == "272021350",]$pop,
                   CITY_NAME = first(CITY_NAME),
                   S_NAME = first(S_NAME),
                   KIHON1 = first(KIHON1),
                   JINKO = sum(JINKO),
                   geometry = sf::st_union(geometry))
# Create data frame that excludes these areas
pref_mutual_without_new_address <- pref_mutual %>%
  dplyr::filter(!code %in% c("272021150",
                             "272021260"))
# Combine them together
pref_mutual <- pref_mutual_new_address %>%
  dplyr::bind_rows(pref_mutual_without_new_address)

# Merge 岸和田市上松町, 下松町, and 額原町, then assign the population of 上松町&下松町
pref_mutual[pref_mutual$code == "272020950",]$geometry <-  #岸和田市額原町
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272020950",]),#岸和田市額原町
      pref_geom_only[pref_geom_only$code == "272021240",],#岸和田市上松町
      pref_geom_only[pref_geom_only$code == "272021232",] #岸和田市下松町
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

pref_mutual[pref_mutual$code == "272020950",]$pop <- #岸和田市額原町
  pref_mutual[pref_mutual$code == "272020950",]$pop + #岸和田市額原町
  pref_pop_only[pref_pop_only$code == "272021233",]$pop + #岸和田市下松町(sub_code:1233)
  pref_pop_only[pref_pop_only$code == "272021234",]$pop + #岸和田市下松町(sub_code:1234)
  pref_pop_only[pref_pop_only$code == "272021241",]$pop + #岸和田市上松町(sub_code:1241)
  pref_pop_only[pref_pop_only$code == "272021242",]$pop  #岸和田市下松町(sub_code:1242)

# Merge 岸和田市土生町, 極楽寺町, 八田町, and 畑町(sub_code:440, 1310)
# then assign the population of 岸和田市畑町(sub_code:1311, 1312)
pref_mutual[pref_mutual$code == "272020411",]$geometry <-  #岸和田市土生町(sub_code:411)
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272020411",]), #岸和田市土生町(sub_code:411)
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272020412",]), #岸和田市土生町(sub_code:412)
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272020451",]), #岸和田市極楽寺町(sub_code:451)
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272020452",]), #岸和田市極楽寺町(sub_code:452)
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272020510",]), #岸和田市八田町
      pref_geom_only[pref_geom_only$code == "272020440",],#岸和田市畑町(sub_code:440）
      pref_geom_only[pref_geom_only$code == "272021310",] #岸和田市畑町(sub_code:1310)
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

pref_mutual[pref_mutual$code == "272020411",]$pop <- #岸和田市土生町(sub_code:411)
  pref_mutual[pref_mutual$code == "272020411",]$pop + #岸和田市土生町(sub_code:411)
  pref_mutual[pref_mutual$code == "272020412",]$pop + #岸和田市土生町(sub_code:412)
  pref_mutual[pref_mutual$code == "272020451",]$pop + #岸和田市極楽寺町(sub_code:451)
  pref_mutual[pref_mutual$code == "272020452",]$pop + #岸和田市極楽寺町(sub_code:452)
  pref_mutual[pref_mutual$code == "272020510",]$pop + #岸和田市八田町
  pref_pop_only[pref_pop_only$code == "272021311",]$pop + #岸和田市畑町(sub_code:1311）
  pref_pop_only[pref_pop_only$code == "272021312",]$pop  #岸和田市畑町(sub_code:1312）

# remove duplicates
pref_mutual <- pref_mutual %>%
  dplyr::filter(!code %in% c("272020412",
                             "272020451",
                             "272020452",
                             "272020510"))

# Merge 八尾市大字刑部、大字都塚、大字東弓削、二俣、大字二俣
# then assign the population of 八尾市都塚北、都塚南
pref_mutual[pref_mutual$code == "272120180",]$geometry <-  #八尾市大字刑部
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272120180",]), #八尾市大字刑部
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272121440",]), #大字都塚
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272121430",]), #大字東弓削
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272121550",]), #二俣
      pref_geom_only[pref_geom_only$code == "272121040",]#大字二俣
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

pref_mutual[pref_mutual$code == "272120180",]$pop <-  #八尾市大字刑部
  pref_mutual[pref_mutual$code == "272120180",]$pop + #八尾市大字刑部
  pref_mutual[pref_mutual$code == "272121440",]$pop + #大字都塚
  pref_mutual[pref_mutual$code == "272121430",]$pop + #大字東弓削
  pref_mutual[pref_mutual$code == "272121550",]$pop + #二俣
  pref_pop_only[pref_pop_only$code == "272121630",]$pop + #都塚北
  pref_pop_only[pref_pop_only$code == "272121640",]$pop  #都塚南

# remove duplicates
pref_mutual <- pref_mutual %>%
  dplyr::filter(!code %in% c("272121440",
                             "272121430",
                             "272121550"))

# Merge 四條畷市大字中野、大字蔀屋 then assign the population of 四條畷市西中野
pref_mutual[pref_mutual$code == "272290130",]$geometry <-  #四條畷市大字中野
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272290130",]), #四條畷市大字中野
      pref_geom_only[pref_geom_only$code == "272290110",] #大字蔀屋
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

pref_mutual[pref_mutual$code == "272290130",]$pop <-  #四條畷市大字中野
  pref_mutual[pref_mutual$code == "272290130",]$pop + #四條畷市大字中野
  pref_pop_only[pref_pop_only$code == "272290310",]$pop  #西中野

# Merge together 東大阪市上石切町(sub_code: 1680, 1681, 1682)
pref_mutual[pref_mutual$code == "272271680",]$geometry <-  #東大阪市上石切町(sub_code: 1680)
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272271680",]),#東大阪市上石切町(sub_code: 1680)
      pref_geom_only[pref_geom_only$code == "272271681",],#上石切町(sub_code: 1681)
      pref_geom_only[pref_geom_only$code == "272271682",] #上石切町(sub_code: 1682)
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

# Merge together 泉南市信達大苗代(sub_code: 40, 210)
pref_mutual[pref_mutual$code == "272280210",]$geometry <-  #泉南市信達大苗代(sub_code: 210)
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272280210",]),#泉南市信達大苗代(sub_code: 210)
      pref_geom_only[pref_geom_only$code == "272280040",]#信達大苗代(sub_code: 40)
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

# Merge together 阪南市箱作(sub_code: 210, 230)
pref_mutual[pref_mutual$code == "272320210",]$geometry <-  #阪南市箱作(sub_code: 210)
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "272320210",]),#阪南市箱作(sub_code: 210)
      pref_geom_only[pref_geom_only$code == "272320230",] #箱作(sub_code: 230)
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

# Merge together 島本町東大寺(sub_code: 61, 60)
pref_mutual[pref_mutual$code == "273010061",]$geometry <-  #島本町東大寺(sub_code: 61)
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "273010061",]),#島本町東大寺(sub_code: 61)
      pref_geom_only[pref_geom_only$code == "273010060",] #東大寺(sub_code: 60)
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

# Merge together 河南町大宝(sub_code: 5010, 5020, 5030, 5040, 5050)
pref_mutual[pref_mutual$code == "273825010",]$geometry <-  #河南町東大寺(sub_code: 5010)
  sf::st_geometry(
    rbind(
      sf::st_as_sf(pref_mutual[pref_mutual$code == "273825010",]),#河南町大宝(sub_code: 5010)
      pref_geom_only[pref_geom_only$code == "273825020",], #大宝(sub_code: 5020)
      pref_geom_only[pref_geom_only$code == "273825030",], #大宝(sub_code: 5030)
      pref_geom_only[pref_geom_only$code == "273825040",], #大宝(sub_code: 5040)
      pref_geom_only[pref_geom_only$code == "273825050",]  #大宝(sub_code: 5050)
    ) %>%
      summarize(geometry = sf::st_union(geometry))
  )

# Finalize pref object
pref <- rbind(pref_mutual, pref_geom_only_1, pref_geom_only_2,
              pref_geom_only_3, pref_geom_only_4, pref_geom_only_5,
              pref_geom_only_6, pref_geom_only_7, pref_geom_only_8,
              pref_geom_only_9 ,pref_geom_only_10, pref_geom_only_11)
pref <- pref %>%
    select(mun_code, sub_code, pop, geometry) %>%
    rename(code = mun_code) %>%
    mutate(code = as.numeric(code)) %>%
    arrange(code) %>%
    sf::st_as_sf()

# Finally, confirm that these matching operations were conducted correctly
sum(pref$pop) == sum(pref_pop_2020$pop)
