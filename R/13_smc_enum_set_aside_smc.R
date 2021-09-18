############# set up ###############
library(tidyverse)
set.seed(12345)

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- information set-up -----------#
sim_type <- "smc"
nsims <- 25000
pref_code <- 13
pref_name <- "tokyo"
ndists_new <- 30
ndists_old <- 25

######### Download and Clean Data ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code)
# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

#########Clean data: pref################
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
#Estimate 2020 pop.
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#######Merge Gun#########
#merge nishitama
nishitama <- pref %>%
  dplyr::filter(code %in% c("13303", "13305", "13307", "13308"))
nishitama$code <- 13300
nishitama <- nishitama %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
nishitama$subcode <- "0000"

#merge oshima
oshima <- pref %>%
  dplyr::filter(code %in% c("13361", "13362", "13363", "13364"))
oshima$code <- 13360
oshima <- oshima %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
oshima$subcode <- "0000"

#merge miyake
miyake <- pref %>%
  dplyr::filter(code %in% c("13381", "13382"))
miyake$code <- 13380
miyake <- miyake %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
miyake$subcode <- "0000"

#merge hachijyo
hachijyo <- pref %>%
  dplyr::filter(code %in% c("13401", "13402"))
hachijyo$code <- 13400
hachijyo <- hachijyo %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
hachijyo$subcode <- "0000"

#merge ogasawara
ogasawara <- pref %>%
  dplyr::filter(code == "13421")
ogasawara$code <- 13420
ogasawara <- ogasawara %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
ogasawara$subcode <- "0000"

#merge back together
pref_non_gun <- pref %>%
  dplyr::filter(code %in% c("13303", "13305", "13307", "13308",
                            "13361", "13362", "13363", "13364",
                            "13381", "13382",
                            "13401", "13402",
                            "13421") == FALSE)
#now pref is an object with 0 gun splits
pref <- dplyr::bind_rows(pref_non_gun, nishitama, oshima,
                         miyake, hachijyo, ogasawara)

#######Separat Tama from 23ku###############
rural <-  pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115",
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420") == FALSE)

urban <- pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115", #with Setagaya
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420")) #Islands are considered connected to Minato-ku

#save(list=ls(all=TRUE), file="13_smc_tokyo_basic_data_by_region.Rdata")
