############# set up ###############
#-------------- functions set up ---------------#
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

######Create 3 blocks in Tama################
rural_block <- rural %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry)) #27 administrative units

#adjacency list
rural_block_adj <- redist::redist.adjacency(rural_block)

#block map
rural_block_map <- redist::redist_map(rural_block,
                                      ndists = 3,
                                      pop_tol= 0.10,
                                      total_pop = pop,
                                      adj = rural_block_adj)

# --------- SMC simulation ----------------#
rural_block_smc <- redist::redist_smc(rural_block_map,
                                      nsims = 25000,
                                      pop_temper = 0.05)
#save(list=ls(all=TRUE), file="13_smc_tokyo_block_rural.Rdata")

# --------- Diagnostics ----------------#
wgt_rural_block_smc <- simulation_weight_disparity_table(rural_block_smc)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_rural_block_smc <- cbind(n, wgt_rural_block_smc)
#wgt_rural_block_smc$n[which(wgt_rural_block_smc$max_to_min == min(wgt_rural_block_smc$max_to_min))]
#Maxmin 1.002011 #863  1675  2348  3144 ...
#redist::redist.plot.plans(rural_block_smc, draws = 863, geom = rural_block_map)

#######Clean data: 23-ku (7 blocks)###############
urban_block <- urban %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry)) #27 administrative units

#adjacency list
urban_block_adj <- redist::redist.adjacency(urban_block)

#ferries
ferries_urban_block <- add_ferries(urban_block)

#edit adjacency list
urban_block_adj <- geomander::add_edge(urban_block_adj, ferries_urban_block$V1, ferries_urban_block$V2)

urban_block_map <- redist::redist_map(urban_block,
                                      ndists = 7,
                                      pop_tol= 0.08,
                                      total_pop = pop,
                                      adj = urban_block_adj)

# --------- SMC simulation ----------------#
urban_block_smc <- redist::redist_smc(urban_block_map,
                                      nsims = 25000,
                                      pop_temper = 0.05)
#save(list=ls(all=TRUE), file="13_smc_tokyo_block_urban.Rdata")

# --------- Diagnostics ----------------#
wgt_urban_block_smc <- simulation_weight_disparity_table(urban_block_smc)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_urban_block_smc <- cbind(n, wgt_urban_block_smc)
#wgt_urban_block_smc$n[which(wgt_urban_block_smc$max_to_min == min(wgt_urban_block_smc$max_to_min))]
#Maxmin 1.114643 # 1    15    27 ...
#redist::redist.plot.plans(urban_block_smc, draws = 1, geom = urban_block_map)

##########Separate Urban Blocks################
urban_block_1 <- urban %>%
  filter(code %in% c("13102", "13108", "13123"))
urban_block_2 <- urban %>%
  filter(code %in% c("13118", "13121", "13122"))
urban_block_3 <- urban %>%
  filter(code %in% c("13107", "13106", "13105", "13116", "13117"))
urban_block_4 <- urban %>%
  filter(code %in% c("13109", "13110", "13111"))
urban_block_5 <- urban %>%
  filter(code %in% c("13113","13103", "13112", "13360", "13380", "13400", "13420"))
urban_block_6 <- urban %>%
  filter(code %in% c("13101", "13104", "13114", "13115"))
urban_block_7 <- urban %>%
  filter(code %in% c("13119", "13120"))

redist.plot.map(shp = urban_block_5) + theme_map()

##########Separate Rural Blocks################
rural_block_1 <- rural %>%
  filter(code %in% c("13221", "13213", "13220",
                     "13222", "13229", "13211",
                     "13203", "13210", "13214",
                     "13204"))
rural_block_2 <- rural %>%
  filter(code %in% c("13209", "13224", "13201", "13212", "13228"))
rural_block_3 <- rural %>%
  filter(code %in% c("13221", "13213", "13220",
                     "13222", "13229", "13211",
                     "13203", "13210", "13214",
                     "13204",
                     "13209", "13224", "13201", "13212", "13228")  == FALSE)


########Create Map Objects##############
urban_block_1_adj <- redist::redist.adjacency(urban_block_1)
#manually edit adjacency list
#[33] 13102 0340 中央区佃
#[34] 13102 0350 中央区月島
#[35] 13102 0360 中央区勝どき
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 33, 11) #[11] 13102 110 中央区新川
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 33, 7) #[7] 13102 70 中央区明石町
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 33, 34)
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 34, 35)
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 35, 8)  #[8] 13102 0080 中央区築地
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 35, 36) #[36] 13102 0370 中央区豊海町
#[37] 13102 0380 中央区晴海
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 37, 34)
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 37, 35)
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 38, 58) #[58] 13108 210 江東区　豊洲

urban_block_1_map <- redist::redist_map(urban_block_1,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_1_adj)

urban_block_2_adj <- redist::redist.adjacency(urban_block_2)
urban_block_2_map <- redist::redist_map(urban_block_2,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_2_adj)

urban_block_3_adj <- redist::redist.adjacency(urban_block_3)
urban_block_3_map <- redist::redist_map(urban_block_3,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_3_adj)

urban_block_4_adj <- redist::redist.adjacency(urban_block_4)
#[ 25 ]13109 0250 品川区八潮
#[ 27 ]13109 0250 品川区東八潮
#[ 112 ]13111 0580 大田区東海
#[ 113 ]13111 0590 大田区城南島
urban_block_4_adj <- geomander::add_edge(urban_block_4_adj,  25 ,  27 )
urban_block_4_adj <- geomander::add_edge(urban_block_4_adj,  25 ,  18 ) #[ 18 ]品川区東品川180
urban_block_4_adj <- geomander::add_edge(urban_block_4_adj,  25 ,  16 ) #[ 16 ]品川区東大井160
urban_block_4_adj <- geomander::add_edge(urban_block_4_adj,  25 ,  4 ) #[ 4 ]品川区勝島40
urban_block_4_adj <- geomander::add_edge(urban_block_4_adj,  112 , 113 )
urban_block_4_adj <- geomander::add_edge(urban_block_4_adj,  112 , 63) #[63] 13111 0090大田区平和島

urban_block_4_map <- redist::redist_map(urban_block_4,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_4_adj)

urban_block_5_adj <- redist::redist.adjacency(urban_block_5)
urban_block_5_ferries <- add_ferries(urban_block_5)
urban_block_5_adj <- geomander::add_edge(urban_block_5_adj,
                                         urban_block_5_ferries$V1, urban_block_5_ferries$V2)
#[30]13103 0300港区台場
urban_block_5_adj <- geomander::add_edge(urban_block_5_adj, 30 , 2) #[2]13103 020港区海岸
urban_block_5_map <- redist::redist_map(urban_block_5,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_5_adj)

urban_block_6_adj <- redist::redist.adjacency(urban_block_6)
urban_block_6_map <- redist::redist_map(urban_block_6,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_6_adj)

urban_block_7_adj <- redist::redist.adjacency(urban_block_7)
#[99]13120 0420 練馬区西大泉町
urban_block_7_adj <- geomander::add_edge(urban_block_7_adj, 99, 100) #connect to [100]練馬区西大泉(６丁目) 0430
urban_block_7_map <- redist::redist_map(urban_block_7,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_7_adj)

rural_block_1_adj <- redist::redist.adjacency(rural_block_1)
rural_block_1_map <- redist::redist_map(rural_block_1,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = rural_block_1_adj)

rural_block_2_adj <- redist::redist.adjacency(rural_block_2)
rural_block_2_map <- redist::redist_map(rural_block_2,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = rural_block_2_adj)

rural_block_3_adj <- redist::redist.adjacency(rural_block_3)
rural_block_3_map <- redist::redist_map(rural_block_3,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = rural_block_3_adj)

#save(list=ls(all=TRUE), file="13_smc_tokyo_block_separated_maps.Rdata")




