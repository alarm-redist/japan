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

#######Create 7 blocks in 23-ku###############
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
# --------- Urban Block no. 1 ----------------#
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

# --------- Urban Block no. 2 ----------------#
urban_block_2_adj <- redist::redist.adjacency(urban_block_2)
urban_block_2_map <- redist::redist_map(urban_block_2,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_2_adj)

# --------- Urban Block no. 3 ----------------#
urban_block_3_adj <- redist::redist.adjacency(urban_block_3)
urban_block_3_map <- redist::redist_map(urban_block_3,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_3_adj)

# --------- Urban Block no. 4 ----------------#
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

# --------- Urban Block no. 5 ----------------#
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

# --------- Urban Block no. 6 ----------------#
urban_block_6_adj <- redist::redist.adjacency(urban_block_6)
urban_block_6_map <- redist::redist_map(urban_block_6,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_6_adj)

# --------- Urban Block no. 7 ----------------#
urban_block_7_adj <- redist::redist.adjacency(urban_block_7)
#[99]13120 0420 練馬区西大泉町
urban_block_7_adj <- geomander::add_edge(urban_block_7_adj, 99, 100) #connect to [100]練馬区西大泉(６丁目) 0430
urban_block_7_map <- redist::redist_map(urban_block_7,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = urban_block_7_adj)

# --------- Rural Block no. 1 ----------------#
rural_block_1_adj <- redist::redist.adjacency(rural_block_1)
rural_block_1_map <- redist::redist_map(rural_block_1,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = rural_block_1_adj)

# --------- Rural Block no. 2 ----------------#
rural_block_2_adj <- redist::redist.adjacency(rural_block_2)
rural_block_2_map <- redist::redist_map(rural_block_2,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = rural_block_2_adj)

# --------- Rural Block no. 3 ----------------#
rural_block_3_adj <- redist::redist.adjacency(rural_block_3)
rural_block_3_map <- redist::redist_map(rural_block_3,
                                        ndists = 3,
                                        pop_tol= 0.08,
                                        total_pop = pop,
                                        adj = rural_block_3_adj)
#save(list=ls(all=TRUE), file="13_smc_tokyo_block_separated_maps.Rdata")

########Check Results#############
# --------- Urban Block no. 1 ----------------#
plan_urban_block_1 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Urban/Block1/13_urban_Block1_25000.Rds")
#Optimal Plan: 18524; 1.000162; 2; 2
redist::redist.plot.plans(plan_urban_block_1, draws = 18524, geom = urban_block_1_map)

#get data on optimal plan
matrix_plan_urban_block_1 <- redist::get_plans_matrix(plan_urban_block_1 %>% filter(draw == 18524))
colnames(matrix_plan_urban_block_1) <- "district"
optimal_boundary_1 <- cbind(urban_block_1, as_tibble(matrix_plan_urban_block_1))

#get data on municipality boundary
block_1_boundaries <- urban_block_1 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_1, aes(fill = factor(district))) +
  geom_sf(data = block_1_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Urban Block no. 2 ----------------#
plan_urban_block_2 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Urban/Block2/13_urban_Block2_25000.Rds")
#Optimal Plan: 8527; 1.000769; 1; 1
redist::redist.plot.plans(plan_urban_block_2, draws = 8527, geom = urban_block_2_map)

#get data on optimal plan
matrix_plan_urban_block_2 <- redist::get_plans_matrix(plan_urban_block_2 %>% filter(draw == 8527))
colnames(matrix_plan_urban_block_2) <- "district"
optimal_boundary_2 <- cbind(urban_block_2, as_tibble(matrix_plan_urban_block_2))

#get data on municipality boundary
block_2_boundaries <- urban_block_2 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_2, aes(fill = factor(district))) +
  geom_sf(data = block_2_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Urban Block no. 3 ----------------#
plan_urban_block_3 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Urban/Block3/13_urban_Block3_25000.Rds")
#Optimal Plan: 7633; 1.002268; 2; 2
redist::redist.plot.plans(plan_urban_block_3, draws = 7633, geom = urban_block_3_map)

#get data on optimal plan
matrix_plan_urban_block_3 <- redist::get_plans_matrix(plan_urban_block_3 %>% filter(draw == 7633))
colnames(matrix_plan_urban_block_3) <- "district"
optimal_boundary_3 <- cbind(urban_block_3, as_tibble(matrix_plan_urban_block_3))

#get data on municipality boundary
block_3_boundaries <- urban_block_3 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_3, aes(fill = factor(district))) +
  geom_sf(data = block_3_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Urban Block no. 4 ----------------#
plan_urban_block_4 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Urban/Block4/13_urban_Block4_25000.Rds")
#Optimal Plan: 2896; 1.000208; 2; 2
redist::redist.plot.plans(plan_urban_block_4, draws = 2896, geom = urban_block_4_map)

#get data on optimal plan
matrix_plan_urban_block_4 <- redist::get_plans_matrix(plan_urban_block_4 %>% filter(draw == 2896))
colnames(matrix_plan_urban_block_4) <- "district"
optimal_boundary_4 <- cbind(urban_block_4, as_tibble(matrix_plan_urban_block_4))

#get data on municipality boundary
block_4_boundaries <- urban_block_4 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_4, aes(fill = factor(district))) +
  geom_sf(data = block_4_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Urban Block no. 5 ----------------#
plan_urban_block_5 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Urban/Block5/13_urban_Block5_25000.Rds")
#Optimal Plan 8748; 1.002717; 2; 2
redist::redist.plot.plans(plan_urban_block_5, draws = 8748, geom = urban_block_5_map)

#get data on optimal plan
matrix_plan_urban_block_5 <- redist::get_plans_matrix(plan_urban_block_5 %>% filter(draw == 8748))
colnames(matrix_plan_urban_block_5) <- "district"
optimal_boundary_5 <- cbind(urban_block_5, as_tibble(matrix_plan_urban_block_5))

#get data on municipality boundary
block_5_boundaries <- urban_block_5 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_5, aes(fill = factor(district))) +
  geom_sf(data = block_5_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Urban Block no. 6 ----------------#
plan_urban_block_6 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Urban/Block6/13_urban_Block6_25000.Rds")
#Optimal Plan 11179; 1.001117; 2; 2
redist::redist.plot.plans(plan_urban_block_6, draws = 11179, geom = urban_block_6_map)

#get data on optimal plan
matrix_plan_urban_block_6 <- redist::get_plans_matrix(plan_urban_block_6 %>% filter(draw == 11179))
colnames(matrix_plan_urban_block_6) <- "district"
optimal_boundary_6 <- cbind(urban_block_6, as_tibble(matrix_plan_urban_block_6))

#get data on municipality boundary
block_6_boundaries <- urban_block_6 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_6, aes(fill = factor(district))) +
  geom_sf(data = block_6_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Urban Block no. 7 ----------------#
plan_urban_block_7 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Urban/Block7/13_urban_Block7_25000.Rds")
#Optimal Plan 11755; 1.001735; 2; 2
redist::redist.plot.plans(plan_urban_block_7, draws = 11755, geom = urban_block_7_map)

#get data on optimal plan
matrix_plan_urban_block_7 <- redist::get_plans_matrix(plan_urban_block_7 %>% filter(draw == 11755))
colnames(matrix_plan_urban_block_7) <- "district"
optimal_boundary_7 <- cbind(urban_block_7, as_tibble(matrix_plan_urban_block_7))

#get data on municipality boundary
block_7_boundaries <- urban_block_7 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_7, aes(fill = factor(district))) +
  geom_sf(data = block_7_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Rural Block no. 1 ----------------#
plan_rural_block_1 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Rural/Block1/13_rural_Block1_25000.Rds")
#Optimal Plan 15671; 1.000989; 2; 2
redist::redist.plot.plans(plan_rural_block_1, draws = 15671, geom = rural_block_1_map)

#get data on optimal plan
matrix_plan_rural_block_1 <- redist::get_plans_matrix(plan_rural_block_1 %>% filter(draw == 15671))
colnames(matrix_plan_rural_block_1) <- "district"
optimal_boundary_r1 <- cbind(rural_block_1, as_tibble(matrix_plan_rural_block_1))

#get data on municipality boundary
block_r1_boundaries <- rural_block_1 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_r1, aes(fill = factor(district))) +
  geom_sf(data = block_r1_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Rural Block no. 2 ----------------#
plan_rural_block_2 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Rural/Block2/13_rural_Block2_25000.Rds")
#Optimal Plan 12601; 1.000366; 2; 2
redist::redist.plot.plans(plan_rural_block_2, draws = 12601, geom = rural_block_2_map)

#get data on optimal plan
matrix_plan_rural_block_2 <- redist::get_plans_matrix(plan_rural_block_2 %>% filter(draw == 12601))
colnames(matrix_plan_rural_block_2) <- "district"
optimal_boundary_r2 <- cbind(rural_block_2, as_tibble(matrix_plan_rural_block_2))

#get data on municipality boundary
block_r2_boundaries <- rural_block_2 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_r2, aes(fill = factor(district))) +
  geom_sf(data = block_r2_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

# --------- Rural Block no. 1 ----------------#
plan_rural_block_3 <- readRDS("~/Desktop/ALARM Project/Tokyo Results/ByRegionxBlocks/Rural/Block3/13_rural_Block3_25000.Rds")
#Optimal Plan 468; 1.001393; 2; 2
redist::redist.plot.plans(plan_rural_block_3, draws = 468, geom = rural_block_3_map)

#get data on optimal plan
matrix_plan_rural_block_3 <- redist::get_plans_matrix(plan_rural_block_3 %>% filter(draw == 468))
colnames(matrix_plan_rural_block_3) <- "district"
optimal_boundary_r3 <- cbind(rural_block_3, as_tibble(matrix_plan_rural_block_3))

#get data on municipality boundary
block_r3_boundaries <- rural_block_3 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_r3, aes(fill = factor(district))) +
  geom_sf(data = block_r3_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")

#########diagnostics###############
wgt_smc_urban <- simulation_weight_disparity_table(urban_smc)
m <- c(1:25000)
wgt_smc_urban <- cbind(m, wgt_smc_urban)
#minimum max:min ratio
min(wgt_smc_urban$max_to_min)
#code for optimal plan
minimum_maxmin <- wgt_smc_urban$m[which(wgt_smc_urban$max_to_min == min(wgt_smc_urban$max_to_min))][1]

plans_pref_urban <- redist::get_plans_matrix(urban_smc)
#count the number of splits
splits_urban <- count_splits(plans_pref_urban, rural_block_3_map$code)
splits_urban[minimum_maxmin]

#count the number of municipalities that are split
csplits_urban <- redist::redist.splits(plans_pref_urban, rural_block_3_map$code)
csplits_urban[minimum_maxmin]

#minimum number of splits; county splits
min(splits_urban)
max(csplits_urban)
min(csplits_urban)

results_urban <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_urban)))
results_urban$max_to_min <- wgt_smc_urban$max_to_min
results_urban$splits <- splits_urban
results_urban$counties_split <- csplits_urban
results_urban$index <- 1:nrow(wgt_smc_urban)
results_urban$dif <-  results_urban$splits - results_urban$counties_split
min(results_urban$dif)
View(results_urban)


min(results_urban$max_to_min[which(results_urban$splits == results_urban$counties_split)])
results_urban$index[which(results_urban$splits == results_urban$counties_split)]



