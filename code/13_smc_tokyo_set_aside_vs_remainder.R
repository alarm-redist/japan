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

#############Urban Create 3 Blocks Through Enumeration#################
#group by municipality
urban_by_mun <- urban %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

#group islands together with Minatoku to make results of enumeration reasonable
minato_islands <- urban_by_mun %>%
  dplyr::filter(code %in% c(13103, 13360, 13380, 13400, 13420)) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
minato_islands$code <- 13103

#bind back together
urban_by_mun <-
  dplyr::bind_rows(urban_by_mun %>%
          dplyr::filter(code %in% c(13103, 13360, 13380, 13400, 13420) == FALSE),
          minato_islands)

#prepare for enumeration: adjacency list
urban_by_mun_edge_adj <- redist::redist.adjacency(urban_by_mun)

#prepare for enumeartion: define map
urban_group_map <- redist::redist_map(urban_by_mun,
                                      ndists = 3,
                                      pop_tol= 0.08,
                                      total_pop = pop,
                                      adj = urban_by_mun_edge_adj)

#enumeration set up
makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))
if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

#enumeration
urban_enum_plans <- redist::redist.enumpart(adj = urban_by_mun_edge_adj,
                                            unordered_path = here::here('simulation/unord_tokyo_urban'),
                                            ordered_path = here::here('simulation/ord_tokyo_urban'),
                                            out_path = here::here('simulation/enum_tokyo_urban'),
                                            ndists = 3,
                                            all = TRUE,
                                            total_pop = urban_group_map[[attr(urban_group_map, 'pop_col')]])
save(urban_enum_plans, file = "simulation/13_enum_tokyo_urban_muns.Rdata")

#check result of enumeration
urban_enum_plans_result <- redist::redist.read.enumpart(out_path = 'simulation/enum_tokyo_urban')
urban_enum_plans_result_matrix <- redist::redist_plans(urban_enum_plans_result,
                                                       urban_group_map,
                                                       algorithm = 'enumpart')

urban_enum_plans_result_matrix <- dplyr::as_tibble(urban_enum_plans_result_matrix)

save(urban_enum_plans_result_matrix, file = "urban_enum_plans_result_matrix.Rdata")

#cauculate target pop.
target <- round(sum(urban_by_mun$pop)/21)


#Calculate Score: 1.*target   2.round(*target) - *target   3. take absolute value
urban_enum_plans_result_matrix$score <- abs(urban_enum_plans_result_matrix$total_pop/target -
                                              round(urban_enum_plans_result_matrix$total_pop/target))

#filter out plans with low deviation from multiple of target pop.
good_split <-
  redist::redist_plans(urban_enum_plans_result,
                       urban_group_map,
                       algorithm = 'enumpart') %>%
  filter(draw %in%
           as.numeric(urban_enum_plans_result_matrix$draw[which(urban_enum_plans_result_matrix$score ==
                                                                  min(urban_enum_plans_result_matrix$score))]))

#choose best split
best_split <- as_tibble(good_split)
best_split$score <- abs(best_split$total_pop/target - round(best_split$total_pop/target))
#obtain dataframe that has one line per draw
best_split_plan <- as_tibble(best_split %>% dplyr::filter(district == 1))
best_split_plan$aggregate <- 0
#choose the plan whose total score is minimum
for(i in 1:as.numeric(length(unique(best_split$draw)))){
  best_split_plan$aggregate[i] <-
    best_split$score[(3*i-2)] + best_split$score[(3*i-1)]+  + best_split$score[(3*i)]
}

#get data on optimal plan
optimal_split <- good_split %>%
  dplyr::filter(draw == best_split_plan$draw[which(best_split_plan$aggregate ==
                                                    min(best_split_plan$aggregate))])
matrix_best_split <- redist::get_plans_matrix(optimal_split)
colnames(matrix_best_split) <- "block"

#bind together data on block with map
urban_by_mun_block <- as_tibble(cbind(urban_by_mun, as_tibble(matrix_best_split)))

save(urban_by_mun_block, file = "urban_by_mun_block.Rdata")

#allocate number of seats to each block
optimal_split$multiple <- round(optimal_split$total_pop/target)
optimal_split$target <- optimal_split$total_pop/optimal_split$multiple

save(optimal_split, file = "optimal_split.Rdata")

##########Visualize Best Block####################
urban_excluding_islands <- urban %>%
  filter(code %in% c(13360, 13380, 13400, 13420) == FALSE) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

#match with dataframe that shows the optimal way to break up Tokyo 23-ku into 3 blocks
urban_excluding_islands$block <- (urban_by_mun_block %>% arrange(code))$block

#map
ggplot() +
  geom_sf(data = urban_excluding_islands, aes(fill = factor(block))) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank())

#########Urban Block 1################
urban_block_1 <- urban %>%
  filter(code %in% c(urban_by_mun_block$code[which(urban_by_mun_block$block == 1)],
                     13360, 13380, 13400, 13420)) #add islands (connected to Minato-ku)

#adjacency list
urban_block_1_adj <- redist::redist.adjacency(urban_block_1)

#ferries
ferries_urban_block_1 <- add_ferries(urban_block_1)
#edit adjacency list
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj,
                                         ferries_urban_block_1$V1, ferries_urban_block_1$V2)

#[92] 13102 0340 中央区佃
#[93] 13102 0350 中央区月島
#[94] 13102 0360 中央区勝どき
#[95] 13102 0370 中央区豊海町
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 92, 70) #[70] 13102 110 中央区新川
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 92, 66) #[66] 13102 0070 中央区明石町
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 92, 93)
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 93, 94)
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 94, 67)  #[67] 13102 0080 中央区築地
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 94, 95)

#[96] 13102 0380 中央区晴海
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 96, 93)
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 96, 94)

#[126]13103 0300 港区台場
urban_block_1_adj <- geomander::add_edge(urban_block_1_adj, 126, 98) #[98]13103 0020港区海岸

#save(urban_block_1, file = "urban_block_1.Rdata")
#save(urban_block_1_adj, file = "urban_block_1_adj.Rdata")


#define map
urban_block_1_map <- redist::redist_map(urban_block_1,
                                        ndists = 8,
                                        pop_tol = 0.08,
                                        total_pop = pop,
                                        adj = urban_block_1_adj)

# --------- MS simulation ----------------#
#run simulation
urban_block_1_ms <- redist::redist_mergesplit(urban_block_1_map,
                                              nsims = 25000,
                                              counties = urban_block_1_map$code,
                                              constraints = list(multisplits = list(strength = 500),
                                                                 splits = list(strength = 300))
)

##########Diagnostics: Urban Block 1#############
wgt_urban_block_1 <- simulation_weight_disparity_table(urban_block_1_rs)
m <- c(1:12501)
wgt_urban_block_1 <- cbind(m, wgt_urban_block_1)
min(wgt_urban_block_1$max_to_min)

minimum_maxmin_urban_1 <-
  wgt_urban_block_1$m[which(wgt_urban_block_1$max_to_min == min(wgt_urban_block_1$max_to_min))][1]

plans_pref_urban_block_1 <- redist::get_plans_matrix(urban_block_1_rs)

#count the number of splits
splits_urban_block_1 <- count_splits(plans_pref_urban_block_1, urban_block_1_map$code)
splits_urban_block_1[minimum_maxmin_urban_1]

#count the number of municipalities that are split
csplits_urban_block_1 <- redist::redist.splits(plans_pref_urban_block_1, urban_block_1_map$code)
csplits_urban_block_1[minimum_maxmin_urban_1]

results_urban_1 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_urban_block_1)))
results_urban_1$max_to_min <- wgt_urban_block_1$max_to_min
results_urban_1$splits <- splits_urban_block_1
results_urban_1$counties_split <- csplits_urban_block_1
results_urban_1$index <- 1:nrow(wgt_urban_block_1)
results_urban_1$dif <-  results_urban_1$splits - results_urban_1$counties_split
min(results_urban_1$dif)
View(results_urban_1)

block1_optimal <- which(results_urban_1$max_to_min ==
                          min((results_urban_1 %>% filter(dif == 0))$max_to_min))[1]

#############Urban Block 1 Plot Plan################
#get data on optimal plan
matrix_plan_urban_block_1 <- redist::get_plans_matrix(urban_block_1_rs %>% filter(draw == block1_optimal))
colnames(matrix_plan_urban_block_1) <- "district"
matrix_plan_urban_block_1 <- head(matrix_plan_urban_block_1, 424) #filter out islands
optimal_boundary_1 <- cbind(head(urban_block_1, 424), as_tibble(matrix_plan_urban_block_1))

#get data on municipality boundary
urban_block_boundaries_1 <- urban_block_1 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
urban_block_boundaries_1 <- head(urban_block_boundaries_1, 10) #filter out islands

#get data on district boundary
urban_district_boundaries_1 <- optimal_boundary_1 %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_1, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_1, fill = NA, color = "black", lwd = 1.5) +
  geom_sf(data = urban_district_boundaries_1, fill = NA, color = "yellow", lwd = 0.15) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())


#########Urban Block 2################
urban_block_2 <- urban %>%
  filter(code %in% urban_by_mun_block$code[which(urban_by_mun_block$block == 2)])

#adjacency list
urban_block_2_adj <- redist::redist.adjacency(urban_block_2)

#[235]13120 0420 練馬区西大泉町
urban_block_2_adj <- geomander::add_edge(urban_block_2_adj, 235, 236) #connect to [236]練馬区西大泉(６丁目)

#save(urban_block_2, file = "urban_block_2.Rdata")
#save(urban_block_2_adj, file = "urban_block_2_adj.Rdata")

#define map
urban_block_2_map <- redist::redist_map(urban_block_2,
                                        ndists = 5,
                                        pop_tol = 0.07,
                                        total_pop = pop,
                                        adj = urban_block_2_adj)
# --------- MS simulation ----------------#
#run simulation
urban_block_2_ms <- redist::redist_mergesplit(urban_block_2_map,
                                              nsims = 25000,
                                              counties = urban_block_2_map$code,
                                              constraints = list(multisplits = list(strength = 500),
                                                                 splits = list(strength = 300))
)

##########Diagnostics: Urban Block 2#############
wgt_urban_block_2 <- simulation_weight_disparity_table(urban_block_2_rs)
m <- c(1:12501)
wgt_urban_block_2 <- cbind(m, wgt_urban_block_2)
min(wgt_urban_block_2$max_to_min)

minimum_maxmin_urban_2 <-
  wgt_urban_block_2$m[which(wgt_urban_block_2$max_to_min == min(wgt_urban_block_2$max_to_min))][1]

plans_pref_urban_block_2 <- redist::get_plans_matrix(urban_block_2_rs)

#count the number of splits
splits_urban_block_2 <- count_splits(plans_pref_urban_block_2, urban_block_2_map$code)
splits_urban_block_2[minimum_maxmin_urban_2]

#count the number of municipalities that are split
csplits_urban_block_2 <- redist::redist.splits(plans_pref_urban_block_2, urban_block_2_map$code)
csplits_urban_block_2[minimum_maxmin_urban_2]

results_urban_2 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_urban_block_2)))
results_urban_2$max_to_min <- wgt_urban_block_2$max_to_min
results_urban_2$splits <- splits_urban_block_2
results_urban_2$counties_split <- csplits_urban_block_2
results_urban_2$index <- 1:nrow(wgt_urban_block_2)
results_urban_2$dif <-  results_urban_2$splits - results_urban_2$counties_split
min(results_urban_2$dif)
View(results_urban_2)

block2_optimal <- which(results_urban_2$max_to_min ==
                          min((results_urban_2 %>% filter(dif == 0))$max_to_min))[1]

#############Urban Block 2 Plot Plan################
#get data on optimal plan
matrix_plan_urban_block_2 <- redist::get_plans_matrix(urban_block_2_rs %>% filter(draw == block2_optimal))
colnames(matrix_plan_urban_block_2) <- "district"
optimal_boundary_2 <- cbind(urban_block_2, as_tibble(matrix_plan_urban_block_2))

#get data on municipality boundary
urban_block_boundaries_2 <- urban_block_2 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#get data on district boundary
urban_district_boundaries_2 <- optimal_boundary_2 %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_2, aes(fill = factor(district))) +
  scale_fill_manual(values = c("green", "purple", "blue", "yellow", "brown")) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_2, fill = NA, color = "black", lwd = 2.5) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

#################Urban Block 3#################
urban_block_3 <- urban %>%
  filter(code %in% c(urban_by_mun_block$code[which(urban_by_mun_block$block == 3)]))

#adjacency list
urban_block_3_adj <- redist::redist.adjacency(urban_block_3)


#manually edit adjacency list
#edit adjacency list
#[70]13109 0250 品川区八潮
urban_block_3_adj <- geomander::add_edge(urban_block_3_adj, 70, 63) #[63]品川区東品川180
urban_block_3_adj <- geomander::add_edge(urban_block_3_adj, 70, 61) #[61]品川区東大井160
urban_block_3_adj <- geomander::add_edge(urban_block_3_adj, 70, 49) #[49]品川区勝島40

#[72]13109 0270 品川区東八潮
urban_block_3_adj <- geomander::add_edge(urban_block_3_adj, 72, 70) #[70]13109 0250 品川区八潮

#[157]13111 0580 大田区東海 -- currently connected to only [70]13109 0250 品川区八潮 and [158]13111 0590大田区城南島
urban_block_3_adj <- geomander::add_edge(urban_block_3_adj, 157, 108)　#[108]13111 0090 大田区平和島 107 109
urban_block_3_adj <- geomander::add_edge(urban_block_3_adj, 157, 159)   #[159]13111 0600大田区京浜島


#save(urban_block_3, file = "urban_block_3.Rdata")
#save(urban_block_3_adj, file = "urban_block_3_adj.Rdata")

urban_block_3_map <- redist::redist_map(urban_block_3,
                                        ndists = 8,
                                        pop_tol = 0.075,
                                        total_pop = pop,
                                        adj = urban_block_3_adj)

# --------- SMC simulation ----------------#
#run simulation
urban_block_3_ms <- redist::redist_mergesplit(urban_block_3_map,
                                              nsims = 25000,
                                              counties = urban_block_3_map$code,
                                              constraints = list(multisplits = list(strength = 500),
                                                                 splits = list(strength = 300))
)

##########Diagnostics: Urban Block 3#############
wgt_urban_block_3 <- simulation_weight_disparity_table(urban_block_3_rs)
m <- c(1:12501)
wgt_urban_block_3 <- cbind(m, wgt_urban_block_3)
min(wgt_urban_block_3$max_to_min)

minimum_maxmin_urban_3 <-
  wgt_urban_block_3$m[which(wgt_urban_block_3$max_to_min == min(wgt_urban_block_3$max_to_min))][1]

plans_pref_urban_block_3 <- redist::get_plans_matrix(urban_block_3_rs)

#count the number of splits
splits_urban_block_3 <- count_splits(plans_pref_urban_block_3, urban_block_3_map$code)
splits_urban_block_3[minimum_maxmin_urban_3]

#count the number of municipalities that are split
csplits_urban_block_3 <- redist::redist.splits(plans_pref_urban_block_3, urban_block_3_map$code)
csplits_urban_block_3[minimum_maxmin_urban_3]

results_urban_3 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_urban_block_3)))
results_urban_3$max_to_min <- wgt_urban_block_3$max_to_min
results_urban_3$splits <- splits_urban_block_3
results_urban_3$counties_split <- csplits_urban_block_3
results_urban_3$index <- 1:nrow(wgt_urban_block_3)
results_urban_3$dif <-  results_urban_3$splits - results_urban_3$counties_split
min(results_urban_3$dif)
View(results_urban_3)

block3_optimal <- which(results_urban_3$max_to_min ==
                          min((results_urban_3 %>% filter(dif == 0))$max_to_min))[1]


#############Urban Block 3 Plot Plan################
#get data on optimal plan
matrix_plan_urban_block_3 <- redist::get_plans_matrix(urban_block_3_rs %>% filter(draw == block3_optimal))
colnames(matrix_plan_urban_block_3) <- "district"
optimal_boundary_3 <- cbind(urban_block_3, as_tibble(matrix_plan_urban_block_3))

#get data on municipality boundary
urban_block_boundaries_3 <- urban_block_3 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#get data on district boundary
urban_district_boundaries_3 <- optimal_boundary_3 %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_3, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_3, fill = NA, color = "black", lwd = 1.5) +
  geom_sf(data = urban_district_boundaries_3, fill = NA, color = "yellow", lwd = 0.15) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

##########Rural MS###################
#adjacency list
rural_adj <- redist::redist.adjacency(rural)

#save(rural, file = "rural.Rdata")
#save(rural_adj, file = "rural_adj.Rdata")

#define map
rural_map <- redist::redist_map(rural,
                                ndists = 9,
                                pop_tol= 0.04,
                                total_pop = pop,
                                adj = rural_adj)

# --------- MS simulation ----------------#
#run simulation
rural_ms <- redist::redist_mergesplit(rural_map,
                                      nsims = 25000,
                                      counties = rural_map$code,
                                      constraints = list(multisplits = list(strength = 500),
                                                         splits = list(strength = 300))
)

#########diagnostics:rural#########
wgt_rural <- simulation_weight_disparity_table(rural_ms_res)
m <- c(1:12501)
wgt_rural <- cbind(m, wgt_rural)
min(wgt_rural$max_to_min)

minimum_maxmin_rural <-
  wgt_rural$m[which(wgt_rural$max_to_min == min(wgt_rural$max_to_min))][1]

plans_pref_rural <- redist::get_plans_matrix(rural_ms_res)

#count the number of splits
splits_rural <- count_splits(plans_pref_rural, rural_map$code)
splits_rural[minimum_maxmin_rural]

#count the number of municipalities that are split
csplits_rural<- redist::redist.splits(plans_pref_rural, rural_map$code)
csplits_rural[minimum_maxmin_rural]

results_rural <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_rural)))
results_rural$max_to_min <- wgt_rural$max_to_min
results_rural$splits <- splits_rural
results_rural$counties_split <- csplits_rural
results_rural$index <- 1:nrow(wgt_rural)
results_rural$dif <-  results_rural$splits - results_rural$counties_split
min(results_rural$dif)
View(results_rural)

optimal_rural <- which(results_rural$max_to_min ==
                         min((results_rural %>% filter(dif == 0))$max_to_min))[1]

#############Rural  Plot Plan################
#get data on optimal plan
matrix_plan_rural <- redist::get_plans_matrix(rural_ms_res %>% filter(draw == optimal_rural))
colnames(matrix_plan_rural) <- "district"
optimal_boundary_rural <- cbind(rural, as_tibble(matrix_plan_rural))

#get data on district boundary
rural_district_boundaries <- optimal_boundary_rural %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry))

#get data on municipality boundary
rural_boundaries <- rural %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_rural, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = rural_boundaries, fill = NA, color = "black", lwd = 1) +
  geom_sf(data = rural_district_boundaries, fill = NA, color = "yellow", lwd = 0.15) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

##############Merge Together 4 Blocks###############
#--------merge together---------#
#change color of Suginami-ku to make it more visible
optimal_boundary_1_edited$district[optimal_boundary_1$district==1] <- 9


ggplot() +
  #urban block 1
  geom_sf(data = optimal_boundary_1_edited, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_1, fill = NA, color = "black", lwd = 2.5) +
  geom_sf(data = urban_district_boundaries_1, fill = NA, color = "red", lwd = 1.5) +
  #urban block 2
  geom_sf(data = optimal_boundary_2, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_2, fill = NA, color = "black", lwd = 2.5) +
  geom_sf(data = urban_district_boundaries_2, fill = NA, color = "red", lwd = 1.5) +
  #urban block 3
  geom_sf(data = optimal_boundary_3, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_3, fill = NA, color = "black", lwd = 2.5) +
  geom_sf(data = urban_district_boundaries_3, fill = NA, color = "red", lwd = 1.5) +
  #rural
  geom_sf(data = optimal_boundary_rural, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = rural_boundaries, fill = NA, color = "black", lwd = 2.5) +
  geom_sf(data = rural_district_boundaries, fill = NA, color = "red", lwd = 1.5) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

#------------status quo-----------------#
pref_sq <- status_quo_match(pref)

save(pref_sq, file = "pref_sq.Rdata")

pref_sq_without_islands <- head(pref_sq, 1603) #filter out islands

pref_sq_boundaries <- pref_sq_without_islands %>%
  group_by(ku) %>%
  summarise(geometry = sf::st_union(geometry))

#change the number of ku to make plot more visible
"pref_sq_without_islands$ku[pref_sq_without_islands$ku==10] <- 3
pref_sq_without_islands$ku[pref_sq_without_islands$ku==11] <- 1
pref_sq_without_islands$ku[pref_sq_without_islands$ku==12] <- 2
pref_sq_without_islands$ku[pref_sq_without_islands$ku==13] <- 3
pref_sq_without_islands$ku[pref_sq_without_islands$ku==14] <- 4
pref_sq_without_islands$ku[pref_sq_without_islands$ku==15] <- 5
pref_sq_without_islands$ku[pref_sq_without_islands$ku==16] <- 6
pref_sq_without_islands$ku[pref_sq_without_islands$ku==17] <- 7
pref_sq_without_islands$ku[pref_sq_without_islands$ku==18] <- 9
pref_sq_without_islands$ku[pref_sq_without_islands$ku==19] <- 8
pref_sq_without_islands$ku[pref_sq_without_islands$ku==20] <- 10
pref_sq_without_islands$ku[pref_sq_without_islands$ku==21] <- 1
pref_sq_without_islands$ku[pref_sq_without_islands$ku==22] <- 2
pref_sq_without_islands$ku[pref_sq_without_islands$ku==23] <- 3
pref_sq_without_islands$ku[pref_sq_without_islands$ku==24] <- 4
pref_sq_without_islands$ku[pref_sq_without_islands$ku==25] <- 5"

ggplot() +
  geom_sf(data = pref_sq_without_islands, aes(fill = factor(ku))) +  #color in 25  districts
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_excluding_islands, fill = NA, color = "black", lwd = 2.5) + #municipality boundary
  geom_sf(data = rural_boundaries, fill = NA, color = "black", lwd = 2.5) +
  geom_sf(data = pref_sq_boundaries, fill = NA, color = "red", lwd = 1.5)+ #district boundary
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

#---------status quo: urban block 1-----------#
urban_block_1_sq <- pref_sq %>%
  filter(code %in% urban_by_mun_block$code[which(urban_by_mun_block$block == 1)])
urban_block_1_sq_boundaries <- urban_block_1_sq %>%
  group_by(ku) %>%
  summarise(geometry = sf::st_union(geometry))
ggplot() +
  geom_sf(data = urban_block_1_sq, aes(fill = factor(ku))) +  #color in 25  districts
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_1, fill = NA, color = "black", lwd = 2.5) + #municipality boundary
  geom_sf(data = urban_block_1_sq_boundaries, fill = NA, color = "red", lwd = 1.5) + #district boundary
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

#---------status quo: urban block 2-----------#
urban_block_2_sq <- pref_sq %>%
  filter(code %in% urban_by_mun_block$code[which(urban_by_mun_block$block == 2)])
urban_block_2_sq_boundaries <- urban_block_2_sq %>%
  group_by(ku) %>%
  summarise(geometry = sf::st_union(geometry))
ggplot() +
  geom_sf(data = urban_block_2_sq, aes(fill = factor(ku))) +  #color in 25  districts
  scale_fill_manual(values = c("grey64", "lightsalmon", "deeppink", "orange4",
                               "paleturquoise", "red4", "olivedrab1"
                               )) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_2, fill = NA, color = "black", lwd = 2.5) + #municipality boundary
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())


#---------status quo: urban block 3-----------#
urban_block_3_sq <- pref_sq %>%
  filter(code %in% urban_by_mun_block$code[which(urban_by_mun_block$block == 3)])
urban_block_3_sq_boundaries <- urban_block_3_sq %>%
  group_by(ku) %>%
  summarise(geometry = sf::st_union(geometry))
ggplot() +
  geom_sf(data = urban_block_3_sq, aes(fill = factor(ku))) +  #color in 25  districts
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_block_boundaries_3, fill = NA, color = "black", lwd = 2.5) + #municipality boundary
  geom_sf(data = urban_block_3_sq_boundaries, fill = NA, color = "red", lwd = 1.5) + #district boundary
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

#---------status quo: rural-----------#
rural_sq <- pref_sq %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                "13106", "13107", "13108", "13109", "13110",
                "13111", "13112", "13113", "13114", "13115",
                "13116", "13117", "13118", "13119", "13120",
                "13121", "13122", "13123",
                "13360", "13380", "13400", "13420") == FALSE)
ggplot() +
  geom_sf(data = rural_sq, aes(fill = factor(ku))) +  #color in 25  districts
  scale_fill_manual(values = c("plum4", "darkseagreen4", "darkorange3", "yellow",
                               "burlywood4", "forestgreen", "red", "royalblue4")) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = rural_boundaries, fill = NA, color = "black", lwd = 1.5) + #municipality boundary
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

##########Co-occurrence: Rural##############
#filter out the plans with 0 multi-splits
zero_multisplit_rural <- results_rural %>%
  filter(dif == 0)

#filter out the plans that have a low max:min ratio (Top 10%)
good_num_0 <-  zero_multisplit_rural %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(nsims*0.1)) %>%
  select(index)
good_num_0 <- as.vector(t(good_num_0))
sim_smc_pref_0_good <- rural_ms_res %>%
  filter(draw %in% good_num_0)
#obtain co-occurrence matrix
m_co_0 = redist::prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

#use cluster package to create clusters
cl_co_0 = cluster::agnes(m_co_0)
plot(as.dendrogram(cl_co_0)) # pick a number of clusters from the dendrogram.
prec_clusters_0 = cutree(cl_co_0, 9) #9: number of clusters

#convert data on membership to tibble
pref_membership_0 <- as_tibble(as.data.frame(prec_clusters_0))
names(pref_membership_0) <- "membership"

#co-occurrence: ratio
cooc_ratio <- vector(length = length(rural$code))
#----define relcomp function--------
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(rural$code))
{
  cooc_ratio[i] <- 1 -
    sum(rural$pop[relcomp(rural_adj[[i]]+1,
                              which(prec_clusters_0 == prec_clusters_0[i]))] * m_co_0[i, relcomp(rural_adj[[i]]+1,
                                                                                                 which(prec_clusters_0 == prec_clusters_0[i]))])/
    sum(rural$pop[rural_adj[[i]]+1] * m_co_0[i, rural_adj[[i]]+1])
}

#match membership data with map object
rural_membership <- cbind(rural, cooc_ratio, pref_membership_0)

#sort membership data by group
rural_membership_1 <- rural_membership %>% dplyr::filter(membership == 1)
rural_membership_2 <- rural_membership %>% dplyr::filter(membership == 2)
rural_membership_3 <- rural_membership %>% dplyr::filter(membership == 3)
rural_membership_4 <- rural_membership %>% dplyr::filter(membership == 4)
rural_membership_5 <- rural_membership %>% dplyr::filter(membership == 5)
rural_membership_6 <- rural_membership %>% dplyr::filter(membership == 6)
rural_membership_7 <- rural_membership %>% dplyr::filter(membership == 7)
rural_membership_8 <- rural_membership %>% dplyr::filter(membership == 8)
rural_membership_9 <- rural_membership %>% dplyr::filter(membership == 9)

###plot
ggplot() +
  geom_sf(data = rural_membership_1, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="lightpink", high="red") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_2, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low = "lightgoldenrodyellow", high = "yellow") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_3, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low = "grey71", high = "grey22") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_4, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="darkseagreen1", high="darkseagreen4") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_5, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="burlywood1", high="burlywood4") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_6, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low = "royalblue1", high = "royalblue4") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_7, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="plum1", high="plum4") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_8, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="palegreen", high="forestgreen") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = rural_membership_9, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="orange", high="darkorange3") +

  labs(color = "Co-occurrence",
       title = "Co-occurrence Analysis: Plans with Top 10% Max-min Ratio") +
  geom_sf(data = rural_boundaries, fill = NA, color = "black", lwd = 1.5) +
  theme(legend.box = "vertical",
        legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())



#######Visualization of Blocks###############
tokyo_exclude_islands <- pref %>%
  dplyr::filter(code %in% c("13360", "13380", "13400", "13420") == FALSE) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

tokyo_exclude_islands$block <- c(urban_excluding_islands$block,
                                 rep(4, length(unique(rural$code))))

#get data on district boundary
block_boundary <- tokyo_exclude_islands %>%
  group_by(block) %>%
  summarise(geometry = sf::st_union(geometry))

ggplot() +
  geom_sf(data = tokyo_exclude_islands, aes(fill = factor(block))) +
  scale_fill_manual(values = c("red", "yellow", "orange", "green")) +
  geom_sf(data = block_boundary, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

tokyo_exclude_islands$block <- c(rep(1, 23),
                                 rep(2, length(unique(rural$code))))

#get data on district boundary
block_boundary <- tokyo_exclude_islands %>%
  group_by(block) %>%
  summarise(geometry = sf::st_union(geometry))

ggplot() +
  geom_sf(data = tokyo_exclude_islands, aes(fill = factor(block))) +
  scale_fill_manual(values = c("yellow", "green")) +
  geom_sf(data = block_boundary, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())


