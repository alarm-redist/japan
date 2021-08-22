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
# prefectural information
sim_type <- "smc"
nsims <- 25000
pref_code <- 13
pref_name <- "tokyo"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 30
ndists_old <- 25

#------- Specify municipality splits -------------#
# number of splits
nsplit <- 0
merge_gun_exception <- c()
#西多摩郡 is the only gun in Tokyo and it is not split under the status quo
#大島支庁,三宅支庁, 八丈支庁, 小笠原支庁 are treated as gun

######### Download and Clean Data ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code) #first download data

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)


#########[By region] SMC with fractures constraint################
#clean data
pref <- pref_raw %>%
  clean_jcdf()
pref <- pref %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
#Estimate 2020 pop.
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#---------Setagaya-----------------#
setagaya <- pref %>%
  dplyr::filter(code == 13112)

#adjacency list
setagaya_adj <- redist::redist.adjacency(setagaya)

#set up map
setagaya_map <- redist::redist_map(setagaya,
                                   ndists = 2,
                                   pop_tol= 0.03,
                                   total_pop = pop,
                                   adj = setagaya_adj)

#smc simulation
setagaya_smc <- redist::redist_smc(setagaya_map,
                                   nsims = nsims,
                                   pop_temper = 0.05)

#save
saveRDS(setagaya_smc, paste("simulation/",
                            as.character(pref_code),
                            "_",
                            "setagaya",
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

setagaya_wgt_smc <- simulation_weight_disparity_table(setagaya_smc)

#---------0 split data----------------#
#check 0 split data (match with 2020 census data)
pref_0 <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)
#treat gun as one municipality
pref_0 <- merge_gun(pref_0)

ranking <- pref_0 %>%
  dplyr::arrange(desc(pop))
#select municipalities whose population is less than 50% of target pop -> keep intact
pref_small <- pref_0 %>% dplyr::filter(pop < 0.50 * sum(pref_0$pop)/ndists_new)
pref_small$subcode <- "0000"

#select municipalities whose population is more than 50% of target pop -> separate
large_codes <- pref_0$code[which(pref_0$code %in% pref_small$code == FALSE)]
pref_large <- pref %>% dplyr::filter(code %in% large_codes)

pref_50 <- dplyr::bind_rows(pref_small, pref_large)

#------23-ku Simulation Prep-------------#
urban <- pref_50 %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111",          "13113", "13114", "13115", #skip Setagaya
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420")) #Islands are considered part of Minato-ku

#adjacency list
urbanadj <- redist::redist.adjacency(urban)

#ferries
ferries_urban <- add_ferries(urban)

#edit adjacency list
urbanadj <- geomander::add_edge(urbanadj, ferries_urban$V1, ferries_urban$V2)
#manually edit adjacency list
#[247]13109 0250 品川区八潮
#[334]13111 0580 大田区東海
#[335]13111 0590 大田区城南島
urbanadj <- geomander::add_edge(urbanadj, 247, 240) #[240]品川区東品川180
urbanadj <- geomander::add_edge(urbanadj, 247, 238) #[238]品川区東大井160
urbanadj <- geomander::add_edge(urbanadj, 247, 226) #[226]品川区勝島40
urbanadj <- geomander::add_edge(urbanadj, 334, 335)
urbanadj <- geomander::add_edge(urbanadj, 334, 285) #[285] 13111 0090大田区平和島
#[574]13120 0420 練馬区西大泉町
urbanadj <- geomander::add_edge(urbanadj, 574, 575) #connect to [575]練馬区西大泉(６丁目) 0430

#map
urban_map <- redist::redist_map(urban,
                                ndists = ndists_new,
                                pop_tol= 0.20,
                                total_pop = pop,
                                adj = urbanadj)

#------Tama Region Simulation Prep-------------#
rural <-  pref_50 %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115",
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420") == FALSE)

#adjacency list
ruraladj <- redist::redist.adjacency(rural)

#map
rural_map <- redist::redist_map(rural,
                                ndists = ndists_new,
                                pop_tol= 0.20,
                                total_pop = pop,
                                adj = ruraladj)


#save(list=ls(all=TRUE), file="13_smc_tokyo_data_by_region.Rdata")

#urban map
urban_map <- redist::redist_map(urban,
                                ndists = 19,
                                pop_tol= 0.05,
                                total_pop = pop,
                                adj = urbanadj)

#rural map
rural_map <- redist::redist_map(rural,
                                ndists = 9,
                                pop_tol= 0.20,
                                total_pop = pop,
                                adj = ruraladj)

urban_smc <- redist::redist_smc(urban_map,
                                nsims = 25000,
                                counties = urban_map$code,
                                constraints = list(multisplits = list(strength = 4)),
                                pop_temper = 0.05
)


########Pref: pref_raw############
pref <- clean_jcdf(pref_raw)
pref_0 <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)
##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#ferries
ferries_0 <- add_ferries(pref_0)
#[3]13103港区 -> [54]13420小笠原支庁 [52]13380三宅支庁 [51]13360大島支庁
#[52]13380三宅支庁 -> [53]13400八丈支庁

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list

#add edge
prefadj_0 <- geomander::add_edge(prefadj_0,
                                 ferries_0[, 1],
                                 ferries_0[, 2])


pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.65,
                                 total_pop = pop,
                                 adj = prefadj_0)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_0split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_0 <- redist::redist_smc(pref_map_0,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_0, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_0",
                              ".Rds",
                              sep = ""))


##########0 split Analysis###############
# extract plans
pref_smc_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)

# get disparity table data
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_smc_0 <- cbind(n, wgt_smc_0)
#wgt_smc_0$n[which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))]
#Maxmin 7.2768 # 3    21    22    28    41...
#redist::redist.plot.plans(sim_smc_pref_0, draws = 3, geom = pref_map_0)

#Sq max 573,969 東京２２区 Sq min 482,077 東京25区 -> 1.1906

tokyo_13_optimalmap_0 <- redist::redist.plot.map(shp = pref_0,
                                                 plan = pref_smc_plans_0[, which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))[1]],
                                                 boundaries = FALSE,
                                                 title = "Tokyo Optimal Plan (0-split)") +
  scale_fill_discrete() +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_0$max_to_min), 3), sep = ""), hjust = 0.5)

#save(list=ls(all=TRUE), file="13_smc_tokyo_data_0to0splits.Rdata")



##########2 splits#################
#find the municipality codes of the 1st ~ 2nd largest municipalities
largest_2 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:2]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_2 <- pref_0 %>% dplyr::filter(code %in% largest_2 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_2$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_2 <- pref %>%
  dplyr::filter(code %in% largest_2) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_2 <- calc_kokumin(pref_split_2, dem_pops)
pref_split_2 <- estimate_2020_pop(pref_split_2, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_2 <- dplyr::bind_rows(pref_intact_2, pref_split_2)

#Ferries
ferries_2 <- add_ferries(pref_2)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_2 <- redist::redist.adjacency(pref_2)
#add edge
prefadj_2 <- geomander::add_edge(prefadj_2, ferries_2$V1, ferries_2$V2)

#manually add adjacency
prefadj_2 <- geomander::add_edge(prefadj_2, 155, 156)
#[155] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [156]練馬区西大泉(６丁目) 0430

pref_map_2 <- redist::redist_map(pref_2,
                                ndists = ndists_new,
                                pop_tol= 0.65,
                                total_pop = pop,
                                adj = prefadj_2)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_2split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_2 <- redist::redist_smc(pref_map_2,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_2, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_2",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_2 <- simulation_weight_disparity_table(sim_smc_pref_2)
#n <- c(1:25000)
#wgt_smc_2 <- cbind(n, wgt_smc_2)
#wgt_smc_2$n[which(wgt_smc_2$max_to_min == min(wgt_smc_2$max_to_min))]
#Maxmin 3.4999 #10828 20200 20976
#redist::redist.plot.plans(sim_smc_pref_2, draws = 10828, geom = pref_map_2)

#county splits
plans_pref_2 <- redist::get_plans_matrix(sim_smc_pref_2)
# get splits
splits_2 <- count_splits(plans_pref_2, pref_map_2$code)
#11 splits?


##########4 splits#################
#find the municipality codes of the 1st ~ 4th largest municipalities
largest_4 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:4]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_4 <- pref_0 %>% dplyr::filter(code %in% largest_4 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_4$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_4 <- pref %>%
  dplyr::filter(code %in% largest_4) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_4 <- calc_kokumin(pref_split_4, dem_pops)
pref_split_4 <- estimate_2020_pop(pref_split_4, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_4 <- dplyr::bind_rows(pref_intact_4, pref_split_4)

#Ferries
ferries_4 <- add_ferries(pref_4)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_4 <- redist::redist.adjacency(pref_4)
#add edge
prefadj_4 <- geomander::add_edge(prefadj_4, ferries_4$V1, ferries_4$V2)

#manually add adjacency
prefadj_4 <- geomander::add_edge(prefadj_4, 215, 216)
#[215] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [216]練馬区西大泉(６丁目) 0430

pref_map_4 <- redist::redist_map(pref_4,
                                 ndists = ndists_new,
                                 pop_tol= 0.50,
                                 total_pop = pop,
                                 adj = prefadj_4)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_4split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_4 <- redist::redist_smc(pref_map_4,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_4, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_4",
                              ".Rds",
                              sep = ""))

#pop disparity
wgt_smc_4 <- simulation_weight_disparity_table(sim_smc_pref_4)
#n <- c(1:25000)
#wgt_smc_4 <- cbind(n, wgt_smc_4)
#wgt_smc_4$n[which(wgt_smc_4$max_to_min == min(wgt_smc_4$max_to_min))]
#Maxmin 2.4975 #278   497  1383....
#redist::redist.plot.plans(sim_smc_pref_4, draws = 278, geom = pref_map_4)

#county splits
plans_pref_4 <- redist::get_plans_matrix(sim_smc_pref_4)
# get splits
splits_4 <- count_splits(plans_pref_4, pref_map_4$code)
#16 splits?

##########6 splits#################
#find the municipality codes of the 1st ~ 6th largest municipalities
largest_6 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:6]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_6 <- pref_0 %>% dplyr::filter(code %in% largest_6 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_6$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_6 <- pref %>%
  dplyr::filter(code %in% largest_6) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_6 <- calc_kokumin(pref_split_6, dem_pops)
pref_split_6 <- estimate_2020_pop(pref_split_6, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_6 <- dplyr::bind_rows(pref_intact_6, pref_split_6)

#Ferries
ferries_6 <- add_ferries(pref_6)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_6 <- redist::redist.adjacency(pref_6)
#add edge
prefadj_6 <- geomander::add_edge(prefadj_6, ferries_6$V1, ferries_6$V2)

#manually add adjacency
prefadj_6 <- geomander::add_edge(prefadj_6, 249, 250)
#[249] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [250]練馬区西大泉(６丁目) 0430

pref_map_6 <- redist::redist_map(pref_6,
                                 ndists = ndists_new,
                                 pop_tol= 0.27,
                                 total_pop = pop,
                                 adj = prefadj_6)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_6split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_6 <- redist::redist_smc(pref_map_6,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_6, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_6",
                              ".Rds",
                              sep = ""))

#pop disparity
wgt_smc_6 <- simulation_weight_disparity_table(sim_smc_pref_6)
#n <- c(1:25000)
#wgt_smc_6 <- cbind(n, wgt_smc_6)
#wgt_smc_6$n[which(wgt_smc_6$max_to_min == min(wgt_smc_6$max_to_min))]
#Maxmin 1.5612 #18129
#redist::redist.plot.plans(sim_smc_pref_6, draws = 18129, geom = pref_map_6)

#county splits
plans_pref_6 <- redist::get_plans_matrix(sim_smc_pref_6)
# get splits
splits_6 <- count_splits(plans_pref_6, pref_map_6$code)
#22 splits?


##########8 splits#################
#find the municipality codes of the 1st ~ 8th largest municipalities
largest_8 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:8]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_8 <- pref_0 %>% dplyr::filter(code %in% largest_8 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_8$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_8 <- pref %>%
  dplyr::filter(code %in% largest_8) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_8 <- calc_kokumin(pref_split_8, dem_pops)
pref_split_8 <- estimate_2020_pop(pref_split_8, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_8 <- dplyr::bind_rows(pref_intact_8, pref_split_8)

#Ferries
ferries_8 <- add_ferries(pref_8)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_8 <- redist::redist.adjacency(pref_8)
#add edge
prefadj_8 <- geomander::add_edge(prefadj_8, ferries_8$V1, ferries_8$V2)

#manually add adjacency
prefadj_8 <- geomander::add_edge(prefadj_8, 304, 305)
#[304] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [305]練馬区西大泉(６丁目) 0430

pref_map_8 <- redist::redist_map(pref_8,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_8)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_8split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_8 <- redist::redist_smc(pref_map_8,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_8, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_8",
                              ".Rds",
                              sep = ""))

#pop disparity
wgt_smc_8 <- simulation_weight_disparity_table(sim_smc_pref_8)
#n <- c(1:25000)
#wgt_smc_8 <- cbind(n, wgt_smc_8)
#wgt_smc_8$n[which(wgt_smc_8$max_to_min == min(wgt_smc_8$max_to_min))]
#Maxmin 1.4113 #950  5033  6848  8596  8700  9756 12537 20757 22027
#redist::redist.plot.plans(sim_smc_pref_8, draws = 950, geom = pref_map_8)

#county splits
plans_pref_8 <- redist::get_plans_matrix(sim_smc_pref_8)
# get splits
splits_8 <- count_splits(plans_pref_8, pref_map_8$code)
#26 splits?


##########8 splits (pop. of municipality * total population of adjacent municipalities)#################
#--------Raw data----------#
#clean 0 split data
pref_0 <- merge_gun(pref_0)
ferries_0 <- add_ferries(pref_0)
prefadj_0 <- redist::redist.adjacency(pref_0)
prefadj_0 <- geomander::add_edge(prefadj_0, ferries_0[, 1], ferries_0[, 2])
#Small units
pref <- pref %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#--------Total pop. of adjacent municipalities----------#
adj_total <- c(1:length(prefadj_0))
for(i in 1:length(prefadj_0)){
  adj_total[i] <- sum(pref_0$pop[prefadj_0[[i]]+1])
}
yy <- dplyr::bind_cols(pref_0$code, pref_0$pop, adj_total)
colnames(yy) <- c("code", "pop", "adj_total")
yy$YY <- yy$pop * yy$adj_total
#reorder
yy <- yy %>%
  arrange(desc(YY))

#find the municipality codes of the 1st ~ 8th largest YY Index
yy_8 <- yy$code[1:8]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_8_2 <- pref_0 %>% dplyr::filter(code %in% yy_8 == FALSE )
#run merge at this stage
pref_intact_8_2$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_8_2 <- pref %>% dplyr::filter(code %in% yy_8)

#bind together
pref_8_2 <- dplyr::bind_rows(pref_intact_8_2, pref_split_8_2)

#Ferries
ferries_8_2 <- add_ferries(pref_8_2)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_8_2 <- redist::redist.adjacency(pref_8_2)
#add edge
prefadj_8_2 <- geomander::add_edge(prefadj_8_2, ferries_8_2$V1, ferries_8_2$V2)

#manually add adjacency
prefadj_8_2 <- geomander::add_edge(prefadj_8_2, 116, 109)
prefadj_8_2 <- geomander::add_edge(prefadj_8_2, 116, 107)
prefadj_8_2 <- geomander::add_edge(prefadj_8_2, 116, 95)
prefadj_8_2 <- geomander::add_edge(prefadj_8_2, 176, 177)
prefadj_8_2 <- geomander::add_edge(prefadj_8_2, 176, 127)
prefadj_8_2 <- geomander::add_edge(prefadj_8_2, 319, 320)
#connect [116]品川区八潮 13109 250 to [109]品川区東品川180;[107]品川区東大井160 [95]品川区勝島40
#connect [176]大田区東海 13111 580 to [177]東京都大田区城南島590; [127] 13111 0090大田区平和島
#[319] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [320]練馬区西大泉(６丁目) 0430

pref_map_8_2 <- redist::redist_map(pref_8_2,
                                  ndists = ndists_new,
                                  pop_tol= 0.50,
                                  total_pop = pop,
                                  adj = prefadj_8_2)


# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_8_2 <- redist::redist_smc(pref_map_8_2,
                                      nsims = nsims,
                                      pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_8_2, paste("simulation/",
                                as.character(pref_code),
                                "_",
                                as.character(pref_name),
                                "_",
                                as.character(sim_type),
                                "_",
                                as.character(nsims),
                                "_8_2",
                                ".Rds",
                                 sep = ""))

#pop disparity
wgt_smc_8_2 <- simulation_weight_disparity_table(sim_smc_pref_8_2)
#n <- c(1:25000)
#wgt_smc_8_2 <- cbind(n, wgt_smc_8_2)
#wgt_smc_8_2$n[which(wgt_smc_8_2$max_to_min == min(wgt_smc_8_2$max_to_min))]
#Maxmin 2.50643 #8215 10148 17573 24827
#redist::redist.plot.plans(sim_smc_pref_8_2, draws = 8215, geom = pref_map_8_2)

#county splits
plans_pref_8_2 <- redist::get_plans_matrix(sim_smc_pref_8_2)
# get splits
splits_8_2 <- count_splits(plans_pref_8_2, pref_map_8_2$code)
#28 splits?



##########9 splits#################
#find the municipality codes of the 1st ~ 9th largest municipalities
largest_9 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:9]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_9 <- pref_0 %>% dplyr::filter(code %in% largest_9 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_9$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_9 <- pref %>%
  dplyr::filter(code %in% largest_9) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_9 <- calc_kokumin(pref_split_9, dem_pops)
pref_split_9 <- estimate_2020_pop(pref_split_9, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_9 <- dplyr::bind_rows(pref_intact_9, pref_split_9)

#Ferries
ferries_9 <- add_ferries(pref_9)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_9 <- redist::redist.adjacency(pref_9)
#add edge
prefadj_9 <- geomander::add_edge(prefadj_9, ferries_9$V1, ferries_9$V2)

#manually add adjacency
prefadj_9 <- geomander::add_edge(prefadj_9, 348, 349)
#[348] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [349]練馬区西大泉(６丁目) 0430

pref_map_9 <- redist::redist_map(pref_9,
                                 ndists = ndists_new,
                                 pop_tol= 0.17,
                                 total_pop = pop,
                                 adj = prefadj_9)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_9split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_9 <- redist::redist_smc(pref_map_9,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_9, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_9",
                              ".Rds",
                              sep = ""))

#pop disparity
wgt_smc_9 <- simulation_weight_disparity_table(sim_smc_pref_9)
#n <- c(1:25000)
#wgt_smc_9 <- cbind(n, wgt_smc_9)
#wgt_smc_9$n[which(wgt_smc_9$max_to_min == min(wgt_smc_9$max_to_min))]
#Maxmin 1.3523 #2    4    6    8    9 ...
#redist::redist.plot.plans(sim_smc_pref_9, draws = 2, geom = pref_map_9)

#county splits
plans_pref_9 <- redist::get_plans_matrix(sim_smc_pref_9)
# get splits
splits_9 <- count_splits(plans_pref_9, pref_map_9$code)
#26 splits?


########9 splits (in order of total pop. of adjacent municipalities) ############
#--------Raw data----------#
#0 split
pref_0 <- merge_gun(pref_0)
ferries_0 <- add_ferries(pref_0)
prefadj_0 <- redist::redist.adjacency(pref_0)
prefadj_0 <- geomander::add_edge(prefadj_0, ferries_0[, 1], ferries_0[, 2])
#Small units
pref <- pref %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#--------Total pop. of adjacent municipalities----------#
adj_total <- c(1:length(prefadj_0))
for(i in 1:length(prefadj_0)){
  adj_total[i] <- sum(pref_0$pop[prefadj_0[[i]]+1])
}
pop_total_adj <- dplyr::bind_cols(pref_0$code, adj_total)
colnames(pop_total_adj) <- c("code", "pop")
pop_total_adj <- pop_total_adj %>%
  arrange(desc(pop))

#--------Prepare dataframe----------#
#find the municipality codes of the 1st ~ 9th largest municipalities
largest_9_2 <- pop_total_adj$code[1:9]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_9_2 <- pref_0 %>% dplyr::filter(code %in% largest_9_2 == FALSE)
pref_intact_9_2$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_9_2 <- pref %>%
  dplyr::filter(code %in% largest_9_2)
pref_9_2 <- dplyr::bind_rows(pref_intact_9_2, pref_split_9_2)

# -------- set up for simulation ------------#
#Ferries
ferries_9_2 <- add_ferries(pref_9_2)
# Adjacency list
prefadj_9_2 <- redist::redist.adjacency(pref_9_2)
#add edge
prefadj_9_2 <- geomander::add_edge(prefadj_9_2, ferries_9_2$V1, ferries_9_2$V2)
prefadj_9_2 <- geomander::add_edge(prefadj_9_2, 308, 309)
#[309] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [310]練馬区西大泉(６丁目) 0430
pref_map_9_2 <- redist::redist_map(pref_9_2,
                                   ndists = ndists_new,
                                   pop_tol= 1.00,
                                   total_pop = pop,
                                   adj = prefadj_9_2)

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_9_2 <- redist::redist_smc(pref_map_9_2,
                                       nsims = nsims,
                                       pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_9_2, paste("simulation/",
                                as.character(pref_code),
                                "_",
                                as.character(pref_name),
                                "_",
                                as.character(sim_type),
                                "_",
                                as.character(nsims),
                                "_9_2",
                                ".Rds",
                                sep = ""))

#pop disparity
wgt_smc_9_2 <- simulation_weight_disparity_table(sim_smc_pref_9_2)
#n <- c(1:25000)
#wgt_smc_9_2 <- cbind(n, wgt_smc_9_2)
#wgt_smc_9_2$n[which(wgt_smc_9_2$max_to_min == min(wgt_smc_9_2$max_to_min))]
#Maxmin 2.4348 #7529
#redist::redist.plot.plans(sim_smc_pref_9_2, draws = 18129, geom = pref_map_9_2)


##########10 splits#################
#find the municipality codes of the 1st ~ 10th largest municipalities
largest_10 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:10]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_10 <- pref_0 %>% dplyr::filter(code %in% largest_10 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_10$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_10 <- pref %>%
  dplyr::filter(code %in% largest_10) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_10 <- calc_kokumin(pref_split_10, dem_pops)
pref_split_10 <- estimate_2020_pop(pref_split_10, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_10 <- dplyr::bind_rows(pref_intact_10, pref_split_10)

#Ferries
ferries_10 <- add_ferries(pref_10)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_10 <- redist::redist.adjacency(pref_10)
#add edge
prefadj_10 <- geomander::add_edge(prefadj_10, ferries_10$V1, ferries_10$V2)

#manually add adjacency
prefadj_10 <- geomander::add_edge(prefadj_10, 347, 348)
#[347] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [348]練馬区西大泉(６丁目) 0430

pref_map_10 <- redist::redist_map(pref_10,
                                 ndists = ndists_new,
                                 pop_tol= 0.17,
                                 total_pop = pop,
                                 adj = prefadj_10)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_10split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_10 <- redist::redist_smc(pref_map_10,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_10, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_10",
                              ".Rds",
                              sep = ""))

#pop disparity
wgt_smc_10 <- simulation_weight_disparity_table(sim_smc_pref_10)
#n <- c(1:25000)
#wgt_smc_10 <- cbind(n, wgt_smc_10)
#wgt_smc_10$n[which(wgt_smc_10$max_to_min == min(wgt_smc_10$max_to_min))]
#Maxmin 1.3471 #1858 7214
#redist::redist.plot.plans(sim_smc_pref_10, draws = 1858, geom = pref_map_10)

#county splits
plans_pref_10 <- redist::get_plans_matrix(sim_smc_pref_10)
# get splits
splits_10 <- count_splits(plans_pref_10, pref_map_10$code)
#31 splits?


##########10 splits (largest 9 + highest YY)#################
#find the municipality codes of the 1st ~ 8th largest municipalities
yy_10 <- c((pref_0 %>% dplyr::arrange(desc(pop)))$code[1:9],13109)

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_10_y <- pref_0 %>% dplyr::filter(code %in% yy_10 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_10_y$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_10_y <- pref %>%
  dplyr::filter(code %in% yy_10) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_10_y <- calc_kokumin(pref_split_10_y, dem_pops)
pref_split_10_y <- estimate_2020_pop(pref_split_10_y, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_10_y <- dplyr::bind_rows(pref_intact_10_y, pref_split_10_y)

#Ferries
ferries_10_y <- add_ferries(pref_10_y)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_10_y <- redist::redist.adjacency(pref_10_y)
#add edge
prefadj_10_y <- geomander::add_edge(prefadj_10_y, ferries_10_y$V1, ferries_10_y$V2)

#manually add adjacency
prefadj_10_y <- geomander::add_edge(prefadj_10_y, 114, 107)
prefadj_10_y <- geomander::add_edge(prefadj_10_y, 114, 105)
prefadj_10_y <- geomander::add_edge(prefadj_10_y, 114, 93)
prefadj_10_y <- geomander::add_edge(prefadj_10_y, 174, 175)
prefadj_10_y <- geomander::add_edge(prefadj_10_y, 174, 125)
prefadj_10_y <- geomander::add_edge(prefadj_10_y, 374, 375)
#connect [114]品川区八潮 13109 250 to [107]品川区東品川180;[105]品川区東大井160 [93]品川区勝島40
#connect [174]大田区東海 13111 580 to [175]東京都大田区城南島590; [125] 13111 0090大田区平和島
#[374] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [375]練馬区西大泉(６丁目) 0430

pref_map_10_y <- redist::redist_map(pref_10_y,
                                    ndists = ndists_new,
                                    pop_tol= 0.20,
                                    total_pop = pop,
                                    adj = prefadj_10_y)


# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_10_y <- redist::redist_smc(pref_map_10_y,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_10_y, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_10_y",
                              ".Rds",
                              sep = ""))

#pop disparity
wgt_smc_10_y <- simulation_weight_disparity_table(sim_smc_pref_10_y)
#n <- c(1:25000)
#wgt_smc_10_y <- cbind(n, wgt_smc_10_y)
#wgt_smc_10_y$n[which(wgt_smc_10_y$max_to_min == min(wgt_smc_10_y$max_to_min))]
#Maxmin 1.3749 #348   634   688   865...
#redist::redist.plot.plans(sim_smc_pref_10_y, draws = 950, geom = pref_map_10_y)

#county splits
plans_pref_10_y <- redist::get_plans_matrix(sim_smc_pref_10_y)
# get splits
splits_10_y <- count_splits(plans_pref_10_y, pref_map_10_y$code)
#26 splits?


##########10 splits (Chris)#############
sim_smc_pref_10_2 <- redist::redist_smc(pref_map_10,
                                       nsims = 25000,
                                       pop_temper = 0.05,
                                       counties = pref_map_10$code,
                                       constraints = list(splits = list(strength = 1000)))

plans_pref_10_2 <- redist::get_plans_matrix(sim_smc_pref_10_2)

weight_pref_10_2 <- simulation_weight_disparity_table(sim_smc_pref_10_2)
weight_pref_10_2$index <- 1:nrow(weight_pref_10_2)
weight_pref_10_2$counties_split <- redist::redist.splits(plans_pref_10_2, pref_map_10$code)
part_chains_10_2 <- plans_pref_10_2[, weight_pref_10_2[order(weight_pref_10_2$counties_split,
                                                           weight_pref_10_2$max_to_min), ]$index [1:2500]]

parallel_pref_10_2 <- redist_mergesplit_parallel(
  map = pref_map_10,
  nsims = 200,
  chains = ncol(part_chains_10_2),
  warmup = 199,
  init_plan = part_chains_10_2,
  counties = pref_map_10$code,
  constraints = list(splits = list(strength = 1000)),
  silent = TRUE
)

# save it
saveRDS(parallel_pref_10_2, paste("simulation/",
                                 sprintf("%02d", pref_code),
                                 "_",
                                 as.character(pref_name),
                                 "_",
                                 as.character(sim_type),
                                 "_",
                                 as.character(nsims),
                                 "_",
                                 "parallel_10.Rds",
                                 sep = ""))

# get disparity data
parallel_weight_pref_10_2 <- simulation_weight_disparity_table(parallel_pref_10_2)
parallel_plans_pref_10_2 <- redist::get_plans_matrix(parallel_pref_10_2)

# get splits
parallel_splits_10_2 <- count_splits(parallel_plans_pref_10_2, part_map_10$code)
parallel_countiessplit_10_2 <- redist::redist.splits(parallel_plans_pref_10_2, part_map_10$code)

parallel_results_10_2 <- data.frame(matrix(ncol = 0, nrow = nrow(parallel_weight_pref_10_2)))
parallel_results_10_2$max_to_min <- parallel_weight_pref_10_2$max_to_min
parallel_results_10_2$splits <- parallel_splits_10_2
parallel_results_10_2$counties_split <- parallel_countiessplit_10_2
parallel_results_10_2$index <- 1:nrow(parallel_weight_pref_10_2)

parallel_results_10_2 <- parallel_results_10_2 %>%
  dplyr::group_by(max_to_min, splits, counties_split) %>%
  dplyr::summarise(index = first(index))




##########10 splits (Nakano) #################
#find the municipality codes of the 1st ~ 10th largest municipalities
largest_10_n <- c((pref_0 %>% dplyr::arrange(desc(pop)))$code[1:9], 13114)

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_10_n <- pref_0 %>% dplyr::filter(code %in% largest_10_n == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_10_n$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_10_n <- pref %>%
  dplyr::filter(code %in% largest_10_n) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_10_n <- calc_kokumin(pref_split_10_n, dem_pops)
pref_split_10_n <- estimate_2020_pop(pref_split_10_n, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_10_n <- dplyr::bind_rows(pref_intact_10_n, pref_split_10_n)

#Ferries
ferries_10_n <- add_ferries(pref_10_n)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_10_n <- redist::redist.adjacency(pref_10_n)
#add edge
prefadj_10_n <- geomander::add_edge(prefadj_10_n, ferries_10_n$V1, ferries_10_n$V2)

#manually add adjacency
prefadj_10_n <- geomander::add_edge(prefadj_10_n, 366, 367)
#[366] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [367]練馬区西大泉(６丁目) 0430

pref_map_10_n <- redist::redist_map(pref_10_n,
                                    ndists = ndists_new,
                                    pop_tol= 0.10,
                                    total_pop = pop,
                                    adj = prefadj_10_n)


# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_10_n <- redist::redist_smc(pref_map_10_n,
                                        nsims = nsims,
                                        pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_10_n, paste("simulation/",
                                as.character(pref_code),
                                "_",
                                as.character(pref_name),
                                "_",
                                as.character(sim_type),
                                "_",
                                as.character(nsims),
                                "_10_n",
                                ".Rds",
                                sep = ""))

#pop disparity
wgt_smc_10_n <- simulation_weight_disparity_table(sim_smc_pref_10_n)
#wgt_smc_10_n <- cbind(n, wgt_smc_10_n)
#wgt_smc_10_n$n[which(wgt_smc_10_n$max_to_min == min(wgt_smc_10_n$max_to_min))]
#Maxmin 1.1635 #589   652  1352  1982  2176 ...
#redist::redist.plot.plans(sim_smc_pref_10_n, draws = 589, geom = pref_map_10_n)

#county splits
plans_pref_10_n <- redist::get_plans_matrix(sim_smc_pref_10_n)
# get splits
splits_10_n <- count_splits(plans_pref_10_n, pref_map_10_n$code)
#31 splits?


##########11 splits#################
#find the municipality codes of the 1st ~ 11th largest municipalities
largest_11 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:11]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_11 <- pref_0 %>% dplyr::filter(code %in% largest_11 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_11$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_11 <- pref %>%
  dplyr::filter(code %in% largest_11) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_11 <- calc_kokumin(pref_split_11, dem_pops)
pref_split_11 <- estimate_2020_pop(pref_split_11, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_11 <- dplyr::bind_rows(pref_intact_11, pref_split_11)

#Ferries
ferries_11 <- add_ferries(pref_11)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_11 <- redist::redist.adjacency(pref_11)
#add edge
prefadj_11 <- geomander::add_edge(prefadj_11, ferries_11$V1, ferries_11$V2)

#manually add adjacency
prefadj_11 <- geomander::add_edge(prefadj_11, 346, 347)
#[346] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [347]練馬区西大泉(６丁目) 0430

pref_map_11 <- redist::redist_map(pref_11,
                                  ndists = ndists_new,
                                  pop_tol= 0.17,
                                  total_pop = pop,
                                  adj = prefadj_11)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_11split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_11 <- redist::redist_smc(pref_map_11,
                                      nsims = nsims,
                                      pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_11, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_11",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_11 <- simulation_weight_disparity_table(sim_smc_pref_11)
#n <- c(1:25000)
#wgt_smc_11 <- cbind(n, wgt_smc_11)
#wgt_smc_11$n[which(wgt_smc_11$max_to_min == min(wgt_smc_11$max_to_min))]
#Maxmin 1.3500 #17941
#redist::redist.plot.plans(sim_smc_pref_11, draws = 17941, geom = pref_map_11)

#county splits
plans_pref_11 <- redist::get_plans_matrix(sim_smc_pref_11)
# get splits
splits_11 <- count_splits(plans_pref_11, pref_map_11$code)
#29 splits?



##########12 splits#################
#find the municipality codes of the 1st ~ 12th largest municipalities
largest_12 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:12]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_12 <- pref_0 %>% dplyr::filter(code %in% largest_12 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_12$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_12 <- pref %>%
  dplyr::filter(code %in% largest_12) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_12 <- calc_kokumin(pref_split_12, dem_pops)
pref_split_12 <- estimate_2020_pop(pref_split_12, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_12 <- dplyr::bind_rows(pref_intact_12, pref_split_12)

#Ferries
ferries_12 <- add_ferries(pref_12)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_12 <- redist::redist.adjacency(pref_12)
#add edge
prefadj_12 <- geomander::add_edge(prefadj_12, ferries_12$V1, ferries_12$V2)

#manually add adjacency
prefadj_12 <- geomander::add_edge(prefadj_12, 112, 105)
prefadj_12 <- geomander::add_edge(prefadj_12, 112, 103)
prefadj_12 <- geomander::add_edge(prefadj_12, 112, 91)
prefadj_12 <- geomander::add_edge(prefadj_12, 172, 173)
prefadj_12 <- geomander::add_edge(prefadj_12, 372, 373)

#connect [112]品川区八潮 13109 250 to [105]品川区東品川180;[103]品川区東大井160 [91]品川区勝島40
#connect [172]大田区東海 13111 580 to [173]東京都大田区城南島590
#[372] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [373]練馬区西大泉(６丁目) 0430

pref_map_12 <- redist::redist_map(pref_12,
                                  ndists = ndists_new,
                                  pop_tol= 0.165,
                                  total_pop = pop,
                                  adj = prefadj_12)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_12split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_12 <- redist::redist_smc(pref_map_12,
                                      nsims = nsims,
                                      pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_12, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_12",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_12 <- simulation_weight_disparity_table(sim_smc_pref_12)
#n <- c(1:25000)
#wgt_smc_12 <- cbind(n, wgt_smc_12)
#wgt_smc_12$n[which(wgt_smc_12$max_to_min == min(wgt_smc_12$max_to_min))]
#Maxmin 1.3521 # 603   621   655   835...
#redist::redist.plot.plans(sim_smc_pref_12, draws = 603, geom = pref_map_12)

#county splits
plans_pref_12 <- redist::get_plans_matrix(sim_smc_pref_12)
# get splits
splits_12 <- count_splits(plans_pref_12, pref_map_12$code)
#33 splits?


##########13 splits#################
#find the municipality codes of the 1st ~ 13th largest municipalities
largest_13 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:13]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_13 <- pref_0 %>% dplyr::filter(code %in% largest_13 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_13$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_13 <- pref %>%
  dplyr::filter(code %in% largest_13) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_13 <- calc_kokumin(pref_split_13, dem_pops)
pref_split_13 <- estimate_2020_pop(pref_split_13, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_13 <- dplyr::bind_rows(pref_intact_13, pref_split_13)

#Ferries
ferries_13 <- add_ferries(pref_13)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_13 <- redist::redist.adjacency(pref_13)
#add edge
prefadj_13 <- geomander::add_edge(prefadj_13, ferries_13$V1, ferries_13$V2)

#manually add adjacency
prefadj_13 <- geomander::add_edge(prefadj_13, 111, 104)
prefadj_13 <- geomander::add_edge(prefadj_13, 111, 102)
prefadj_13 <- geomander::add_edge(prefadj_13, 111, 90)
prefadj_13 <- geomander::add_edge(prefadj_13, 171, 172)
prefadj_13 <- geomander::add_edge(prefadj_13, 401, 402)

#connect [111]品川区八潮 13109 250 to [104]品川区東品川180;[102]品川区東大井160 [90]品川区勝島40
#connect [171]大田区東海 13111 580 to [172]東京都大田区城南島590
#[401] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [402]練馬区西大泉(６丁目) 0430

pref_map_13 <- redist::redist_map(pref_13,
                                  ndists = ndists_new,
                                  pop_tol= 0.165,
                                  total_pop = pop,
                                  adj = prefadj_13)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_13split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_13 <- redist::redist_smc(pref_map_13,
                                      nsims = nsims,
                                      pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_13, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_13",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_13 <- simulation_weight_disparity_table(sim_smc_pref_13)
#n <- c(1:25000)
#wgt_smc_13 <- cbind(n, wgt_smc_13)
#wgt_smc_13$n[which(wgt_smc_13$max_to_min == min(wgt_smc_13$max_to_min))]
#Maxmin 1.3445 # 31,  91   102   112   128   190...
#redist::redist.plot.plans(sim_smc_pref_13, draws = 31, geom = pref_map_13)

#county splits
plans_pref_13 <- redist::get_plans_matrix(sim_smc_pref_13)
# get splits
splits_13 <- count_splits(plans_pref_13, pref_map_13$code)
#40 splits?

#find the municipality codes of the 1st ~ 13th largest municipalities
largest_13 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:13]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_13 <- pref_0 %>% dplyr::filter(code %in% largest_13 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_13$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_13 <- pref %>%
  dplyr::filter(code %in% largest_13) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_13 <- calc_kokumin(pref_split_13, dem_pops)
pref_split_13 <- estimate_2020_pop(pref_split_13, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_13 <- dplyr::bind_rows(pref_intact_13, pref_split_13)

#Ferries
ferries_13 <- add_ferries(pref_13)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_13 <- redist::redist.adjacency(pref_13)
#add edge
prefadj_13 <- geomander::add_edge(prefadj_13, ferries_13$V1, ferries_13$V2)

#manually add adjacency
prefadj_13 <- geomander::add_edge(prefadj_13, 111, 104)
prefadj_13 <- geomander::add_edge(prefadj_13, 111, 102)
prefadj_13 <- geomander::add_edge(prefadj_13, 111, 90)
prefadj_13 <- geomander::add_edge(prefadj_13, 171, 172)
prefadj_13 <- geomander::add_edge(prefadj_13, 401, 402)

#connect [111]品川区八潮 13109 250 to [104]品川区東品川180;[102]品川区東大井160 [90]品川区勝島40
#connect [171]大田区東海 13111 580 to [172]東京都大田区城南島590
#[401] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [402]練馬区西大泉(６丁目) 0430

pref_map_13 <- redist::redist_map(pref_13,
                                  ndists = ndists_new,
                                  pop_tol= 0.165,
                                  total_pop = pop,
                                  adj = prefadj_13)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_13split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_13 <- redist::redist_smc(pref_map_13,
                                      nsims = nsims,
                                      pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_13, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_13",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_13 <- simulation_weight_disparity_table(sim_smc_pref_13)
#n <- c(1:25000)
#wgt_smc_13 <- cbind(n, wgt_smc_13)
#wgt_smc_13$n[which(wgt_smc_13$max_to_min == min(wgt_smc_13$max_to_min))]
#Maxmin 1.3445 # 31,  91   102   112   128   190...
#redist::redist.plot.plans(sim_smc_pref_13, draws = 31, geom = pref_map_13)

#county splits
plans_pref_13 <- redist::get_plans_matrix(sim_smc_pref_13)
# get splits
splits_13 <- count_splits(plans_pref_13, pref_map_13$code)
#40 splits?


##########13 splits no. 2 #################
#find the municipality codes of the 1st ~ 13th largest municipalities
#split 中野 instead of 北
largest_13_2 <- c(largest_12, 13114)

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_13_2 <- pref_0 %>% dplyr::filter(code %in% largest_13_2 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_13_2$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_13_2 <- pref %>%
  dplyr::filter(code %in% largest_13_2) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_13_2 <- calc_kokumin(pref_split_13_2, dem_pops)
pref_split_13_2 <- estimate_2020_pop(pref_split_13_2, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_13_2 <- dplyr::bind_rows(pref_intact_13_2, pref_split_13_2)

#Ferries
ferries_13_2 <- add_ferries(pref_13_2)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_13_2 <- redist::redist.adjacency(pref_13_2)
#add edge
prefadj_13_2 <- geomander::add_edge(prefadj_13_2, ferries_13_2$V1, ferries_13_2$V2)

#manually add adjacency
prefadj_13_2 <- geomander::add_edge(prefadj_13_2, 111, 104)
prefadj_13_2 <- geomander::add_edge(prefadj_13_2, 111, 102)
prefadj_13_2 <- geomander::add_edge(prefadj_13_2, 111, 90)
prefadj_13_2 <- geomander::add_edge(prefadj_13_2, 171, 172)
prefadj_13_2 <- geomander::add_edge(prefadj_13_2, 390, 391)

#connect [111]品川区八潮 13109 250 to [104]品川区東品川180;[102]品川区東大井160 [90]品川区勝島40
#connect [171]大田区東海 13111 580 to [172]東京都大田区城南島590
#[390] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [391]練馬区西大泉(６丁目) 0430

pref_map_13_2 <- redist::redist_map(pref_13_2,
                                    ndists = ndists_new,
                                    pop_tol= 0.09,
                                    total_pop = pop,
                                    adj = prefadj_13_2)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_13split_2.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_13_2 <- redist::redist_smc(pref_map_13_2,
                                        nsims = nsims,
                                        pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_13_2, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_13_2",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_13_2 <- simulation_weight_disparity_table(sim_smc_pref_13_2)
#wgt_smc_13_2 <- cbind(n, wgt_smc_13_2)
#wgt_smc_13_2$n[which(wgt_smc_13_2$max_to_min == min(wgt_smc_13_2$max_to_min))]
#Maxmin 1.1551 #3099 21119...
#redist::redist.plot.plans(sim_smc_pref_13_2, draws = 3099, geom = pref_map_13_2)

#county splits
plans_pref_13_2 <- redist::get_plans_matrix(sim_smc_pref_13_2)
# get splits
splits_13_2 <- count_splits(plans_pref_13_2, pref_map_13_2$code)
#40 splits?

##########14 splits#################
#find the municipality codes of the 1st ~ 14th largest municipalities
largest_14 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:14]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_14 <- pref_0 %>% dplyr::filter(code %in% largest_14 == FALSE ) %>%
  merge_gun()
#run merge at this stage
pref_intact_14$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_14 <- pref %>%
  dplyr::filter(code %in% largest_14) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_14 <- calc_kokumin(pref_split_14, dem_pops)
pref_split_14 <- estimate_2020_pop(pref_split_14, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_14 <- dplyr::bind_rows(pref_intact_14, pref_split_14)

#Ferries
ferries_14 <- add_ferries(pref_14)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_14 <- redist::redist.adjacency(pref_14)
#add edge
prefadj_14 <- geomander::add_edge(prefadj_14, ferries_14$V1, ferries_14$V2)

#manually add adjacency
prefadj_14 <- geomander::add_edge(prefadj_14, 110, 103)
prefadj_14 <- geomander::add_edge(prefadj_14, 110, 101)
prefadj_14 <- geomander::add_edge(prefadj_14, 110, 89)
prefadj_14 <- geomander::add_edge(prefadj_14, 170, 171)
prefadj_14 <- geomander::add_edge(prefadj_14, 419, 420)

#connect [110]品川区八潮 13109 250 to [103]品川区東品川180;[101]品川区東大井160 [89]品川区勝島40
#connect [170]大田区東海 13111 580 to [171]東京都大田区城南島590
#[419] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [420]練馬区西大泉(６丁目) 0430

#########14 splits no.1###############
pref_map_14 <- redist::redist_map(pref_14,
                                  ndists = ndists_new,
                                  pop_tol= 0.10,
                                  total_pop = pop,
                                  adj = prefadj_14)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_14split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_14 <- redist::redist_smc(pref_map_14,
                                      nsims = nsims,
                                      pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_14, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_14",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_14 <- simulation_weight_disparity_table(sim_smc_pref_14)
#n <- c(1:25000)
#wgt_smc_14 <- cbind(n, wgt_smc_14)
#wgt_smc_14$n[which(wgt_smc_14$max_to_min == min(wgt_smc_14$max_to_min))]
#Maxmin 1.1725 # 15  5735  6892 10655 11743...
#redist::redist.plot.plans(sim_smc_pref_14, draws = 15, geom = pref_map_14)

#county splits
plans_pref_14 <- redist::get_plans_matrix(sim_smc_pref_14)
# get splits
splits_14 <- count_splits(plans_pref_14, pref_map_14$code)
#40 splits

#########14 splits no.2###############
pref_map_14_2 <- redist::redist_map(pref_14,
                                   ndists = ndists_new,
                                   pop_tol= 0.08,
                                   total_pop = pop,
                                   adj = prefadj_14)

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_14_2 <- redist::redist_smc(pref_map_14_2,
                                        nsims = nsims,
                                        pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_14_2, paste("simulation/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_14_2",
                               ".Rds",
                               sep = ""))

#pop disparity
wgt_smc_14_2 <- simulation_weight_disparity_table(sim_smc_pref_14_2)
#n <- c(1:25000)
#wgt_smc_14_2 <- cbind(n, wgt_smc_14_2)
#wgt_smc_14_2$n[which(wgt_smc_14_2$max_to_min == min(wgt_smc_14_2$max_to_min))]
#Maxmin 1.1309 #9496 19397...
#redist::redist.plot.plans(sim_smc_pref_14_2, draws = 9496, geom = pref_map_14)

#county splits
plans_pref_14_2 <- redist::get_plans_matrix(sim_smc_pref_14_2)
# get splits
splits_14_2 <- count_splits(plans_pref_14_2, pref_map_14$code)
#40 splits

##########17 splits#################
#find the municipality codes of the 1st ~ 17th largest municipalities
largest_17 <- (pref_0 %>% dplyr::arrange(desc(pop)))$code[1:17]

#filter out the municipalities to keep treat as one unit, without dividing them
pref_intact_17 <- pref_0 %>% dplyr::filter(code %in% largest_17 == FALSE ) %>%
  merge_gun()
  #run merge at this stage
pref_intact_17$subcode <- "0000"

#filter out the municipalities to split and estimate the population as of 2020
pref_split_17 <- pref %>%
  dplyr::filter(code %in% largest_17) %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref_split_17 <- calc_kokumin(pref_split_17, dem_pops)
pref_split_17 <- estimate_2020_pop(pref_split_17, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

pref_17 <- dplyr::bind_rows(pref_intact_17, pref_split_17)

#Ferries
ferries_17 <- add_ferries(pref_17)

# -------- set up for simulation ------------#
# Adjacency list
prefadj_17 <- redist::redist.adjacency(pref_17)
#add edge
prefadj_17 <- geomander::add_edge(prefadj_17, ferries_17$V1, ferries_17$V2)

#manually add adjacency
prefadj_17 <- geomander::add_edge(prefadj_17, 201, 194)
prefadj_17 <- geomander::add_edge(prefadj_17, 201, 192)
prefadj_17 <- geomander::add_edge(prefadj_17, 201, 180)
prefadj_17 <- geomander::add_edge(prefadj_17, 288, 289)
prefadj_17 <- geomander::add_edge(prefadj_17, 557, 558)

#connect [201]品川区八潮 13109 250 to [194]品川区東品川180;[192]品川区東大井160 [180]品川区勝島40
#connect [288]大田区東海 13111 580 to [289]東京都大田区城南島590
#[557] 練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to [558]練馬区西大泉(６丁目) 0430

pref_map_17 <- redist::redist_map(pref_17,
                                  ndists = ndists_new,
                                  pop_tol= 0.05,
                                  total_pop = pop,
                                  adj = prefadj_17)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_17split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_17 <- redist::redist_smc(pref_map_17,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_17, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_17",
                              ".Rds",
                              sep = ""))

#pop disparity
wgt_smc_17 <- simulation_weight_disparity_table(sim_smc_pref_17)
#n <- c(1:25000)
#wgt_smc_17 <- cbind(n, wgt_smc_17)
#wgt_smc_17$n[which(wgt_smc_17$max_to_min == min(wgt_smc_17$max_to_min))]
#Maxmin 1.0835 #14466
#redist::redist.plot.plans(sim_smc_pref_17, draws = 14466, geom = pref_map_17)

#county splits
plans_pref_17 <- redist::get_plans_matrix(sim_smc_pref_17)
# get splits
splits_17 <- count_splits(plans_pref_17, pref_map_17$code)
#48 splits?

