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
merge_gun_exception <- c() #西多摩郡, 大島支庁,三宅支庁, 八丈支庁, 小笠原支庁 are treated as gun

######### Download and Clean Data ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code)

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

#########Clean data: pref################
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

#######Setagaya###############
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

#######Clean data: 23-ku###############
urban <- pref %>%
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
#[92] 13102 0340 中央区佃
#[93] 13102 0350 中央区月島
#[94] 13102 0360 中央区勝どき
#[95] 13102 0370 中央区豊海町
urbanadj <- geomander::add_edge(urbanadj, 92, 70) #[70] 13102 110 中央区新川
urbanadj <- geomander::add_edge(urbanadj, 92, 66) #[66] 13102 0070 中央区明石町
urbanadj <- geomander::add_edge(urbanadj, 92, 93)
urbanadj <- geomander::add_edge(urbanadj, 93, 94)
urbanadj <- geomander::add_edge(urbanadj, 94, 67)  #[67] 13102 0080 中央区築地
urbanadj <- geomander::add_edge(urbanadj, 94, 95)

#[96] 13102 0380 中央区晴海
urbanadj <- geomander::add_edge(urbanadj, 96, 93)
urbanadj <- geomander::add_edge(urbanadj, 96, 94)
urbanadj <- geomander::add_edge(urbanadj, 96, 320) #[320] 13108 210 江東区　豊洲


#[369]13109 0250 品川区八潮
#[456]13111 0580 大田区東海
#[457]13111 0590 大田区城南島
urbanadj <- geomander::add_edge(urbanadj, 369, 362) #[362]品川区東品川180
urbanadj <- geomander::add_edge(urbanadj, 369, 360) #[360]品川区東大井160
urbanadj <- geomander::add_edge(urbanadj, 369, 348) #[348]品川区勝島40
urbanadj <- geomander::add_edge(urbanadj, 456, 457)
urbanadj <- geomander::add_edge(urbanadj, 456, 407) #[407] 13111 0090大田区平和島

#[703]13120 0420 練馬区西大泉町
urbanadj <- geomander::add_edge(urbanadj, 703, 704) #connect to [704]練馬区西大泉(６丁目) 0430

urban_map <- redist::redist_map(urban,
                                ndists = 19,
                                pop_tol= 0.09,
                                total_pop = pop,
                                adj = urbanadj)

#######Clean data: Tama###############
rural <-  pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115",
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420") == FALSE)

#adjacency list
ruraladj <- redist::redist.adjacency(rural)

#save(list=ls(all=TRUE), file="13_smc_tokyo_data_by_region_0_freeze.Rdata")

################23-ku simulation############################
#load data
load("~/R/Tokyo/by_region/13_smc_tokyo_data_by_region.Rdata")

#maps
urban_map <- redist::redist_map(urban,
                                ndists = 19,
                                pop_tol= 0.05,
                                total_pop = pop,
                                adj = urbanadj)

# --------- SMC simulation ----------------#
#run simulation
urban_smc <- redist::redist_smc(urban_map,
                                nsims = 250000,
                                counties = urban_map$code,
                                constraints = list(multisplits = list(strength = 60)),
                                pop_temper = 0.05
)

#save results
saveRDS(urban_smc, paste("~/R/Tokyo/by_region/",
                         "13",
                         "_",
                         "urban",
                         "_",
                         "smc",
                         "_",
                         "25000",
                         ".Rds",
                         sep = ""))

#########diagonistics:urban#########
load("~/Desktop/ALARM Project/jcdf/13_smc_tokyo_data_by_region_0_freeze.Rdata")
urban_smc <- readRDS("~/Desktop/ALARM Project/Tokyo Results/tmux/SMC/SamePlan?/0.09-9000-1000/[0.09-9000]13_urban_smc_1000.Rds")

wgt_smc_urban <- simulation_weight_disparity_table(urban_smc)
m <- c(1:1000)
wgt_smc_urban <- cbind(m, wgt_smc_urban)

#minimum max:min ratio
min(wgt_smc_urban$max_to_min)

#code for optimal plan
minimum_maxmin <- wgt_smc_urban$m[which(wgt_smc_urban$max_to_min == min(wgt_smc_urban$max_to_min))][1]

plans_pref_urban <- redist::get_plans_matrix(urban_smc)

#count the number of splits
splits_urban <- count_splits(plans_pref_urban, urban_map$code)
splits_urban[minimum_maxmin]

#count the number of municipalities that are split
csplits_urban <- redist::redist.splits(plans_pref_urban, urban_map$code)
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

redist::redist.plot.plans(urban_smc, draws = 114, geom = urban_map)



