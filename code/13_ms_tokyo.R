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
sim_type <- "ms"
pref_code <- 13
pref_name <- "tokyo"
ndists_new <- 30
ndists_old <- 25

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

#########小地域-level data (estimate 2020 pop)################
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

##############Setagaya########################
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
                            sep = "")) #saved in jcdf/simulation

###############Data cleaning (Freeze small municipalities)###################
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

############23-ku simulation prep########################
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

################Tama simulation prep########################
rural <-  pref_50 %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115",
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420") == FALSE)

#adjacency list
ruraladj <- redist::redist.adjacency(rural)

#save(list=ls(all=TRUE), file="13_smc_tokyo_data_by_region.Rdata")

################23-ku simulation############################
#load data
load("~/R/Tokyo/by_region/13_smc_tokyo_data_by_region.Rdata")

#maps
urban_map <- redist::redist_map(urban,
                                ndists = 19,
                                pop_tol= 0.05,
                                total_pop = pop,
                                adj = urbanadj)

# --------- MS simulation ----------------#
#run simulation
urban_ms <- redist::redist_mergesplit(urban_map,
                                      nsims =  250000,
                                      counties = urban_map$code,
                                      warmup = 0,
                                      constraints = list(multissplits = list(strength = 15),
                                                         splits = list(strength = 4))
)

#save results
saveRDS(urban_ms, paste("~/R/Tokyo/by_region/",
                        "13",
                        "_",
                        "urban",
                        "_",
                        "ms",
                        "_",
                        "25000",
                        ".Rds",
                        sep = "")) #saved in Azure -> downloaded as "13_urban_ms_250000.Rds"

urban_ms <- readRDS("~/Desktop/ALARM Project/Tokyo Results/tmux/[0.05-15-4]13_urban_ms_250000.Rds")

#########diagonistics:urban#########
wgt_ms_urban <- simulation_weight_disparity_table(urban_ms)
m <- c(1:250001)
wgt_ms_urban <- cbind(m, wgt_ms_urban)

#minimum max:min ratio
min(wgt_ms_urban$max_to_min)

#code for optimal plan
minimum_maxmin <- wgt_ms_urban$m[which(wgt_ms_urban$max_to_min == min(wgt_ms_urban$max_to_min))][1]

plans_pref_urban <- redist::get_plans_matrix(urban_ms)

#count the number of splits
splits_urban <- count_splits(plans_pref_urban, urban$code)
splits_urban[minimum_maxmin]

#count the number of municipalities that are split
csplits_urban <- redist::redist.splits(plans_pref_urban, urban$code)
csplits_urban[minimum_maxmin]

#minimum number of splits; county splits
min(splits_urban)
min(csplits_urban)

results_urban <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_ms_urban)))
results_urban$max_to_min <- wgt_ms_urban$max_to_min
results_urban$splits <- splits_urban
results_urban$counties_split <- csplits_urban
results_urban$index <- 1:nrow(wgt_ms_urban)
results_urban$dif <-  results_urban$splits - results_urban$counties_split

min(results_urban$max_to_min[which(results_urban$splits == results_urban$counties_split)])
results_urban$index[which(results_urban$splits == results_urban$counties_split)]

redist::redist.plot.plans(urban_ms, draws = 2, geom = urban_map)

#draw: 2; maxmin 1.073158; splits 19; csplits 17; dif 2

