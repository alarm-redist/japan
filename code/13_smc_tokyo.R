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

pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)


##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref

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

#save(list=ls(all=TRUE), file="13_smc_tokyo_data_0split.Rdata")

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


########Analysis###############
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
