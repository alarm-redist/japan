############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)
#remotes::install_github("alarm-redist/redist@dev")
library(redist)
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- information set-up -----------#
# prefectural information
sim_type <- "ms"
nsims <- 500000
pref_code <- 11
pref_name <- "saitama"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 16
ndists_old <- 15
sq_maxmin <- 1.444
#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
# Iruma and , 11326, 11327
merge_gun_exception <- c(11324)  # enter `c()` if not applicable

######### Download and Clean Census ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code) #first download data

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")

# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

#clean pref_raw
pref <- pref_raw %>%
  clean_jcdf()
pref <- pref %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
#Estimate 2020 pop.
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

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
pref_0 <- merge_gun(pref_0,
                    exception = merge_gun_exception)
ranking <- pref_0 %>%
  dplyr::arrange(desc(pop))
#select municipalities whose population is less than 33% of target pop -> keep intact
pref_small <- pref_0 %>%
  dplyr::filter(pop < 0.33 * sum(pref_0$pop)/ndists_new)
pref_small$subcode <- "0000"
#select municipalities whose population is more than 33% of target pop -> separate
large_codes <- pref_0$code[which(pref_0$code %in% pref_small$code == FALSE)]
pref_large <- pref %>%
  dplyr::filter(code %in% large_codes)
#bind together
pref_33 <- dplyr::bind_rows(pref_small, pref_large)

############Simulation Prep########################
#adjacency list
prefadj <- redist::redist.adjacency(pref_33)

neighbor <- geomander::suggest_neighbors(shp = pref_33,
                                         adjacency = prefadj)
if(nrow(neighbor) > 0){
  prefadj <- geomander::add_edge(prefadj,
                                 neighbor$x,
                                 neighbor$y,
                                 zero = TRUE)
}

pref_map <- redist::redist_map(pref_33,
                               ndists = round(sum(pref_33$pop)/(sum(pref_33$pop)/ndists_new)),
                               pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                               total_pop = pop,
                               adj = prefadj)

pref_ms <- redist::redist_mergesplit(
  map = pref_map,
  nsims = nsims,
  counties = pref_33$code,
  warmup = 0,
  constraints = list(multissplits = list(strength = 30),
                     splits = list(strength = 7))
)

i <- 33
# save it
saveRDS(pref_ms, paste("simulation/",
                       sprintf("%02d", pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(sim_type),
                       "_",
                       as.character(nsims),
                       "_",
                       i,
                       "percent",
                       "_",
                       "pref_ms.Rds",
                       sep = ""))



# get disparity data
weight_pref <- simulation_weight_disparity_table(pref_ms)
plans_pref <- redist::get_plans_matrix(pref_ms)

# get splits
pref_ms_splits <- count_splits(plans_pref, pref_map$code)
pref_ms_countiessplit <- redist::redist.splits(plans_pref, pref_map$code)

pref_ms_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref)))
pref_ms_results$max_to_min <- weight_pref$max_to_min
pref_ms_results$splits <- pref_ms_splits
pref_ms_results$counties_split <- pref_ms_countiessplit
pref_ms_results$index <- 1:nrow(weight_pref)

pref_ms_results <- pref_ms_results %>%
  dplyr::group_by(max_to_min, splits, counties_split) %>%
  dplyr::summarise(index = first(index)) %>%
  dplyr::arrange(splits)

# rename elements to be used
assign(paste(pref_name, pref_code, "full", i, sep = "_"),
       pref_33)
assign(paste(pref_name, pref_code, "adj", "full", i, sep = "_"),
       prefadj)
assign(paste(pref_name, pref_code, "map", "full", i, sep = "_"),
       pref_map)
assign(paste(pref_name, pref_code, "sim", sim_type, "full", i, sep = "_"),
       pref_ms)
assign(paste(pref_name, pref_code, sim_type, "plans", "full", i, sep = "_"),
       plans_pref)
assign(paste(pref_name, pref_code, sim_type, "results", "full", i, sep = "_"),
       pref_ms_results)


rm(list= ls()[(ls() %in% c("pref_part",
                           "part_adj",
                           "part_map",
                           "part_smc_pref",
                           "part_plans_pref",
                           "part_weight_pref",
                           "ferries",
                           "suggest",
                           "port_data",
                           "route_data",
                           "part_splits"
))])


min(pref_ms_results$max_to_min[which(pref_ms_results$splits == pref_ms_results$counties_split)])

saitama_11_ms_results_full_33 %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == counties_split) %>%
  dplyr::arrange(max_to_min)

