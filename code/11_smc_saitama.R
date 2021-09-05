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
sim_type <- "smc"
nsims <- 250000
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

############Simulation Prep########################
#adjacency list
prefadj <- redist::redist.adjacency(pref)

neighbor <- geomander::suggest_neighbors(shp = pref,
                                         adjacency = prefadj)
if(nrow(neighbor) > 0){
  prefadj <- geomander::add_edge(prefadj,
                                 neighbor$x,
                                 neighbor$y,
                                 zero = TRUE)
}

pref_map <- redist::redist_map(pref,
                               ndists = round(sum(pref$pop)/(sum(pref$pop)/ndists_new)),
                               pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                               total_pop = pop,
                               adj = prefadj)

pref_smc <- redist::redist_smc(pref_map,
                              nsims = nsims,
                              counties = code,
                              constraints = list(multisplits = list(strength = 50))
                              #pop_temper = 0.05
)

# save it
saveRDS(pref_smc, paste("simulation/",
                       sprintf("%02d", pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(sim_type),
                       "_",
                       as.character(nsims),
                       ".Rds",
                       sep = ""))


# get disparity data
weight_pref <- simulation_weight_disparity_table(pref_smc)
plans_pref <- redist::get_plans_matrix(pref_smc)

# get splits
pref_smc_splits <- count_splits(plans_pref, pref_map$code)
pref_smc_countiessplit <- redist::redist.splits(plans_pref, pref_map$code)

pref_smc_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref)))
pref_smc_results$max_to_min <- weight_pref$max_to_min
pref_smc_results$splits <- pref_smc_splits
pref_smc_results$counties_split <- pref_smc_countiessplit
pref_smc_results$index <- 1:nrow(weight_pref)

pref_smc_results <- pref_smc_results %>%
  dplyr::group_by(max_to_min, splits, counties_split) %>%
  dplyr::summarise(index = first(index)) %>%
  dplyr::arrange(splits)

i <- 1

# rename elements to be used
assign(paste(pref_name, pref_code, "full", i, sep = "_"),
       pref)
assign(paste(pref_name, pref_code, "adj", "full", i, sep = "_"),
       prefadj)
assign(paste(pref_name, pref_code, "map", "full", i, sep = "_"),
       pref_map)
assign(paste(pref_name, pref_code, "sim", sim_type, "full", i, sep = "_"),
       pref_smc)
assign(paste(pref_name, pref_code, sim_type, "plans", "full", i, sep = "_"),
       plans_pref)
assign(paste(pref_name, pref_code, sim_type, "results", "full", i, sep = "_"),
       pref_smc_results)

min(pref_smc_results$max_to_min[which(pref_smc_results$splits == pref_smc_results$counties_split)])

saitama_11_smc_results_full_1 %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == counties_split) %>%
  dplyr::arrange(max_to_min)
