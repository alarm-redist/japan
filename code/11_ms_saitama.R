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
nsims <- 25000
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
#select municipalities whose population is less than 50% of target pop -> keep intact
pref_small <- pref_0 %>% dplyr::filter(pop < 0.50 * sum(pref_0$pop)/ndists_new)
pref_small$subcode <- "0000"
#select municipalities whose population is more than 50% of target pop -> separate
large_codes <- pref_0$code[which(pref_0$code %in% pref_small$code == FALSE)]
pref_large <- pref %>% dplyr::filter(code %in% large_codes)
#bind together
pref_50 <- dplyr::bind_rows(pref_small, pref_large)

############Simulation Prep########################
#adjacency list
prefadj <- redist::redist.adjacency(pref_50)

neighbor <- geomander::suggest_neighbors(shp = pref_50,
                                           adjacency = prefadj)
if(nrow(neighbor) > 0){
  prefadj <- geomander::add_edge(prefadj,
                                 neighbor$x,
                                 neighbor$y,
                                 zero = TRUE)
  }

pref_map <- redist::redist_map(pref_50,
                               ndists = round(sum(pref_50$pop)/(sum(pref_50$pop)/ndists_new)),
                               pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                               total_pop = pop,
                               adj = prefadj)

pref_ms <- redist::redist_mergesplit(
  map = pref_map,
  nsims = 250000,
  counties = pref_50$code,
  warmup = 0,
  constraints = list(fractures = list(strength = 4),
                     splits = list(strength = 4))
  )
i <- 1
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
                                  "block",
                                  i,
                                  "_",
                                  "parallel.Rds",
                                  sep = ""))



# get disparity data
weight_pref <- simulation_weight_disparity_table(part_parallel_pref)
plans_pref <- redist::get_plans_matrix(part_parallel_pref)

# get splits
part_parallel_splits <- count_splits(part_parallel_plans_pref, part_map$code)
part_parallel_countiessplit <- redist::redist.splits(part_parallel_plans_pref, part_map$code)

parallel_results <- data.frame(matrix(ncol = 0, nrow = nrow(part_parallel_weight_pref)))
parallel_results$max_to_min <- part_parallel_weight_pref$max_to_min
parallel_results$splits <- part_parallel_splits
parallel_results$counties_split <- part_parallel_countiessplit
parallel_results$index <- 1:nrow(part_parallel_weight_pref)

### fix this one!!!
block_codes <- c(rep(1, length(which(part_codes <= 11110))),
                 rep(2, length(which(part_codes > 11110))))

block_div <- block_codes[match(part_map$code, pref_block$code)]

#### Check this!!!!!
parallel_results$cross <- count_overlap(part_parallel_plans_pref, block_div)

parallel_results <- parallel_results %>%
  dplyr::group_by(max_to_min, splits, counties_split, cross) %>%
  dplyr::summarise(index = first(index)) %>%
  dplyr::arrange(splits)

# rename elements to be used
assign(paste(pref_name, pref_code, "full", i, sep = "_"),
       pref_part)
assign(paste(pref_name, pref_code, "adj", "full", i, sep = "_"),
       part_adj)
assign(paste(pref_name, pref_code, "map", "full", i, sep = "_"),
       part_map)
assign(paste(pref_name, pref_code, "sim", sim_type, "full", i, sep = "_"),
       part_parallel_pref)
assign(paste(pref_name, pref_code, sim_type, "plans", "full", i, sep = "_"),
       part_parallel_plans_pref)
assign(paste(pref_name, pref_code, sim_type, "results", "full", i, sep = "_"),
       parallel_results)


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


min(parallel_results$max_to_min[which(parallel_results$splits == parallel_results$counties_split)])

