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

pref <- pref_raw %>%
  clean_jcdf()

# remove lake
ifelse(is.null(lakes_removed),
       pref <- pref,
       pref <- remove_lake(pref, lakes_removed))

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

####### Simulation by number of splits#######

pref_block <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

pref_block <- merge_gun(pref_block)
pref_block$subcode = "0000"
block_adj <- redist::redist.adjacency(pref_block)

###### simulation ######
small_units <- pref %>% dplyr::select(code, KIHON1, JINKO, geometry)
small_units <- calc_kokumin(small_units, dem_pops)
small_units <- estimate_2020_pop(small_units, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#------------- set up map ----------------
part_codes <- pref_block$code
muni_codes <- unique(small_units$code)

# Saitama-shi
block_codes <- list(part_codes[which(part_codes <= 11110)],
                    part_codes[which(part_codes > 11110)])

largest_x <- intersect(pref_block$code[which(pref_block$pop >= 1/3 * sum(pref_block$pop)/ndists_new)], muni_codes)


save(list=ls(all=TRUE), file="code/saitama_functions.Rdata")



i <- 1

  pref_part <- dplyr::bind_rows(small_units %>%
                                  dplyr::filter(code %in% largest_x &
                                                  code %in% c(block_codes[[i]], block_codes[[i+1]])),
                                pref_block %>%
                                  dplyr::filter(!(code %in% largest_x) &
                                                  code %in% c(block_codes[[i]], block_codes[[i+1]])))
  part_adj <- redist::redist.adjacency(shp = pref_part) # Adjacency list

  neighbor <- geomander::suggest_neighbors(shp = pref_part,
                                           adjacency = part_adj)
  if(nrow(neighbor) > 0) {

    part_adj <- geomander::add_edge(part_adj,
                                    neighbor$x,
                                    neighbor$y,
                                    zero = TRUE)
  }

  #while(length(unique((geomander::check_contiguity(part_adj))$component)) > 1) {

   # suggest <- geomander::suggest_component_connection(shp = pref_part,
   #                                                    adjacency = part_adj,
    #                                                   group = match(pref_part$code, unique(pref_part$code)))

  #  part_adj <- geomander::add_edge(part_adj,
     #                               suggest$x,
      #                              suggest$y,
       #                             zero = TRUE)

  #}

  part_map <- redist::redist_map(pref_part,
                                 ndists = round(sum(pref_part$pop)/(sum(pref_block$pop)/ndists_new)),
                                 pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                                 total_pop = pop,
                                 adj = part_adj)

  # init_smc_pref <- redist::redist_smc(
  #   map = part_map,
  #   nsims = 100,
  #   counties = pref_part$code,
  #   pop_temper = 0.05
  # )
  #
  # init_plan_vec <- redist::get_plans_matrix(init_smc_pref)
  #
  # part_parallel_pref <- redist::redist_mergesplit_parallel(
  #   map = part_map,
  #   nsims = 5000,
  #   chains = ncol(init_plan_vec),
  #   warmup = 0,
  #   init_plan = init_plan_vec,
  #   counties = pref_part$code,
  #   constraints = list(fractures = list(strength = 5), splits = list(strength = 5)),
  #   verbose = FALSE
  # )

  part_parallel_pref <- redist::redist_mergesplit(
    map = part_map,
    nsims = 250000,
    counties = pref_part$code,
    warmup = 0,
    constraints = list(fractures = list(strength = 4),
                       splits = list(strength = 4))
  )

  # save it
  saveRDS(part_parallel_pref, paste("simulation/",
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
  part_parallel_weight_pref <- simulation_weight_disparity_table(part_parallel_pref)
  part_parallel_plans_pref <- redist::get_plans_matrix(part_parallel_pref)

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

