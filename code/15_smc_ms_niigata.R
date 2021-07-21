############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)
#remotes::install_github("alarm-redist/redist@dev")

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- information set-up -----------#
# prefectural information
sim_type <- "smc_ms"
nsims <- 25000
pref_code <- 15
pref_name <- "niigata"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 5
ndists_old <- 6

#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
merge_gun_exception <- c()  # enter `c()` if not applicable

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
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

# remove lake
ifelse(is.null(lakes_removed),
       pref <- pref,
       pref <- remove_lake(pref, lakes_removed))

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

####### Simulation by number of splits#######

intact_codes <- c()

for(i in 0:0){
  pref_n <- split_pref(pref = pref,
                       census2020 = census2020,
                       old_boundary = old_boundary,
                       pop_by_old_boundary = pop_by_old_boundary,
                       nsplit = i,
                       split_codes = split_codes,
                       intact_codes = intact_codes,
                       merge_gun_exception = merge_gun_exception)

  # new 15001 ~ 15002 district
  pref_n <- avoid_enclave(pref_n, c(15211, 15202, 15208))
  pref_n <- avoid_enclave(pref_n, c(15500, 15205))

  #------------- set up map ----------------
  # simulation parameters
  prefadj <- redist::redist.adjacency(shp = pref_n) # Adjacency list

  # add ferry if applicable
  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref_n)

    if(nrow(ferries) > 0) {
      prefadj <- geomander::add_edge(prefadj,
                                     ferries[, 1],
                                     ferries[, 2],
                                     zero = TRUE)
    }

  }

  if(length(unique((geomander::check_contiguity(prefadj))$component)) > 1) {

    suggest <-  geomander::suggest_component_connection(shp = pref_n,
                                                        adj = prefadj)
    prefadj <- geomander::add_edge(prefadj,
                                   suggest$x,
                                   suggest$y,
                                   zero = TRUE)

  }

  # define map
  pref_map <- redist::redist_map(pref_n,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj)

  ###### simulation ######
  sim_smc_pref <- redist::redist_smc(pref_map,
                                     nsims = nsims,
                                     pop_temper = 0.05)


  # save it
  saveRDS(sim_smc_pref, paste("simulation/",
                              sprintf("%02d", pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_",
                              as.character(i),
                              ".Rds",
                              sep = ""))

  # get plans
  smc_plans_pref <- redist::get_plans_matrix(sim_smc_pref)

  # get disparity data
  smc_weight_pref <- simulation_weight_disparity_table(sim_smc_pref)

  # rename elements to be used
  assign(paste(pref_name, pref_code, i, sep = "_"),
         pref_n)
  assign(paste(pref_name, pref_code, "adj", i, sep = "_"),
         prefadj)
  assign(paste(pref_name, pref_code, "map", i, sep = "_"),
         pref_map)
  assign(paste(pref_name, pref_code, "sim_smc", i, sep = "_"),
         sim_smc_pref)
  assign(paste(pref_name, pref_code, "smc_plans", i, sep = "_"),
         smc_plans_pref)
  assign(paste(pref_name, pref_code,"smc_weight", i, sep = "_"),
        smc_weight_pref)

  rm(list= ls()[(ls() %in% c("pref_n",
                             "prefadj",
                             "pref_map",
                             "sim_smc_pref",
                             "smc_plans_pref",
                             "smc_weight_pref",
                             "ferries",
                             "suggest",
                             "port_data",
                             "route_data")
  )])

}

optimal_plan <- niigata_15_smc_plans_0[, which(niigata_15_smc_weight_0$max_to_min == min(niigata_15_smc_weight_0$max_to_min))]

niigata_15_orig <- split_pref(pref = pref,
                              census2020 = census2020,
                              old_boundary = old_boundary,
                              pop_by_old_boundary = pop_by_old_boundary,
                              nsplit = 0,
                              split_codes = split_codes,
                              intact_codes = intact_codes,
                              merge_gun_exception = merge_gun_exception)

split_codes <- intersect(niigata_15_orig$code, as.numeric(unique(pop_by_old_boundary %>% dplyr::filter(X.1 == 9) %>% dplyr::select(X))$X))

nsplit <- length(split_codes)

for(i in c(nsplit)){
  pref_n <- split_pref(pref = pref,
                       census2020 = census2020,
                       old_boundary = old_boundary,
                       pop_by_old_boundary = pop_by_old_boundary,
                       nsplit = i,
                       split_codes = split_codes,
                       intact_codes = intact_codes,
                       merge_gun_exception = merge_gun_exception)

  pref_n <- avoid_enclave(pref_n, c(15211, 15215))
  pref_n <- avoid_enclave(pref_n, c(15500, 15505))

  #------------- set up map ----------------
  # simulation parameters
  prefadj <- redist::redist.adjacency(shp = pref_n) # Adjacency list

  # add ferry if applicable
  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref_n)

    if(nrow(ferries) > 0) {
      prefadj <- geomander::add_edge(prefadj,
                                     ferries[, 1],
                                     ferries[, 2],
                                     zero = TRUE)
    }

  }

  if(length(unique((geomander::check_contiguity(prefadj))$component)) > 1) {

    suggest <-  geomander::suggest_component_connection(shp = pref_n,
                                                        adj = prefadj)
    prefadj <- geomander::add_edge(prefadj,
                                   suggest$x,
                                   suggest$y,
                                   zero = TRUE)

  }



  # define map
  pref_map <- redist::redist_map(pref_n,
                                 ndists = ndists_new,
                                 pop_tol = redist::redist.parity(optimal_plan, total_pop = niigata_15_0$pop),
                                 total_pop = pop,
                                 adj = prefadj)


  # establish keys to map 0-split, 1-split plans to 2-split plans
  key <- vector(length = length(pref_n$code))

  old_codes <-
    list(find_old_codes(split_codes[1], pop_by_old_boundary))

  for(i in 2:length(split_codes)) {
    old_codes <- append(old_codes, list(find_old_codes(split_codes[i], pop_by_old_boundary)))
  }

  for (i in 1:length(pref_n$code)) {
    for (j in 1:length(split_codes)) {
      if (pref_n$code[i] %in% old_codes[[j]]) {key[i] <- split_codes[j]}
    }
    if (pref_n$code[i] == 15001) {key[i] <- 15202}
    else if (pref_n$code[i] == 15002) {key[i] <- 15205}
    if (key[i] == FALSE) {key[i] <- pref_n$code[i]}
  }

  init_plan_vec <- vector(length = nrow(pref_n))

  for(i in 1:nrow(pref_n)) {
    if(length(which(niigata_15_0$code == key[i])) > 0) {init_plan_vec[i] <- optimal_plan[which(niigata_15_0$code == key[i])]}
    else if (key[i] %in% c(15205, 15500)) {init_plan_vec[i] <- optimal_plan[which(niigata_15_0$code == 15002)]}
    else if (key[i] %in% c(15211, 15202, 15208)) {init_plan_vec[i] <- optimal_plan[which(niigata_15_0$code == 15001)]}
  }

  ###### simulation ######
  sim_smc_pref <- redist::redist_mergesplit(pref_map,
                                            nsims = nsims,
                                            warmup = 0,
                                            init_plan = init_plan_vec,
                                            counties = key,
                                            constraints = list(splits = 10))

  # save it
  saveRDS(sim_smc_pref, paste("simulation/",
                              sprintf("%02d", pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_",
                              as.character(i),
                              ".Rds",
                              sep = ""))

  # get plans
  smc_plans_pref <- redist::get_plans_matrix(sim_smc_pref)

  # get disparity data
  smc_weight_pref <- simulation_weight_disparity_table(sim_smc_pref)

  # rename elements to be used
  assign(paste(pref_name, pref_code, i, sep = "_"),
         pref_n)
  assign(paste(pref_name, pref_code, "adj", i, sep = "_"),
         prefadj)
  assign(paste(pref_name, pref_code, "map", i, sep = "_"),
         pref_map)
  assign(paste(pref_name, pref_code, "sim_smc", i, sep = "_"),
         sim_smc_pref)
  assign(paste(pref_name, pref_code, "smc_plans", i, sep = "_"),
         smc_plans_pref)
  assign(paste(pref_name, pref_code,"smc_weight", i, sep = "_"),
         smc_weight_pref)

  rm(list= ls()[(ls() %in% c("pref_n",
                             "prefadj",
                             "pref_map",
                             "sim_smc_pref",
                             "smc_plans_pref",
                             "smc_weight_pref",
                             "ferries",
                             "suggest",
                             "port_data",
                             "route_data")
  )])

}

niigata_15_splits <- count_splits(niigata_15_smc_plans_99, key)

niigata_15_goodindices <- which(niigata_15_splits <= 2)
niigata_15_goodmaps <- niigata_15_smc_plans_99[, niigata_15_goodindices]
niigata_15_bestmap <- niigata_15_goodmaps[, which(niigata_15_smc_weight_99$max_to_min[niigata_15_goodindices] == min(niigata_15_smc_weight_99$max_to_min[niigata_15_goodindices]))]

redist::redist.plot.map(niigata_15_99, plan = niigata_15_bestmap[,1], boundaries = FALSE)


niigata_15_orig_map <- status_quo_match(niigata_15_2)

niigata_15_orig_weights <- simulation_weight_disparity_table(redist::redist_plans(niigata_15_orig_map$ku, niigata_15_map_2, algorithm = "smc"))

niigata_15_cooccurence_0 <- redist::prec_cooccurrence(niigata_15_sim_smc_0[which(niigata_15_smc_weight_0$max_to_min < 1.20), ])

heatmap(niigata_15_cooccurence_0, scale = "column")

niigata_15_significance_0 <- niigata_15_cooccurence_0
niigata_15_significance_0[which(niigata_15_significance_0 < 0.7)] <- 0

niigata_15_graph_0 <- igraph::graph.adjacency(niigata_15_significance_0,
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)

plot(niigata_15_graph_0,
     vertex.label = substr(niigata_15_0$code, 3, 5),
     vertex.size = (niigata_15_0$pop)/max((niigata_15_0$pop)) * 30,
     edge.width=igraph::E(niigata_15_graph_0)$weight^4 * 5,
     layout = igraph::layout_with_fr(niigata_15_graph_0))

redist::redist.plot.map(niigata_15_0)

niigata_15_components_0 <- igraph::components(niigata_15_graph_0)

niigata_15_clusterindex_0 <- which(niigata_15_components_0$csize > 1)

niigata_15_colorclusters_0 <- vector(length = nrow(niigata_15_0))

for(i in 1:nrow(niigata_15_0)) {
  ifelse(niigata_15_components_0$membership [i] %in% niigata_15_clusterindex_0,
         niigata_15_colorclusters_0[i] <- which(niigata_15_clusterindex_0 == niigata_15_components_0$membership[i]),
         niigata_15_colorclusters_0[i] <- length(niigata_15_clusterindex_0) + 1)
}

redist::redist.plot.map(shp = niigata_15_0,
                plan = niigata_15_colorclusters_0
) + scale_fill_manual(values = c("red", "green", "blue", "yellow", "gray"))

niigata_15_centroids_0 <- sf::st_centroid(niigata_15_0)






