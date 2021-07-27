############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)
remotes::install_github("alarm-redist/redist@dev")

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
pref_code <- 14
pref_name <- "kanagawa"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 20
ndists_old <- 18

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

intact_codes <- c()

nsplit <- length(split_codes)

n_blocks <- 4

pref_block <- pref %>%
    dplyr::group_by(code, CITY_NAME) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    dplyr::left_join(census2020, by = c('code')) %>%
    dplyr::rename(pop = pop_national) %>%
    dplyr::select(code, pop, geometry)
  pref_block$subcode = "0000"

blockadj <- redist::redist.adjacency(shp = pref_block)

block_map <- redist::redist_map(pref_block,
                                 ndists = n_blocks,
                                 pop_tol = 0.1,
                                 total_pop = pop,
                                 adj = blockadj)

smc_block <- redist::redist_smc(map = block_map, nsims = 25000)

block_plans_pref <- redist::get_plans_matrix(smc_block)

block_weight_pref <- simulation_weight_disparity_table(smc_block)

kanagawa_blocks <- block_plans_pref[, which(block_weight_pref$max_to_min == min(block_weight_pref$max_to_min))[1]]

###### simulation ######
small_units <- pref %>% dplyr::select(code, KIHON1, JINKO, geometry)
small_units <- calc_kokumin(small_units, dem_pops)
small_units <- estimate_2020_pop(small_units, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

x <- 5

#------------- set up map ----------------

for (j in 1:n_blocks) {

  part_codes <- pref_block$code[which(kanagawa_blocks == j)]
  largest_x <- (pref_block[order(-pref_block$pop),] %>% dplyr::filter(code %in% part_codes))$code[1:x]

  pref_part <- dplyr::bind_rows(small_units %>% dplyr::filter(code %in% largest_x), pref_block %>% dplyr::filter(!(code %in% largest_x) & code %in% part_codes))

  part_adj <- redist::redist.adjacency(shp = pref_part) # Adjacency list

    " add ferry if applicable
    if(check_ferries(pref_code) == TRUE){
      # add ferries
      # ferries <- add_ferries(pref_part)

      if(nrow(ferries) > 0) {
        part_adj <- geomander::add_edge(part_adj,
                                       ferries[, 1],
                                       ferries[, 2],
                                       zero = TRUE)
      }

    }"

  neighbor <- geomander::suggest_neighbors(shp = pref_part,
                                             adjacency = part_adj)
  if(nrow(neighbor) > 0) {

    part_adj <- geomander::add_edge(part_adj,
                                    neighbor$x,
                                    neighbor$y,
                                    zero = TRUE)
  }

  if(length(unique((geomander::check_contiguity(part_adj))$component)) > 1) {

    suggest <- geomander::suggest_component_connection(shp = pref_part,
                                                        adjacency = part_adj,
                                                        group = match(pref_part$code, unique(pref_part$code)))

    part_adj <- geomander::add_edge(part_adj,
                                    suggest$x,
                                    suggest$y,
                                    zero = TRUE)

  }

  part_map <- redist::redist_map(pref_part,
                                  ndists = ndists_new/n_blocks,
                                  pop_tol = 0.35,
                                  total_pop = pop,
                                  adj = part_adj)

  init_plan <- redist::redist_smc(map = part_map,
                                  nsims = 1,
                                  pop_temper = 0.05)

  init_plan_vec <- redist::get_plans_matrix(init_plan)[,1]

  part_map <- redist::redist_map(pref_part,
                                 ndists = ndists_new/n_blocks,
                                 pop_tol = 0.15,
                                 total_pop = pop,
                                 adj = part_adj)

  multiple <- 2
  part_smc_pref <- redist::redist_shortburst(map = part_map,
                                             score_fn = redist::scorer_pop_dev(part_map) + multiple*redist::scorer_splits(part_map, pref_part$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 2500,
                                             counties = pref_part$code,
                                             init_plan = init_plan_vec)

  # save it
  saveRDS(part_smc_pref, paste("simulation/",
                              sprintf("%02d", pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_",
                              "block_",
                              as.character(j),
                              ".Rds",
                              sep = ""))

  # get plans, remove init
  part_plans_pref <- redist::get_plans_matrix(part_smc_pref)

  # get disparity data
  part_weight_pref <- simulation_weight_disparity_table(part_smc_pref)

  # get splits
  part_splits <- count_splits(part_plans_pref, part_map$code)

  # rename elements to be used
  assign(paste(pref_name, pref_code, "block", j, sep = "_"),
           pref_part)
  assign(paste(pref_name, pref_code, "adj", "block", j, sep = "_"),
           part_adj)
  assign(paste(pref_name, pref_code, "map", "block", j, sep = "_"),
           part_map)
  assign(paste(pref_name, pref_code, "sim", sim_type, "block", j, sep = "_"),
           part_smc_pref)
  assign(paste(pref_name, pref_code, sim_type, "plans", "block", j, sep = "_"),
           part_plans_pref)
  assign(paste(pref_name, pref_code, sim_type, "weight", "block", j, sep = "_"),
           part_weight_pref)
  assign(paste(pref_name, pref_code, sim_type, "splits", "block", j, sep = "_"),
           part_splits)

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
                            )
  )])

}

kanagawa_14_results <- data.frame(matrix(ncol = 0, nrow = n_blocks*nrow(kanagawa_14_ms_weight_block_1)))
kanagawa_14_results$block <- c(rep(1, nrow(kanagawa_14_ms_weight_block_1)),
                               rep(2, nrow(kanagawa_14_ms_weight_block_2)),
                               rep(3, nrow(kanagawa_14_ms_weight_block_3)),
                               rep(4, nrow(kanagawa_14_ms_weight_block_4)))
kanagawa_14_results$index <- rep(1:nrow(kanagawa_14_ms_weight_block_1), n_blocks)
kanagawa_14_results$max_to_min <- c(kanagawa_14_ms_weight_block_1$max_to_min,
                                    kanagawa_14_ms_weight_block_2$max_to_min,
                                    kanagawa_14_ms_weight_block_3$max_to_min,
                                    kanagawa_14_ms_weight_block_4$max_to_min)
kanagawa_14_results$splits <- c(kanagawa_14_ms_splits_block_1,
                                kanagawa_14_ms_splits_block_2,
                                kanagawa_14_ms_splits_block_3,
                                kanagawa_14_ms_splits_block_4)

kanagawa_14_uniques <- kanagawa_14_results %>%
  dplyr::group_by(block, splits) %>%
  dplyr::summarize(max_to_min = min(max_to_min))

# find the best zero-split maps, too!

orig_splits <- 6

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

