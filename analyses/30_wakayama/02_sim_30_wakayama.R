###############################################################################
# Simulations for `30_wakayama`
# © ALARM Project, March 2022
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####
# Add information about 郡
pref <- merge_gun(pref)

# Define pref_0
pref_0 <-  sf::st_as_sf(
  dplyr::bind_rows(

    # Set aside gun that are not respected under the status quo
    pref %>% filter(gun_code %in% as.numeric(gun_exception)),

    # Merge gun
    pref %>%
      dplyr::filter(gun_code %in% gun_exception == FALSE) %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = code[1])
  )
)

# Add adjacency
# Make adjacency list
# There are no edges to add as there are no areas disconnected from the mainland
# Note: 友ヶ島 is uninhabited.
# Note: 北山村 and 新宮市飛地, where surrounded by other prefecture, are treated as an island,
# but 北山村 is merged together with 東牟婁郡, and 新宮市飛地 is merged with 新宮市 in our analysis,
# as it is treated under status quo.
# Thus, we treat them as adjacent to 新宮市, which we don't have to add edges in this data pre-processing.
# Therefore, we simply make a list without ferries.
prefadj_0 <- redist::redist.adjacency(pref_0)

# Optional: Suggest connection between disconnected groups
"suggest <-  geomander::suggest_component_connection(shp = pref_n,
                                                    adj = prefadj_n)
prefadj_n <- geomander::add_edge(prefadj_n,
                                 suggest$x,
                                 suggest$y,
                                 zero = TRUE)"


# TODO Repair adjacencies if necessary, and document these changes.
# prefadj_x <- geomander::add_edge(prefadj_x,
# which(pref_x$pre_gappei_code == xxxxx),
# which(pref_x$pre_gappei_code == xxxxx))

# Run simulations
run_simulations <- function(pref_n, prefadj_n){

  # 0 split or 1 split
  if("pre_gappei_code" %in% colnames(pref_n)){
    i <- 1
  }else{
    i <- 0
  }

  # Create redist.map object
  pref_map_n <- redist::redist_map(pref_n,
                                   ndists = ndists_new,
                                   pop_tol= (sq_max_to_min - 1)/(1 + sq_max_to_min),
                                   total_pop = pop,
                                   adj = prefadj_n)

  # Run simulation
  sim_smc_pref_n <- redist::redist_smc(
    map = pref_map_n,
    nsims = nsims,
    pop_temper = 0.05
  )

  # Save pref object, pref_map object, adjacency list, and simulation data
  saveRDS(pref_n, paste("data-out/pref/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_",
                        as.character(nsims),
                        "_",
                        as.character(i),
                        ".Rds",
                        sep = ""))

  saveRDS(prefadj_n, paste("data-out/pref/",
                           as.character(pref_code),
                           "_",
                           as.character(pref_name),
                           "_",
                           as.character(nsims),
                           "_adj_",
                           as.character(i),
                           ".Rds",
                           sep = ""))

  saveRDS(pref_map_n, paste("data-out/maps/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_map_",
                            as.character(nsims),
                            "_",
                            as.character(i),
                            ".Rds",
                            sep = ""))

  saveRDS(sim_smc_pref_n, paste("data-out/plans/",
                                as.character(pref_code),
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

  assign(paste("pref", i, sep = "_"),
         pref_n,
         envir = .GlobalEnv)

  assign(paste("pref", "map", i, sep = "_"),
         pref_map_n,
         envir = .GlobalEnv)

  assign(paste("prefadj", i, sep = "_"),
         prefadj_n,
         envir = .GlobalEnv)

  assign(paste("sim", "smc", "pref", i, sep = "_"),
         sim_smc_pref_n,
         envir = .GlobalEnv)

}

run_simulations(pref_0, prefadj_0)
