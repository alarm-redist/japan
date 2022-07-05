###############################################################################
# Simulations for `07_fukushima`
# © ALARM Project, June 2022
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
                                   pop_tol= pop_tol,
                                   total_pop = pop,
                                   adj = prefadj_n)

  # Run simulation
  set.seed(2020)
  sim_smc_pref_n <- redist::redist_smc(
    map = pref_map_n,
    nsims = nsims,
    runs = 4L,
    pop_temper = 0.05
  )

  # Save pref object, pref_map object, adjacency list, and simulation data
  saveRDS(pref_n, paste("data-out/pref/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_",
                        as.character(i),
                        ".Rds",
                        sep = ""))

  saveRDS(prefadj_n, paste("data-out/pref/",
                           as.character(pref_code),
                           "_",
                           as.character(pref_name),
                           "_adj_",
                           as.character(i),
                           ".Rds",
                           sep = ""))

  # pref_map object: to be uploaded to Dataverse
  write_rds(pref_map_n, paste("data-out/maps/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_hr_2020_map_",
                              as.character(i),
                              ".rds",
                              sep = ""),
            compress = "xz")

  saveRDS(sim_smc_pref_n, paste("data-out/plans/",
                                as.character(pref_code),
                                "_",
                                as.character(pref_name),
                                "_",
                                as.character(sim_type),
                                "_",
                                as.character(nsims * 4),
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

# Run simulations
# For Fukushima, we only run `0_split` models.
# Whereas the 1_split model by default splits the largest municipality based on boundaries before 平成の大合併,
# there are no such boundaries for the largest municipality in Fukushima, いわき市.
# The last time any municipality was merged into いわき市 was 1966.
# Thus, we only run a 0_split model
run_simulations(pref_0, prefadj_0)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_pref_0)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.

hist(plans_diversity(sim_smc_pref_0))
