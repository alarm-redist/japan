###############################################################################
# Simulations for `34_Hiroshima`
# © ALARM Project, April 2021
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

# Define pref_1: Split largest municipality
# Select the municipalities with the largest population (excluding the 区 of 政令指定都市)
split_code <- (pref %>%
                 dplyr::filter(code >=
                                 (pref$code[1]%/%1000)*1000+200))[order(-(pref %>%
                                                                            dplyr::filter(code >=
                                                                                            (pref$code[1]%/%1000)*1000+200))$pop), ]$code[1]
new_1 <- as.character(split_code)
# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition (total population - foreign population)
pref_1 <- reflect_old_boundaries(pref_0, old_mun, census_mun_old_2020, new_1)

# Add adjacency
add_adjacency <- function(pref_n){

  prefadj_n <- redist::redist.adjacency(pref_n)

  # Modify according to ferry adjacencies
  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries_n <- add_ferries(pref_n)
    prefadj_n <- geomander::add_edge(prefadj_n,
                                     ferries_n[, 1],
                                     ferries_n[, 2],
                                     zero = TRUE)
  }

  #return result
  return(prefadj_n)
}

# Make adjacency list
prefadj_0 <- add_adjacency(pref_0)
prefadj_1 <- add_adjacency(pref_1)

# Suggest connection between disconnected groups
suggest <-  geomander::suggest_component_connection(shp = pref_1,
                                                    adj = prefadj_1)
# As a result of splitting 福山市, 福山市内海町 appears to be disconnected.
# To fix this, an adjacency edge is added between 内海町(34481) and 沼隈町(34482)
prefadj_1 <- geomander::add_edge(prefadj_1,
                                 suggest$x,
                                 suggest$y,
                                 zero = TRUE)


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
run_simulations(pref_1, prefadj_1)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.

hist(plans_diversity(sim_smc_pref_0))
hist(plans_diversity(sim_smc_pref_1))
