###############################################################################
# Simulations for `33_Okayama`
# © ALARM Project, April 2021
###############################################################################

# Add information about 郡
pref <- merge_gun(pref)

# Define pref_0
# This is an object without Kurashiki-shi, which is set aside because
# its population is larger than the target population
pref_0 <-  sf::st_as_sf(
  dplyr::bind_rows(

    # Set aside gun that are not respected under the status quo
    pref %>% filter(gun_code %in% as.numeric(gun_exception)),

    # Merge gun
    pref %>%
      # Filter out Kurashiki-shi, whose population is larger than the target population
      dplyr::filter(code %in% pref$code[which(pref$pop > sum(pref$pop)/ndists_new)] == FALSE) %>%

      dplyr::filter(gun_code %in% gun_exception == FALSE) %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = code[1])
  )
)

# Also make object with Kurashiki-shi
pref_0_with_kurashiki <- sf::st_as_sf(
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
pref_1 <- reflect_old_boundaries(pref_0_with_kurashiki, old_mun, census_mun_old_2020, new_1)

# Make adjacency list
prefadj_0 <- redist::redist.adjacency(pref_0)
prefadj_1 <- redist::redist.adjacency(pref_1)

# Run simulations
run_simulations <- function(pref_n, prefadj_n){

  # 0 split or 1 split
  if("pre_gappei_code" %in% colnames(pref_n)){
    i <- 1
  }else{
    i <- 0
  }

  if("pre_gappei_code" %in% colnames(pref_n)){
    # Create redist.map object
    pref_map_n <- redist::redist_map(pref_n,
                                     ndists = ndists_new,
                                     pop_tol= 0.30,
                                     total_pop = pop,
                                     adj = prefadj_n)

  }else{
    # Create redist.map object
    pref_map_n <- redist::redist_map(pref_n,
                                     ndists = ndists_new - 1, # set aside Kurashiki
                                     pop_tol= 0.30,
                                     total_pop = pop,
                                     adj = prefadj_n)
  }

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
run_simulations(pref_1, prefadj_1)
