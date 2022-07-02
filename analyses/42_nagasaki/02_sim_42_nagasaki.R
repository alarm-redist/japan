###############################################################################
# Simulations for `42_nagasaki`
# © ALARM Project, June 2021
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####
# Add information about 郡
pref <- merge_gun(pref)

# Define pref_0
pref_0 <-  sf::st_as_sf(
  dplyr::bind_rows(

    # Merge gun
    pref %>%
      dplyr::filter(gun_code %in% gun_exception == FALSE) %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = code[1]),

    # Set aside gun that are not respected under the status quo
    pref %>% filter(gun_code %in% as.numeric(gun_exception))
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

# Repair adjacency list
# Under the enacted plan, 対馬市(42209) and 壱岐市(42210) are considered as
# being contiguous to 大村市(42205) because they are connected via air routes.
# Thus, we consider 対馬市(42209) and 壱岐市(42210) to be adjacent to 大村市(42205).
prefadj_0 <- geomander::add_edge(prefadj_0,
                                 which(pref_0$code == 42209), #対馬市
                                 which(pref_0$code == 42205)) #大村市
prefadj_0 <- geomander::add_edge(prefadj_0,
                                 which(pref_0$code == 42210), #壱岐市
                                 which(pref_0$code == 42205)) #大村市
prefadj_1 <- geomander::add_edge(prefadj_1,
                                 which(pref_1$code == 42209), #対馬市
                                 which(pref_1$code == 42205)) #大村市
prefadj_1 <- geomander::add_edge(prefadj_1,
                                 which(pref_1$code == 42210), #壱岐市
                                 which(pref_1$code == 42205)) #大村市

# In addition, under the enacted plan, 西海市(42212) and 佐世保市(42202) are considered
# to be contiguous, as those municipalities are connected via a bridge.
# Thus, we add an adjacency between 西海市(42212) and 佐世保市(42202) and
# treat them as being adjacent to each other.
prefadj_0 <- geomander::add_edge(prefadj_0,
                                 which(pref_0$code == 42212), #西海市
                                 which(pref_0$code == 42202)) #佐世保市
prefadj_1 <- geomander::add_edge(prefadj_1,
                                 which(pref_1$code == 42212), #対馬市
                                 which(pref_1$code == 42202)) #佐世保市

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
    runs = 2L,
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
                                as.character(nsims * 2),
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

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_pref_0)
summary(sim_smc_pref_1)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.

hist(plans_diversity(sim_smc_pref_0))
hist(plans_diversity(sim_smc_pref_1))
