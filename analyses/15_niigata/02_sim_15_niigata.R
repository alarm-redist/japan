###############################################################################
# Simulations for `15_Niigata`
# © ALARM Project, June 2022
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####
# Add information about 郡
pref <- merge_gun(pref)

# Process only for Niigata. To avoid enclaves causing discontiguity, 
# we merge together pairs of municipalities A and B if municipality A has an 
# enclave within B.

# 新潟市北区 has an enclave within 阿賀野氏, so we merge the two municipalities.
pref_enclave_1 <- pref %>%
  dplyr::filter(code %in% c(15101, 15223)) %>%
  dplyr::summarise(code = code,
                   pop = pop,
                   geometry = sf::st_union(geometry),
                   gun_code = gun_code,
                   merged_code = 15901)

# 刈羽村 consists of two discontiguous parts within 柏崎市, so we merge the two municipalities
pref_enclave_2 <- pref %>%
  dplyr::filter(code %in% c(15205, 15504)) %>%
  dplyr::summarise(code = code,
                   pop = pop,
                   geometry = sf::st_union(geometry),
                   gun_code = gun_code,
                   merged_code = 15902)

# 見附市 has an enclave within 長岡市, so we merge the two municipalities.
pref_enclave_3 <- pref %>%
  dplyr::filter(code %in% c(15202, 15211)) %>%
  dplyr::summarise(code = code,
                   pop = pop,
                   geometry = sf::st_union(geometry),
                   gun_code = gun_code,
                   merged_code = 15903)

# Join together to create a new prefecture object
pref_without_enclaves <- pref %>%
  dplyr::filter(!code %in% c(15101, 15223,
                             15205, 15504,
                             15202, 15211)) %>%
  dplyr::mutate(merged_code = code)

pref_merged_0 <- rbind(pref_without_enclaves,
                       pref_enclave_1,
                       pref_enclave_2,
                       pref_enclave_3)


# Define pref_0
pref_0 <-  sf::st_as_sf(
  dplyr::bind_rows(
    
    # Set aside gun that are not respected under the status quo
    pref_merged_0 %>% filter(gun_code %in% as.numeric(gun_exception)),
    
    # Merge gun
    pref_merged_0 %>%
      dplyr::filter(gun_code %in% gun_exception == FALSE) %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = code[1],
                       merged_code = merged_code[1])
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
pref_1$merged_code[which(pref_1$code == 15202)] <- pref_1$pre_gappei_code[which(pref_1$code == 15202)]


pref_1_without_enclaves <- pref_1 %>%
  dplyr::filter(!code %in% c(15101, 15223,
                             15205, 15504)) %>%
  dplyr::mutate(merged_code = pre_gappei_code)

pref_merged_1 <- rbind(pref_1_without_enclaves,
                       pref_enclave_1 %>% dplyr::mutate(pre_gappei_code = code),
                       pref_enclave_2 %>% dplyr::mutate(pre_gappei_code = code))

# Define pref_0
pref_1 <-  sf::st_as_sf(
  dplyr::bind_rows(
    
    # Set aside gun that are not respected under the status quo
    pref_merged_1 %>% filter(gun_code %in% as.numeric(gun_exception)),
    
    # Merge gun
    pref_merged_1 %>%
      dplyr::filter(gun_code %in% gun_exception == FALSE) %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = code[1],
                       merged_code = merged_code[1],
                       pre_gappei_code = pre_gappei_code[1])
  )
)

pref_smc_0 <- pref_0 %>% 
  dplyr::group_by(merged_code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry),
                   code = first(code),
                   pop = sum(pop),
                   gun_code = first(gun_code))

pref_smc_1 <- pref_1 %>% 
  dplyr::group_by(merged_code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry),
                   code = first(code),
                   pop = sum(pop),
                   gun_code = first(gun_code),
                   pre_gappei_code = pre_gappei_code[1])

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

# Make SMC prefs that merge the merged_code column

# Make adjacency list
prefadj_0 <- add_adjacency(pref_smc_0)
prefadj_1 <- add_adjacency(pref_smc_1)

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
                                as.character(format(nsims * 4, scientific=FALSE)),
                                "_",
                                as.character(i),
                                ".Rds",
                                sep = ""))
  
  assign(paste("pref", "smc", i, sep = "_"),
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

run_simulations(pref_smc_0, prefadj_0)
run_simulations(pref_smc_1, prefadj_1)

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
