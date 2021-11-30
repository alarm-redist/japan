# Clean census data
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)
  
# Add information about gun (郡)
pref <- merge_gun(pref)

# TODO Define the koiki-renkei areas (広域連携)
a_koiki <- c()
b_koiki <- c()
c_koiki <- c()
koiki <- c(a_koiki, b_koiki, c_koiki)

# Run simulations for n splits
for (i in 0:n_split)
{
  
  # Create sf object and adjacency list
  pref_n <- sf::st_as_sf(
    pref %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = code[1])
  )
  
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
  
  # Create redist.map object
  pref_map_n <- redist::redist_map(pref_n,
                                   ndists = ndists_new,
                                   pop_tol= 0.10,
                                   total_pop = pop,
                                   adj = prefadj_n)
  
  # Run simulation
  sim_smc_pref_n <- redist::redist_smc(pref_map_n,
                                       nsims = nsims,
                                       pop_temper = 0.05)
  
  # Save map and simulation data
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
  
}
