###############################################################################
# Simulations for `[TODO]`
# © ALARM Project, November 2021
###############################################################################

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

# Find codes that are splittable
valid_codes <- unique(pref$code)
for (i in length(valid_codes):1)
{
  if (length(find_old_codes(valid_codes[i], pop_by_old_boundary)) < 1)
  {
    valid_codes <- valid_codes[-i]
  }
}

# Order splittable municipalities by population
split_codes <- pref[order(-pref$pop), ][which(pref$code %in% valid_codes), ]$code

# Run simulations for n splits
for (i in 0:n_split)
{
  
  # Split relevant municipalities
  if (i > 0)
  {
    new_n <- split_codes[0:i]
    old_n <- find_old_codes(new_n, pop_by_old_boundary)
    pref_n <- reflect_old_boundaries(pref, old_boundary, pop_by_old_boundary, old_n, new_n)
    pref_n <- estimate_old_boundary_pop(old_n, new_n, pref_n, census2020)
  }
  
  # Create sf object and adjacency list
  pref_n <- sf::st_as_sf(
    pref %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = code[1])
  )
  
  # TODO If necessary, manually merge together municipalities in simulation
  
  prefadj_n <- redist::redist.adjacency(pref_n)
  
  # TODO Repair adjacencies if necessary, and document these changes.
  
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
  
  constr_n = redist::redist_constr(pref_map_n)
  constr_n = redist::add_constr_splits(constr_n, strength = 100)
  constr_n = redist::add_constr_multisplits(constr_n, strength = 200)
  
  sim_smc_pref_n <- redist::redist_smc(
    map = pref_map_n,
    nsims = nsims,
    counties = pref_n$code,
    constraints = constr_n,
    pop_temper = 0.05
  )
  
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
