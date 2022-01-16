###############################################################################
# Simulations for `Aichi`
# © ALARM Project, November 2021
###############################################################################

# Clean 2015 Census shapefile
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::select(code, KIHON1, JINKO, geometry)

# Calculate population of Japanese nationals as of 2015 at the 小地域 level
pref <- calc_kokumin(pref, dem_pops)

# Filter out relevant data from 2020 Census
census2020_current_municipalities <- census2020 %>%
  filter(type_of_municipality %in% c("a", "1", "9") == FALSE )

# Estimate 2020 pop. at the 小地域 level
pref <- estimate_2020_pop(pref, census2020_current_municipalities) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

# Obtain codes of 郡 to merge
pref <- merge_gun(pref)
gun_codes <- unique(pref$gun_code[which(pref$gun_code >= (pref$code[1]%/%1000)*1000+300)])
# Filter out exceptions
gun_codes <- setdiff(gun_codes, gun_exception)

# Set aside non-郡 municipalities
pref_non_gun <- pref %>%
  dplyr::filter(gun_code %in% gun_codes == FALSE)

# Merge together 郡
pref_gun <- NULL
for(i in 1:length(gun_codes)){
  # filter out gun
  gun <- pref %>%
    dplyr::filter(gun_code == gun_codes[i])

  # merge together gun
  gun$code <- gun_codes[i]
  gun <- gun %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

  # merge back together
  gun$subcode <- "0000"
  gun$gun_code <- gun_codes[i]
  pref_gun <- dplyr::bind_rows(pref_gun, gun)
}

# Bind together 郡 and non-郡 municipalities
pref <- dplyr::bind_rows(pref_non_gun, pref_gun)

# Make adjacency list
prefadj <- redist::redist.adjacency(pref)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
  # add ferries
  ferries <- add_ferries(pref)
  prefadj <- geomander::add_edge(prefadj,
                                 ferries[, 1],
                                 ferries[, 2],
                                 zero = TRUE)
}

# Suggest connection between disconnected groups
suggest <-  geomander::suggest_component_connection(shp = pref,
                                                    adj = prefadj)
#connects 名古屋市港区金城ふ頭 (23111, 0460) with 名古屋市港区空見町(23111, 1080)
#connects 常滑市セントレア町 (23216, 0960) with 常滑市りんくう町(23216, 0950)
prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)

# Define pref_map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.15,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 6)
constr = redist::add_constr_multisplits(constr, strength = 8)

# Run simulation
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  counties = pref$code,
  constraints = constr,
  pop_temper = 0.05)

# Save map and simulation data
saveRDS(pref_map, paste("data-out/maps/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_map_",
                        as.character(nsims),
                        ".Rds",
                        sep = ""))

saveRDS(sim_smc_pref, paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

check_valid <- function(pref_n, plans_matrix, bridges) {

  pref_sep <- data.frame(unit = 1, geometry = sf::st_cast(pref_n[1, ]$geometry, "POLYGON"))

  for (i in 2:nrow(pref_n))
  {
    pref_sep <- rbind(pref_sep, data.frame(unit = i, geometry = sf::st_cast(pref_n[i, ]$geometry, "POLYGON")))
  }

  pref_sep <- sf::st_as_sf(pref_sep)
  pref_sep_adj <- redist::redist.adjacency(pref_sep)

  mainland <- pref_sep[which(unlist(lapply(pref_sep_adj, length)) > 0), ]
  mainland_adj <- redist::redist.adjacency(mainland)
  mainland$component <- geomander::check_contiguity(mainland_adj)$component

  for (j in 1:length(bridges))
  {
    start <- which(pref_n$pre_gappei_code == bridges[[j]][1])
    end <- which(pref_n$pre_gappei_code == bridges[[j]][2])

    for (x in which(mainland$unit == start))
    {
      for (y in which(mainland$unit == end))
      {
        mainland_adj <- geomander::add_edge(mainland_adj,
                                            x,
                                            y,
                                            zero = TRUE)
      }
    }
  }

  checks <- vector(length = ncol(plans_matrix))

  for (k in 1:ncol(plans_matrix))
  {
    mainland_plan <- plans_matrix[mainland$unit, k]
    checks[k] <- max(geomander::check_contiguity(mainland_adj, mainland_plan + (ndists_new-1)*mainland$component)$component) == 1
  }

  return(checks)

}
