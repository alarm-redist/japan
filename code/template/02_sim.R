###############################################################################
# Simulations for `[TODO]`
# © ALARM Project, November 2021
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####
# Clean census data
census2020_current_municipalities <- census2020 %>%
  #filter out irrelevant data
  filter(type_of_municipality %in% c("a", "1", "9") == FALSE )

pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020_current_municipalities, by = c('code')) %>%
  dplyr::select(code, pop, geometry)

# remove lake if needed
ifelse(is.null(lakes_removed),
       pref <- pref,
       pref <- remove_lake(pref, lakes_removed))

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
pref_1 <- reflect_old_boundaries(pref_0, old_boundary, census2020, new_1)

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
<<<<<<< Updated upstream
                                   pop_tol= 0.10,
=======
                                   pop_tol= 0.15,
>>>>>>> Stashed changes
                                   total_pop = pop,
                                   adj = prefadj_n)

  # Run simulation
  sim_smc_pref_n <- redist::redist_smc(
    map = pref_map_n,
    nsims = nsims,
    pop_temper = 0.05
  )

  # Save pref object, pref_map object, and simulation data
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

  assign(paste("sim", "smc", "pref", i, sep = "_"),
         sim_smc_pref_n,
         envir = .GlobalEnv)

}

run_simulations(pref_0, prefadj_0)
run_simulations(pref_1, prefadj_1)

####-------------- 2. Method for Urban Prefectures-------------------------####
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

# Optional: Suggest connection between disconnected groups
"suggest <-  geomander::suggest_component_connection(shp = pref,
                                                    adj = prefadj)
prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)"

# TODO Repair adjacencies if necessary, and document these changes.
# prefadj <- geomander::add_edge(prefadj,
                                # which(pref$code == xxxxx & pref$subcode == "xxxx"),
                                # which(pref$code == xxxxx & pref$subcode == "xxxx"))

# Define pref_map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.10,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 5)
constr = redist::add_constr_multisplits(constr, strength = 10)

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
  mainland$component <- geomander::check_contiguity(adj = mainland_adj)$component

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
