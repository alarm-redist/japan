###############################################################################
# Simulations for `[TODO]`
# © ALARM Project, November 2021
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####
# Clean census data
census2020_current_municipalities <- census2020 %>%
  filter(type_of_municipality %in% c("a", "1", "9") == FALSE )
      #filter out irrelevant data
      #type_of_municipality: a: prefecture, 1. 政令指定都市及び東京都特別区,
      #9. 平成12年(2000年)現在の市区町村

pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020_current_municipalities, by = c('code')) %>%
  dplyr::select(code, pop, geometry)

# Add information about gun (郡)
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

# pref_1: Split largest municipality
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

  # Suggest connection between disconnected groups
  suggest <-  geomander::suggest_component_connection(shp = pref_n,
                                                      adj = prefadj_n)
  prefadj_n <- geomander::add_edge(prefadj_n,
                                   suggest$x,
                                   suggest$y,
                                   zero = TRUE)

  #return result
  return(prefadj_n)
}

# Make adjacency list
prefadj_0 <- add_adjacency(pref_0)
prefadj_1 <- add_adjacency(pref_1)

# TODO Repair adjacencies if necessary, and document these changes.


# Run simulations
run_simulations <- function(pref_n, prefadj_n){

  if("pre_gappei_code" %in% colnames(pref_n)){
    i <- 1
  }else{
    i <- 0
  }

  # Create redist.map object
  pref_map_n <- redist::redist_map(pref_n,
                                   ndists = ndists_new,
                                   pop_tol= 0.08,
                                   total_pop = pop,
                                   adj = prefadj_n)

  # Run simulation
  sim_smc_pref_n <- redist::redist_smc(
    map = pref_map_n,
    nsims = nsims,
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

run_simulations(pref_0, prefadj_0)
run_simulations(pref_1, prefadj_1)

####-------------- 2. Method for Urban Prefectures-------------------------####
# Clean 2015 Census shapefile
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::select(code, KIHON1, JINKO, geometry)

# Calculate population of Japanese nationals as of 2015 at the 小地域 level
pref <- calc_kokumin(pref, dem_pops)

# Filter out relevant data from 2020 Census (i.e. exclude municipalities pre-平成の大合併)
census2020_current_municipalities <- census2020 %>%
  filter(type_of_municipality %in% c("a", "1", "9") == FALSE )
  #a: prefecture #1. 政令指定都市及び東京都特別区 #9. 平成12年(2000年)現在の市区町村

# Estimate 2020 pop. at the 小地域 level
pref <- estimate_2020_pop(pref, census2020_current_municipalities) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

# Obtain codes of 郡 to merge
pref <- merge_gun(pref)
gun_codes <- unique(pref$gun_code[which(pref$gun_code >= (pref$code[1]%/%1000)*1000+300)])
gun_codes <- setdiff(gun_code, gun_exception)

# Separate non-郡 municipalities
pref_non_gun <- pref %>%
  dplyr::filter(gun_code %in% gun_codes == FALSE )

for(i in 1:length(gun_code)){

  gun <- pref %>%
    dplyr::filter(gun_code == gun_codes[i])

  gun$code <- gun_code[i]

  gun <- gun %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

  gun$subcode <- "0000"

  pref <- dplyr::bind_rows(pref_non_gun, gun)

}



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
prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)


pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.10,
                               total_pop = pop,
                               adj = prefadj)

constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 5)
constr = redist::add_constr_multisplits(constr, strength = 10)

sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  counties = pref$code,
  constraints = constr,
  pop_temper = 0.05)

