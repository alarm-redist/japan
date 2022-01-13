###############################################################################
# Simulations for Tokyo
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

#######Separat Tama from 23ku###############
urban <- pref %>%
  filter(code %in% c(13101:13123,
                     #Islands are considered connected to Minato-ku
                     13360, 13380, 13400, 13420))

rural <-  pref %>%
  filter(code %in% c(13101:13123,
                     13360, 13380, 13400, 13420) == FALSE)

# Calculate seats to allocate
ndists_new_urban <- round(ndists_new * (sum(urban$pop)/sum(pref$pop)))
ndists_new_rural <- round(ndists_new * (sum(rural$pop)/sum(pref$pop)))

#######Urban area###############
# Make adjacency list
urbanadj <- redist::redist.adjacency(urban)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
  # add ferries
  ferries <- add_ferries(urban)
  urbanadj <- geomander::add_edge(urbanadj,
                                 ferries[, 1],
                                 ferries[, 2],
                                 zero = TRUE)
}

# Repair adjacencies
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0340"),
                                which(urban$code == 13102 & urban$subcode == "0110"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0340"),
                                which(urban$code == 13102 & urban$subcode == "0070"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0340"),
                                which(urban$code == 13102 & urban$subcode == "0350"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0350"),
                                which(urban$code == 13102 & urban$subcode == "0360"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0360"),
                                which(urban$code == 13102 & urban$subcode == "0080"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0360"),
                                which(urban$code == 13102 & urban$subcode == "0370"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0380"),
                                which(urban$code == 13102 & urban$subcode == "0350"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0380"),
                                which(urban$code == 13102 & urban$subcode == "0360"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13102 & urban$subcode == "0380"),
                                which(urban$code == 13108 & urban$subcode == "0210"))
urbanadj <- geomander::add_edge(urbanadj,
                                which(urban$code == 13103 & urban$subcode == "0300"),
                                which(urban$code == 13103 & urban$subcode == "0020"))
urbanadj <- geomander::add_edge(urbanadj,
                               which(urban$code == 13120 & urban$subcode == "0420"),
                               which(urban$code == 13120 & urban$subcode == "0430"))
urbanadj <- geomander::add_edge(urbanadj,
                               which(urban$code == 13109 & urban$subcode == "0250"),
                               which(urban$code == 13109 & urban$subcode == "0180"))
urbanadj <- geomander::add_edge(urbanadj,
                               which(urban$code == 13109 & urban$subcode == "0250"),
                               which(urban$code == 13109 & urban$subcode == "0160"))
urbanadj <- geomander::add_edge(urbanadj,
                               which(urban$code == 13109 & urban$subcode == "0250"),
                               which(urban$code == 13109 & urban$subcode == "0040"))
urbanadj <- geomander::add_edge(urbanadj,
                               which(urban$code == 13109 & urban$subcode == "0270"),
                               which(urban$code == 13109 & urban$subcode == "0250"))
urbanadj <- geomander::add_edge(urbanadj,
                               which(urban$code == 13111 & urban$subcode == "0580"),
                               which(urban$code == 13111 & urban$subcode == "0090"))
urbanadj <- geomander::add_edge(urbanadj,
                               which(urban$code == 13111 & urban$subcode == "0580"),
                               which(urban$code == 13111 & urban$subcode == "0600"))


# Define pref_map object
urban_map <- redist::redist_map(urban,
                                ndists = ndists_new_urban,
                                pop_tol= 0.08,
                                total_pop = pop,
                                adj = urbanadj)

# Define constraints
constr_urban = redist::redist_constr(urban_map)
constr_urban = redist::add_constr_splits(constr_urban, strength = 5)
constr_urban = redist::add_constr_multisplits(constr_urban, strength = 10)

# Run simulation
sim_smc_urban <- redist::redist_smc(
  map = urban_map,
  nsims = nsims,
  counties = urban$code,
  constraints = constr_urban,
  pop_temper = 0.05)

# Save map and simulation data
saveRDS(urban_map, paste("data-out/maps/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_urban_map_",
                        as.character(nsims),
                        ".Rds",
                        sep = ""))

saveRDS(sim_smc_urban, paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_urban_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

#######Rural area###############
# Make adjacency list
ruraladj <- redist::redist.adjacency(rural)

# Define pref_map object
rural_map <- redist::redist_map(rural,
                                ndists = ndists_new_rural,
                                pop_tol= 0.06,
                                total_pop = pop,
                                adj = ruraladj)

# Define constraints
constr_rural = redist::redist_constr(rural_map)
constr_rural = redist::add_constr_splits(constr_rural, strength = 2)
constr_rural = redist::add_constr_multisplits(constr_rural, strength = 5)

# Run simulation
sim_smc_rural <- redist::redist_smc(
  map = rural_map,
  nsims = nsims,
  counties = rural$code,
  constraints = constr_rural,
  pop_temper = 0.05)

# Save map and simulation data
saveRDS(rural_map, paste("data-out/maps/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         "_rural_map_",
                         as.character(nsims),
                         ".Rds",
                         sep = ""))

saveRDS(sim_smc_rural, paste("data-out/plans/",
                             as.character(pref_code),
                             "_",
                             as.character(pref_name),
                             "_rural_",
                             as.character(sim_type),
                             "_",
                             as.character(nsims),
                             ".Rds",
                             sep = ""))

