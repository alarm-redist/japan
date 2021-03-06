###############################################################################
# Simulations for `11_saitama`
# © ALARM Project, May 2022
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####
# Assign 郡 codes
pref <- merge_gun(pref)

# Choose 郡 to merge
gun_codes <- unique(pref$gun_code[which(pref$gun_code >= (pref$code[1]%/%1000)*1000+300)])
gun_codes <- setdiff(gun_codes, gun_exception) # Filter out exceptions

# Set aside non-郡 municipalities
pref_non_gun <- dplyr::filter(pref, gun_code %in% gun_codes == FALSE)

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
  gun$sub_code <- NA
  gun$gun_code <- gun_codes[i]
  pref_gun <- dplyr::bind_rows(pref_gun, gun)
}

# Bind together 郡 and non-郡 municipalities
pref <- dplyr::bind_rows(pref_non_gun, pref_gun)

### Method for Saitama ###
# 秩父郡 is made up of discontiguous parts. In order to make sure that 秩父郡 is not split
# and to ensure that we do not create discontiguous districts, we merge it with
# its adjacent municipality, 秩父市.
chichibu_code <- c(11207, 11360)

pref <- dplyr::bind_rows(
  pref %>%
    dplyr::filter(code %in% chichibu_code == FALSE),

  pref %>%
    dplyr::filter(code %in% chichibu_code) %>%
    dplyr::summarise(pop = sum(pop),
                     geometry = sf::st_union(geometry),
                     code = dplyr::last(code),
                     gun_code = dplyr::last(gun_code),
                     sub_code = dplyr::last(sub_code))
  )%>%
  sf::st_as_sf()

# Convert multi-polygons into polygons
new_rows <- data.frame(code = pref[1, ]$code,
                       sub_code = pref[1, ]$sub_code,
                       geometry = sf::st_cast(pref[1, ]$geometry, "POLYGON"),
                       pop = 0,
                       gun_code = pref[1, ]$gun_code
)
new_rows[1, ]$pop <- pref[1, ]$pop

pref_sep <- new_rows

# to calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)
for (i in 2:nrow(pref))
{
  new_rows <- data.frame(code = pref[i, ]$code,
                         sub_code = pref[i, ]$sub_code,
                         geometry = sf::st_cast(pref[i, ]$geometry, "POLYGON"),
                         pop = 0,
                         gun_code = pref[i, ]$gun_code
  )
  # order by size

  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    dplyr::select(-area)

  # assign population to the largest area
  new_rows[1, ]$pop <- pref[i, ]$pop

  pref_sep <- rbind(pref_sep, new_rows)
}

# switch on the `geometry (s2)``
sf_use_s2(TRUE)
pref <- sf::st_as_sf(pref_sep)

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
suggest <-  geomander::suggest_component_connection(shp = pref,
                                                    adj = prefadj)
prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)

# TODO Repair adjacencies if necessary, and document these changes.
# prefadj <- geomander::add_edge(prefadj,
                                # which(pref$code == xxxxx & pref$sub_code == "xxxx"),
                                # which(pref$code == xxxxx & pref$sub_code == "xxxx"))

# Define pref_map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
#constr = redist::add_constr_splits(constr, strength = 3, admin = pref_map$gun_code)
constr = redist::add_constr_multisplits(constr, strength = 4, admin = pref_map$gun_code)
constr = redist::add_constr_total_splits(constr, strength = 4, admin = pref_map$gun_code)

# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  runs = 4L,
  counties = pref$code,
  constraints = constr,
  pop_temper = 0.05)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_pref)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_pref))

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(pref, paste("data-out/pref/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    ".Rds",
                    sep = ""))

saveRDS(prefadj, paste("data-out/pref/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "_adj.Rds",
                       sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(pref_map, paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_hr_2020_map.rds",
                          sep = ""),
          compress = "xz")

saveRDS(sim_smc_pref, paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims * 4),
                            ".Rds",
                            sep = ""))

