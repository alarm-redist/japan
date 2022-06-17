###############################################################################
# Simulations for `27_osaka`
# © ALARM Project, June 2021
###############################################################################

# Obtain codes of 郡 to merge
pref <- merge_gun(pref)
gun_codes <- unique(pref$gun_code[which(pref$gun_code >= (pref$code[1]%/%1000)*1000+300)])
# Filter out exceptions
gun_codes <- setdiff(gun_codes, gun_exception)

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
  gun$sub_code <- 0
  gun$gun_code <- gun_codes[i]
  pref_gun <- dplyr::bind_rows(pref_gun, gun)
}

# Bind together 郡 and non-郡 municipalities
pref <- dplyr::bind_rows(pref_non_gun, pref_gun)

# Converet MULTIPOLYGON to several POLYGONs
new_rows <- data.frame(code = pref[1, ]$code,
                       sub_code = pref[1, ]$sub_code,
                       geometry = sf::st_cast(pref[1, ]$geometry, "POLYGON"),
                       pop = 0,
                       gun_code = pref[1, ]$gun_code
)

new_rows[1, ]$pop <- pref[1, ]$pop

pref_sep <- new_rows

# to calculate area size, switch off the `geometry (s2)`
sf_use_s2(FALSE)
for (i in 2:nrow(pref))
{
  new_rows <- data.frame(code = pref[i, ]$code,
                         sub_code = pref[i, ]$sub_code,
                         geometry = sf::st_cast(pref[i, ]$geometry, "POLYGON"),
                         pop = 0,
                         gun_code = pref[i, ]$gun_code
  )
  # order by size of the area
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    dplyr::select(-area)
  # assign population to the largest area
  new_rows[1, ]$pop <- pref[i, ]$pop

  pref_sep <- rbind(pref_sep, new_rows)
}

# switch on `geometry (s2)`
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

# The function judged incorrectly that 此花区北港白津(sub_code: 160)
# is connected to 此花区梅町(sub_code: 140).
# Thus, we remove this adjacency
suggest <- suggest %>%
  filter(x != which(pref$code == 27104 & pref$sub_code == 140) &
         y != which(pref$code == 27104 & pref$sub_code == 160))

prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)

# TODO Repair adjacencies if necessary, and document these changes.
# Connect 此花区北港白津(sub_code: 160) with 常吉(sub_code: 120)
prefadj <- geomander::add_edge(prefadj,
                which(pref$code == 27104 & pref$sub_code == "160"),
                which(pref$code == 27104 & pref$sub_code == "120"))
# Connect 此花区北港白津(sub_code: 160) with 北港(sub_code: 130)
prefadj <- geomander::add_edge(prefadj,
            which(pref$code == 27104 & pref$sub_code == "160"),
            which(pref$code == 27104 & pref$sub_code == "130"))

# Define pref_map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 16, admin = pref_map$code)
constr = redist::add_constr_multisplits(constr, strength = 1, admin = pref_map$code)

# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = 100,
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
