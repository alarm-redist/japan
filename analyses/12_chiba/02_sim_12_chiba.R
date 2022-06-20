###############################################################################
# Simulations for `00_pref`
# © ALARM Project, April 2021
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####
# Obtain codes of 郡 to merge
pref <- merge_gun(pref)
# Define pref_0

pref <-  sf::st_as_sf(
  dplyr::bind_rows(

    # Municipality that are not respected under the status quo
    pref %>%
      dplyr::filter(code %in% as.numeric(mun_not_freeze)),

    # Set aside gun that are not respected under the status quo,
    # and merge them by the municipality
    pref %>%
      dplyr::filter(gun_code %in% as.numeric(gun_exception)) %>%
      dplyr::filter(code != 12410) %>% # municipality that is split under status quo
      dplyr::group_by(code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       gun_code = gun_code[1]),

    # Merge the rest of municipality and gun
    pref %>%
      dplyr::filter(gun_code %in% c(gun_exception, mun_not_freeze) == FALSE) %>%
      dplyr::group_by(gun_code) %>%
      dplyr::summarize(geometry = sf::st_union(geometry),
                       pop = sum(pop),
                       code = gun_code[1])
  )
)

# Converet MULTIPOLYGON to several POLYGONs
new_rows <- data.frame(code = pref[1, ]$code,
                       sub_code = pref[1, ]$sub_code,
                       geometry = sf::st_cast(pref[1, ]$geometry, "POLYGON"),
                       pop = 0,
                       gun_code = pref[1, ]$gun_code
)

new_rows[1, ]$pop <- pref[1, ]$pop

pref_sep <- new_rows

# To calculate area, switch off the `geometry (s2)``
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
pref_add_edge <-
  matrix(c(
    # 安房郡島部
    which(pref$code == 12460)[1],
    which(pref$code == 12460)[2],
    which(pref$code == 12460)[1],
    which(pref$code == 12460)[3]
  ), ncol = 2, byrow = TRUE)
#Add edges
prefadj <- geomander::add_edge(prefadj,
                               pref_add_edge[,1],
                               pref_add_edge[,2])
# Define pref_map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
#constr = redist::redist_constr(pref_map)
#constr = redist::add_constr_splits(constr, strength = 4, admin = pref_map$gun_code)
#constr = redist::add_constr_multisplits(constr, strength = 5, admin = pref_map$gun_code)

# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  runs = 4L,
  counties = pref$code,
  #  constraints = constr,
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

