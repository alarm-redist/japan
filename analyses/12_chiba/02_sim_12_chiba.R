###############################################################################
# Simulations for `00_pref`
# © ALARM Project, April 2021
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####
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

########################
### Method for Chiba ###
########################
# For Chiba with many discontinuity of gun,
# we will summarize geometry into multiple contiguous unit of gun.
# set aside guns from pref_non_gun

pref_gun_discontinuity <- pref_non_gun %>%
  dplyr::filter(gun_code %in% c(gun_exception) == TRUE) %>%
  dplyr::mutate(gun_block = case_when(
    code %in% c(12322) == TRUE ~ 12322,
    code %in% c(12329) == TRUE ~ 12329,
    code %in% c(12342) == TRUE ~ 12342,
    code %in% c(12347) == TRUE ~ 12347,
    code %in% c(12349) == TRUE ~ 12349,
    code %in% c(12403, 12409) == TRUE ~ 12403,
    code %in% c(12410) == TRUE ~ 12410,
    code %in% c(12441) == TRUE ~ 12441,
    code %in% c(12443) == TRUE ~ 12443 )) %>%
  dplyr::group_by(gun_block) %>%
  dplyr::summarise(code = dplyr::first(gun_code),
                   gun_code = dplyr::first(gun_code),
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

pref_non_gun <- pref_non_gun %>%
  dplyr::filter(gun_code %in% c(gun_exception) == FALSE)

# Bind together 郡 and non-郡 municipalities
pref <- dplyr::bind_rows(pref_non_gun, pref_gun, pref_gun_discontinuity)

# Converet MULTIPOLYGON to several POLYGONs
new_rows <- data.frame(code = pref[1, ]$code,
                       sub_code = pref[1, ]$sub_code,
                       geometry = sf::st_cast(pref[1, ]$geometry, "POLYGON"),
                       pop = 0,
                       gun_code = pref[1, ]$gun_code
)

new_rows[1, ]$pop <- pref[1, ]$pop

pref_sep <- new_rows

for (i in 2:nrow(pref))
{
  new_rows <- data.frame(code = pref[i, ]$code,
                         sub_code = pref[i, ]$sub_code,
                         geometry = sf::st_cast(pref[i, ]$geometry, "POLYGON"),
                         pop = 0,
                         gun_code = pref[i, ]$gun_code
  )
  new_rows[1, ]$pop <- pref[i, ]$pop

  pref_sep <- rbind(pref_sep, new_rows)
}

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
    # 長生郡
    which(pref$code == 12420)[1],
    which(pref$code == 12420)[2],
    which(pref$code == 12420)[1],
    which(pref$code == 12420)[3],

    # 安房郡
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
                               pop_tol= 0.20,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 7, admin = pref_map$code)
constr = redist::add_constr_multisplits(constr, strength = 10, admin = pref_map$code)

set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  runs = 2L,
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
                            as.character(nsims * 2),
                            ".Rds",
                            sep = ""))

