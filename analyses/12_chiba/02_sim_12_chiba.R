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
    # Merge 香取市 and 香取郡 together
    code %in% c(12236, 12342, 12347, 12349) == TRUE ~ 12342,
    code %in% c(12403) == TRUE ~ 12403,
    code %in% c(12409) == TRUE ~ 12409,
    code %in% c(12410) == TRUE ~ 12410,
    # Merge いすみ市 and 夷隅郡 together
    code %in% c(12238, 12441, 12443) == TRUE ~ 12441)) %>%
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

#######Method for Chiba#######
# We will conduct simulations for 船橋市, 松戸市, and 市川市 aside from other municipalities
# and group them as `west`to avoid the municipalities to be split in to more than three districts (multi-splits).
# This is because they are adjacent while their population exceeds the target population
# (the total population divided by the total number of districts).
# Moreover, since the number of municipality split under the status quo is 5
# whereas 3 municipalities exceeds target population,
# this process is essential to produce the valid plans.
# To do so, because 船橋市 has 飛地 (discontinued area) in 鎌ヶ谷市 and
# 浦安市 has no adjacent municipalities but 市川市,
# we need to put those two into the `west` simulation to produce the valid plans without discontinuity.

west <- filter(pref, code %in% c(12204,
                                 12207,
                                 12203,
                                 12227,
                                 12224))

east <-  filter(pref, code %in% c(12204,
                                  12207,
                                  12203,
                                  12227,
                                  12224) == FALSE)

# Calculate seats to allocate
ndists_new_west <- round(ndists_new * (sum(west$pop)/sum(pref$pop)))
ndists_new_east <- round(ndists_new * (sum(east$pop)/sum(pref$pop)))

####### West ########
# Make adjacency list
westadj <- redist::redist.adjacency(west)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
  # add ferries
  ferries <- add_ferries(west)
  westadj <- geomander::add_edge(westadj,
                                          ferries[, 1],
                                          ferries[, 2],
                                          zero = TRUE)
}

# Suggest connection between disconnected groups
suggest_west <-  geomander::suggest_component_connection(shp = west,
                                                         adj = westadj)
westadj <- geomander::add_edge(westadj,
                               suggest_west$x,
                               suggest_west$y,
                               zero = TRUE)


# Define pref_map object
west_map <- redist::redist_map(west,
                                        ndists = ndists_new_west,
                                        pop_tol= 0.35,
                                        total_pop = pop,
                                        adj = westadj)

# Define constraints
constr_west = redist::redist_constr(west_map)
constr_west = redist::add_constr_splits(constr_west,
                                       strength = 4,
                                       admin = west_map$code)
constr_west = redist::add_constr_multisplits(constr_west,
                                             strength = 6,
                                             admin = west_map$code)

# Run simulation
set.seed(2020)
sim_smc_west <- redist::redist_smc(
  map = west_map,
  nsims = nsims,
  runs = 4L,
  counties = west_map$code,
  constraints = constr_west,
  pop_temper = 0.05)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_west)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_west))

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(west, paste("data-out/pref/",
                             as.character(pref_code),
                             "_",
                             as.character(pref_name),
                             "_west.Rds",
                             sep = ""))

saveRDS(westadj, paste("data-out/pref/",
                                as.character(pref_code),
                                "_",
                                as.character(pref_name),
                                "_adj_west.Rds",
                                sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(west_map, paste("data-out/maps/",
                                   as.character(pref_code),
                                   "_",
                                   as.character(pref_name),
                                   "_hr_2020_map_west.rds",
                                   sep = ""),
          compress = "xz")

saveRDS(sim_smc_west, paste("data-out/plans/",
                                     as.character(pref_code),
                                     "_",
                                     as.character(pref_name),
                                     "_",
                                     as.character(sim_type),
                                     "_",
                                     as.character(nsims * 4),
                                     "_west.Rds",
                                     sep = ""))

####### East ########
# Make adjacency list
eastadj <- redist::redist.adjacency(east)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
  # add ferries
  ferries <- add_ferries(east)
  eastadj <- geomander::add_edge(eastadj,
                                 ferries[, 1],
                                 ferries[, 2],
                                 zero = TRUE)
}

# Suggest connection between disconnected groups
suggest_east <-  geomander::suggest_component_connection(shp = east,
                                                         adj = eastadj)
eastadj <- geomander::add_edge(eastadj,
                               suggest_east$x,
                               suggest_east$y,
                               zero = TRUE)

# TODO Repair adjacencies if necessary, and document these changes.
east_add_edge <-
  matrix(c(
    # 安房郡島部
    which(east$code == 12460)[1],
    which(east$code == 12460)[2]
  ), ncol = 2, byrow = TRUE)

#Add edges
eastadj <- geomander::add_edge(eastadj,
                               east_add_edge[,1],
                               east_add_edge[,2])

# Define pref_map object
east_map <- redist::redist_map(east,
                               ndists = ndists_new_east,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = eastadj)

# Define constraints
constr_east = redist::redist_constr(east_map)
constr_east = redist::add_constr_splits(constr_east,
                                        strength = 4,
                                        admin = east_map$code)
constr_east = redist::add_constr_multisplits(constr_east,
                                             strength = 6,
                                             admin = east_map$code)

# Run simulation
set.seed(2020)
sim_smc_east <- redist::redist_smc(
  map = east_map,
  nsims = nsims,
  runs = 4L,
  counties = east_map$code,
  constraints = constr_east,
  pop_temper = 0.05)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_east)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_east))

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(east, paste("data-out/pref/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_east.Rds",
                    sep = ""))

saveRDS(eastadj, paste("data-out/pref/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "_adj_east.Rds",
                       sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(east_map, paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_hr_2020_map_east.rds",
                          sep = ""),
          compress = "xz")

saveRDS(sim_smc_east, paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims * 4),
                            "_east.Rds",
                            sep = ""))


