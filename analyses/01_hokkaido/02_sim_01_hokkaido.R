###############################################################################
# Simulations for `01_hokkaido`
# © ALARM Project, June 2022
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####
# Assign 郡 codes
# For Hokkaido, we will assign codes for 総合振興局 and 振興局 as `gun_code`
# following the rule by the redistricting committee.
pref <- pref %>%
  dplyr::mutate(gun_code = case_when(
    code %in% c(01209,
                01210,
                01215,
                01216,
                01218,
                01222,
                01225,
                01226,
                01227,
                01228,
                01423,
                01424,
                01425,
                01427,
                01428,
                01429,
                01430,
                01431,
                01432,
                01433,
                01434,
                01436,
                01437,
                01438) ~ "sorachi",
    code %in% c(01101,
                01102,
                01103,
                01104,
                01105,
                01106,
                01107,
                01108,
                01109,
                01110,
                01217,
                01224,
                01231,
                01234,
                01235,
                01303,
                01304) ~ "ishikari",
    code %in% c(01203,
                01391,
                01392,
                01393,
                01394,
                01395,
                01396,
                01397,
                01398,
                01399,
                01400,
                01401,
                01402,
                01403,
                01404,
                01405,
                01406,
                01407,
                01408,
                01409) ~ "shiribeshi",
    code %in% c(01205,
                01213,
                01230,
                01233,
                01571,
                01575,
                01578,
                01581,
                01584,
                01585,
                01586) ~ "iburi",
    code %in% c(01601,
                01602,
                01604,
                01607,
                01608,
                01609,
                01610) ~ "hidaka",
    code %in% c(01202,
                01236,
                01331,
                01332,
                01333,
                01334,
                01337,
                01343,
                01345,
                01346,
                01347) ~ "oshima",
    code %in% c(01361,
                01362,
                01363,
                01364,
                01367,
                01370,
                01371) ~ "hiyama",
    code %in% c(01204,
                01220,
                01221,
                01229,
                01452,
                01453,
                01454,
                01455,
                01456,
                01457,
                01458,
                01459,
                01460,
                01461,
                01462,
                01463,
                01464,
                01465,
                01468,
                01469,
                01470,
                01471,
                01472) ~ "kamikawa",
    code %in% c(01212,
                01481,
                01482,
                01483,
                01484,
                01485,
                01486,
                01487) ~ "rumoi",
    code %in% c(01214,
                01511,
                01512,
                01513,
                01514,
                01516,
                01517,
                01518,
                01519,
                01520) ~ "soya",
    code %in% c(01208,
                01211,
                01219,
                01543,
                01544,
                01545,
                01546,
                01547,
                01549,
                01550,
                01552,
                01555,
                01559,
                01560,
                01561,
                01562,
                01563,
                01564) ~ "okhotsk",
    code %in% c(01207,
                01631,
                01632,
                01633,
                01634,
                01635,
                01636,
                01637,
                01638,
                01639,
                01641,
                01642,
                01643,
                01644,
                01645,
                01646,
                01647,
                01648,
                01649) ~ "tokachi",
    code %in% c(01206,
                01661,
                01662,
                01663,
                01664,
                01665,
                01667,
                01668) ~ "kushiro",
    code %in% c(01223,
                01691,
                01692,
                01693,
                01694) ~ "nemuro"))

# Set aside 石狩振興局 (pop == 2,381,374) and assign proportional seats (6) to it.
# This is because in Hokkaido, the boundaries of 振興局 must be respected,
# and only 石狩振興局 needs to be split into multiple districts.
ishikari <- pref %>%
  dplyr::filter(gun_code %in% c("ishikari"))
non_ishikari <- pref %>%
  dplyr::filter(gun_code %in% c("ishikari") == FALSE)
ndists_ishikari <- round(
  sum(ishikari$pop) / (sum(pref$pop)/ndists_new))
ndists_non_ishikari <- ndists_new - ndists_ishikari

### Remaining Shinko-kyoku ###
# Merge by Shinko-kyoku
non_ishikari <- non_ishikari %>%
  dplyr::group_by(gun_code) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Make adjacency list
non_ishikariadj <- redist::redist.adjacency(non_ishikari)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
  # add ferries
  ferries <- add_ferries(non_ishikari)
  non_ishikariadj <- geomander::add_edge(non_ishikariadj,
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
# which(pref$code == xxxxx & pref$sub_code == "xxxx"),
# which(pref$code == xxxxx & pref$sub_code == "xxxx"))

# Define pref_map object
non_ishikari_map <- redist::redist_map(non_ishikari,
                                       ndists = ndists_non_ishikari,
                                       pop_tol= pop_tol_non_ishikari,
                                       total_pop = pop,
                                       adj = non_ishikariadj)

# Run simulation
set.seed(2020)
sim_smc_non_ishikari <- redist::redist_smc(
  map = non_ishikari_map,
  nsims = nsims,
  runs = 4L,
  pop_temper = 0.05)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_non_ishikari)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_non_ishikari))

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(non_ishikari, paste("data-out/pref/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_non_ishikari.Rds",
                    sep = ""))

saveRDS(non_ishikariadj, paste("data-out/pref/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "adj_non_ishikari.Rds",
                       sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(non_ishikari_map, paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "hr_2020_map_non_ishikari.rds",
                          sep = ""),
          compress = "xz")

saveRDS(sim_smc_non_ishikari, paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims * 4),
                            "_non_ishikari.Rds",
                            sep = ""))

### Ishikari Shinko-kyoku ###
# For Ishikari Shinko-kyoku, we will assign 6 seats.
# To make sure that the boudnaries of 郡 are respected,
# we are going to re-assign and merge by the gun_code
ishikari <- ishikari %>%
  dplyr::mutate(gun_code = if_else(
    # ishikari-gun
    code %in% c(01303, 01304),
    01300,
    # else: non_gun
    code))

# Choose 郡 to merge
gun_codes <- unique(ishikari$gun_code[which(ishikari$gun_code >= (ishikari$code[1]%/%1000)*1000+300)])

# Set aside non-郡 municipalities
ishikari_non_gun <- dplyr::filter(ishikari, gun_code %in% gun_codes == FALSE)
# Merge together 郡
ishikari_gun <- NULL
for(i in 1:length(gun_codes)){
  # filter out gun
  gun <- ishikari %>%
    dplyr::filter(gun_code == gun_codes[i])
  # merge together gun
  gun$code <- gun_codes[i]
  gun <- gun %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

  # merge back together
  gun$sub_code <- NA
  gun$gun_code <- gun_codes[i]
  ishikari_gun <- dplyr::bind_rows(ishikari_gun, gun)
}

# Bind together 郡 and non-郡 municipalities
ishikari <- dplyr::bind_rows(ishikari_non_gun, ishikari_gun)

# Convert multi-polygons into polygons
new_rows <- data.frame(code = ishikari[1, ]$code,
                       sub_code = ishikari[1, ]$sub_code,
                       geometry = sf::st_cast(ishikari[1, ]$geometry, "POLYGON"),
                       pop = 0,
                       gun_code = ishikari[1, ]$gun_code
)

new_rows[1, ]$pop <- ishikari[1, ]$pop

pref_sep <- new_rows

# to calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)
for (i in 2:nrow(ishikari))
{
  new_rows <- data.frame(code = ishikari[i, ]$code,
                         sub_code = ishikari[i, ]$sub_code,
                         geometry = sf::st_cast(ishikari[i, ]$geometry, "POLYGON"),
                         pop = 0,
                         gun_code = ishikari[i, ]$gun_code
  )

  # order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    dplyr::select(-area)

  # assign population to the largest area
  new_rows[1, ]$pop <- ishikari[i, ]$pop

  pref_sep <- rbind(pref_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)
ishikari <- sf::st_as_sf(pref_sep)

# Make adjacency list
ishikariadj <- redist::redist.adjacency(ishikari)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(ishikari)
    prefadj <- geomander::add_edge(ishikariadj,
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
                                # which(pref$code == xxxxx & pref$sub_code == "xxxx"),
                                # which(pref$code == xxxxx & pref$sub_code == "xxxx"))

# Define pref_map object
ishikari_map <- redist::redist_map(ishikari,
                               ndists = ndists_ishikari,
                               pop_tol= pop_tol_ishikari,
                               total_pop = pop,
                               adj = ishikariadj)

# Define constraints
constr = redist::redist_constr(ishikari_map)
constr = redist::add_constr_splits(constr, strength = 5, admin = ishikari_map$code)
constr = redist::add_constr_multisplits(constr, strength = 10, admin = ishikari_map$code)

# Run simulation
set.seed(2020)
sim_smc_ishikari <- redist::redist_smc(
  map = ishikari_map,
  nsims = nsims,
  runs = 4L,
  counties = ishikari$code,
#  constraints = constr,
  pop_temper = 0.05)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_ishikari)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_ishikari))


# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(ishikari, paste("data-out/pref/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_ishikari.Rds",
                    sep = ""))

saveRDS(ishikariadj, paste("data-out/pref/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "adj_ishiraki.Rds",
                       sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(ishikari_map, paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "hr_2020_map_ishikari.rds",
                          sep = ""),
          compress = "xz")

saveRDS(sim_smc_ishikari, paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims * 4),
                            "_ishikari.Rds",
                            sep = ""))

