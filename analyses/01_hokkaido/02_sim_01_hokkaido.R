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
                01371,
                01371) ~ "hiyama",
    #旭川市 ／士別市 ／名寄市 ／富良野市 ／鷹栖町 ／東神楽町 ／当麻町 ／比布町 ／愛別町 ／上川町 ／東川町 ／美瑛町 ／上富良野町 ／中富良野町 ／南富良野町 ／占冠村 ／和寒町 ／剣淵町 ／下川町 ／美深町 ／音威子府村 ／中川町 ／幌加内町
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
    #北見市 ／網走市 ／紋別市 ／美幌町 ／津別町 ／斜里町 ／清里町 ／小清水町 ／訓子府町 ／置戸町 ／佐呂間町 ／遠軽町 ／湧別町 ／滝上町 ／興部町 ／西興部村 ／雄武町 ／大空町
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
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 5, admin = pref_map$code)
constr = redist::add_constr_multisplits(constr, strength = 10, admin = pref_map$code)

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

