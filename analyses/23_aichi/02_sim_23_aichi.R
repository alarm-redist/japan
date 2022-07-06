###############################################################################
# Simulations for `23_aichi`
# © ALARM Project, June 2021
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

# TODO Repair adjacencies if necessary, and document these changes.
# Repair adjacencies 1
pref_add_edge_1 <-
  matrix(c(
    # 西尾市東幡豆町(本土)-西尾市島嶼部
    which(pref$code == 23213)[1],
    which(pref$code == 23213)[5],
    which(pref$code == 23213)[1],
    which(pref$code == 23213)[4],
    which(pref$code == 23213)[1],
    which(pref$code == 23213)[3],

    # 知多郡(本土)-知多郡島嶼部
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[5],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[6],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[7],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[8],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[9],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[10],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[11],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[12],
    which(pref$code == 23440)[1],
    which(pref$code == 23440)[13],

    # 蒲郡市三河大島-小島
    which(pref$code == 23214)[2],
    which(pref$code == 23214)[3],

    # 田原市(本土)-姫島
    which(pref$code == 23231)[1],
    which(pref$code == 23231)[2],

    # 常滑市(本土)-中部国際空港
    which(pref$code == 23216)[1],
    which(pref$code == 23216)[2]

  ), ncol = 2, byrow = TRUE)

# Add edges 1
prefadj <- geomander::add_edge(prefadj,
                               pref_add_edge_1[,1],
                               pref_add_edge_1[,2])

# Repair adjacencies 2
pref_add_edge_2 <-
  matrix(c(
    # 名古屋市熱田区
    which(pref$code == 23109)[1],
    which(pref$code == 23109)[2],

    # 名古屋市中川区
    which(pref$code == 23110)[1],
    which(pref$code == 23110)[2],

    # 名古屋市港区
    which(pref$code == 23111)[1],
    which(pref$code == 23111)[2],
    which(pref$code == 23111)[1],
    which(pref$code == 23111)[3],
    which(pref$code == 23111)[1],
    which(pref$code == 23111)[5],
    which(pref$code == 23111)[3],
    which(pref$code == 23111)[7],
    which(pref$code == 23111)[7],
    which(pref$code == 23111)[6],
    which(pref$code == 23111)[6],
    which(pref$code == 23111)[4],

    # 名古屋市中川区-中区
    which(pref$code == 23110)[2],
    which(pref$code == 23106),

    # 名古屋市中村区-中区
    which(pref$code == 23105),
    which(pref$code == 23106),

    # 名古屋市熱田区-南区
    which(pref$code == 23109)[2],
    which(pref$code == 23112),

    # 名古屋市熱田区-瑞穂区
    which(pref$code == 23109)[2],
    which(pref$code == 23108),

    # 名古屋市熱田区-昭和区
    which(pref$code == 23109)[2],
    which(pref$code == 23107),

    # 名古屋市港区-東海市
    which(pref$code == 23111)[4],
    which(pref$code == 23222),

    # 名古屋市南区-東海市
    which(pref$code == 23112),
    which(pref$code == 23222)

  ), ncol = 2, byrow = TRUE)

# Add edges 2
prefadj <- geomander::add_edge(prefadj,
                               pref_add_edge_2[,1],
                               pref_add_edge_2[,2])

# Define pref_map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 4, admin = pref_map$code)
constr = redist::add_constr_multisplits(constr, strength = 3, admin = pref_map$code)

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
