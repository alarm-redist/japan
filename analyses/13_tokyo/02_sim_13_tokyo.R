###############################################################################
# Simulations for `13_tokyo`
# © ALARM Project, April 2021
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
# For Tokyo, run this process only for non-islands
pref_islands <- filter(pref, code >= 13360) # islands
pref <- filter(pref, code < 13360)  # non-islands

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

pref <- dplyr::bind_rows(pref_sep, pref_islands) %>%
  sf::st_as_sf()

#######Separate special wards area from Tama###############
special_wards <- filter(pref, code %in% c(13101:13123,
                13360, 13380, 13400, 13420)) #Islands are considered connected to Minato-ku

tama <-  filter(pref, code %in% c(13101:13123,
                      13360, 13380, 13400, 13420) == FALSE)

# Calculate seats to allocate
ndists_new_special_wards <- round(ndists_new * (sum(special_wards$pop)/sum(pref$pop)))
ndists_new_rural <- round(ndists_new * (sum(tama$pop)/sum(pref$pop)))

#######Special wards###############
# Make adjacency list
special_wardsadj <- redist::redist.adjacency(special_wards)

# Modify according to ferry adjacencies
if(check_ferries(pref_code) == TRUE){
  # add ferries
  ferries <- add_ferries(special_wards)
  special_wardsadj <- geomander::add_edge(special_wardsadj,
                                 ferries[, 1],
                                 ferries[, 2],
                                 zero = TRUE)
}

# Suggest connection between disconnected groups
suggest <-  geomander::suggest_component_connection(shp = special_wards,
                                                    adj = special_wardsadj)
special_wardsadj <- geomander::add_edge(special_wardsadj,
                                        suggest$x,
                                        suggest$y,
                                        zero = TRUE)

# Repair adjacencies
special_wards_add_edge <-
  matrix(c(
    #大田区東海-大田区平和島
    which(special_wards$code == 13111 & special_wards$sub_code == 580),
    which(special_wards$code == 13111 & special_wards$sub_code == 90),
    #大田区東海-大田区京浜島
    which(special_wards$code == 13111 & special_wards$sub_code == 580),
    which(special_wards$code == 13111 & special_wards$sub_code == 600),
    #大田区昭和島-大田区大森東
    which(special_wards$code == 13111 & special_wards$sub_code == 100),
    which(special_wards$code == 13111 & special_wards$sub_code == 20),
    #品川区八潮-品川区東八潮
    which(special_wards$code == 13109 & special_wards$sub_code == 250),
    which(special_wards$code == 13109 & special_wards$sub_code == 270),
    #品川区八潮-品川区東大井
    which(special_wards$code == 13109 & special_wards$sub_code == 250),
    which(special_wards$code == 13109 & special_wards$sub_code == 160),
    #品川区八潮-品川区東品川
    which(special_wards$code == 13109 & special_wards$sub_code == 250),
    which(special_wards$code == 13109 & special_wards$sub_code == 180),
    #港区海岸-港区台場
    which(special_wards$code == 13103 & special_wards$sub_code == 20),
    which(special_wards$code == 13103 & special_wards$sub_code == 300)[2],
    #中央区晴海-江東区豊洲
    which(special_wards$code == 13102 & special_wards$sub_code == 380),
    which(special_wards$code == 13108 & special_wards$sub_code == 210),
    #中央区晴海-中央区月島
    which(special_wards$code == 13102 & special_wards$sub_code == 380),
    which(special_wards$code == 13102 & special_wards$sub_code == 350),
    #中央区月島-中央区明石町
    which(special_wards$code == 13102 & special_wards$sub_code == 350),
    which(special_wards$code == 13102 & special_wards$sub_code == 70),
    #中央区明石町-中央区佃
    which(special_wards$code == 13102 & special_wards$sub_code == 70),
    which(special_wards$code == 13102 & special_wards$sub_code == 340),
    #中央区築地-中央区勝どき
    which(special_wards$code == 13102 & special_wards$sub_code == 80),
    which(special_wards$code == 13102 & special_wards$sub_code == 360),
    #中央区佃-中央区新川
    which(special_wards$code == 13102 & special_wards$sub_code == 340),
    which(special_wards$code == 13102 & special_wards$sub_code == 110),
    #中央区佃-江東区越中島
    which(special_wards$code == 13102 & special_wards$sub_code == 340),
    which(special_wards$code == 13108 & special_wards$sub_code == 180),
    #中央区新川-江東区永代
    which(special_wards$code == 13102 & special_wards$sub_code == 110),
    which(special_wards$code == 13108 & special_wards$sub_code == 100),
    #中央区新川-中央区湊
    which(special_wards$code == 13102 & special_wards$sub_code == 110),
    which(special_wards$code == 13102 & special_wards$sub_code == 60),
    #江東区豊洲-江東区有明
    which(special_wards$code == 13108 & special_wards$sub_code == 210),
    which(special_wards$code == 13108 & special_wards$sub_code == 230),
    #江東区塩浜-江東区木場
    which(special_wards$code == 13108 & special_wards$sub_code == 190),
    which(special_wards$code == 13108 & special_wards$sub_code == 350),
    #江東区塩浜-江東区東陽
    which(special_wards$code == 13108 & special_wards$sub_code == 190),
    which(special_wards$code == 13108 & special_wards$sub_code == 360),
    #江東区塩浜-江東区新砂
    which(special_wards$code == 13108 & special_wards$sub_code == 190),
    which(special_wards$code == 13108 & special_wards$sub_code == 420)
  ), ncol = 2, byrow = TRUE)


special_wardsadj <- geomander::add_edge(special_wardsadj,
                                        which(special_wards$code == 13111 & special_wards$sub_code == 580),
                                        which(pref$code == xxxxx & pref$sub_code == "xxxx"))

# Define pref_map object
special_wards_map <- redist::redist_map(special_wards,
                                       ndists = ndists_new_special_wards,
                                       pop_tol= 0.08,
                                       total_pop = pop,
                                       adj = special_wardsadj)

# Define constraints
constr_special_wards = redist::redist_constr(special_wards_map)
constr_special_wards = redist::add_constr_splits(constr_special_wards,
                                                 strength = 5,
                                                 admin = special_wards_map$code)
constr_special_wards = redist::add_constr_multisplits(constr_special_wards,
                                                      strength = 10,
                                                      admin = special_wards_map$code)

# Run simulation
sim_smc_special_wards <- redist::redist_smc(
  map = special_wards_map,
  nsims = 1000,
  counties = special_wards_map$code,
  constraints = constr_special_wards,
  pop_temper = 0.05)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_special_wards))


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
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

########### Tama area ###############

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
                               pop_tol= 0.10,
                               total_pop = pop,
                               adj = prefadj)

# Define constraints
constr = redist::redist_constr(pref_map)
constr = redist::add_constr_splits(constr, strength = 5)
constr = redist::add_constr_multisplits(constr, strength = 10)

# Run simulation
sim_smc_pref <- redist::redist_smc(
  map = pref_map,
  nsims = nsims,
  counties = pref$code,
  constraints = constr,
  pop_temper = 0.05)

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
                            as.character(nsims),
                            ".Rds",
                            sep = ""))
