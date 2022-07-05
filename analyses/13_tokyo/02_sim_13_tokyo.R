###############################################################################
# Simulations for `13_tokyo`
# © ALARM Project, July 2022
###############################################################################

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
pref <- dplyr::bind_rows(pref_sep, pref_islands) %>%
  sf::st_as_sf()

#######Separate special wards area from Tama###############
special_wards <- pref %>%
  filter(code %in% c(13101:13123,
                     13360, 13380, 13400, 13420)) %>% #Islands are considered connected to Minato-ku

  # Filter out Koto-ku, whose population is larger than the target population
  filter(code %in% pref$code[which(pref$pop > sum(pref$pop)/ndists_new)] == FALSE)

special_wards_with_koto <- filter(pref, code %in% c(13101:13123, 13360, 13380, 13400, 13420))

tama <-  filter(pref, code %in% c(13101:13123, 13360, 13380, 13400, 13420) == FALSE)

# Calculate seats to allocate
ndists_new_special_wards <-
  round(ndists_new * (sum(special_wards_with_koto$pop)/sum(pref$pop))) - 1 # Exclude Koto-ku
ndists_new_tama <- round(ndists_new * (sum(tama$pop)/sum(pref$pop)))

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
suggest_special_wards <-  geomander::suggest_component_connection(shp = special_wards,
                                                    adj = special_wardsadj)
special_wardsadj <- geomander::add_edge(special_wardsadj,
                                        suggest_special_wards$x,
                                        suggest_special_wards$y,
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
    #品川区八潮-品川区東品川5丁目
    which(special_wards$code == 13109 & special_wards$sub_code == 250),
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[2],
    #品川区東品川1・3・4丁目 - 品川区東品川2丁目
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[1],
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[3],
    #品川区東品川2丁目 - 品川区東品川5丁目
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[3],
    which(special_wards$code == 13109 & special_wards$sub_code == 180)[2],
    #港区海岸-港区台場
    which(special_wards$code == 13103 & special_wards$sub_code == 20),
    which(special_wards$code == 13103 & special_wards$sub_code == 300)[1],
    #中央区佃・月島・勝鬨-中央区の大部分の地域
    which(special_wards$code == 13102)[1],
    which(special_wards$code == 13102)[2],
    #中央区浜離宮庭園の各地域
    which(special_wards$code == 13102)[1],
    which(special_wards$code == 13102)[4]
  ), ncol = 2, byrow = TRUE)

#Add edges
special_wardsadj <- geomander::add_edge(special_wardsadj,
                                        special_wards_add_edge[,1],
                                        special_wards_add_edge[,2])

# Define pref_map object
special_wards_map <- redist::redist_map(special_wards,
                                        ndists = ndists_new_special_wards,
                                        pop_tol= pop_tol_special_wards,
                                        total_pop = pop,
                                        adj = special_wardsadj)

# Define constraints
constr_special_wards = redist::redist_constr(special_wards_map)
constr_special_wards = redist::add_constr_splits(constr_special_wards,
                                                 strength = 2,
                                                 admin = special_wards_map$code)
constr_special_wards = redist::add_constr_multisplits(constr_special_wards,
                                                      strength = 6,
                                                      admin = special_wards_map$code)

# Run simulation
set.seed(2020)
sim_smc_special_wards <- redist::redist_smc(
  map = special_wards_map,
  nsims = nsims_special_wards,
  runs = 4L,
  counties = special_wards_map$code,
  constraints = constr_special_wards,
  pop_temper = 0.06)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_special_wards)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_special_wards))

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(special_wards, paste("data-out/pref/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_special_wards.Rds",
                        sep = ""))

saveRDS(special_wardsadj, paste("data-out/pref/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_adj_special_wards.Rds",
                          sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(special_wards_map, paste("data-out/maps/",
                             as.character(pref_code),
                             "_",
                             as.character(pref_name),
                             "_hr_2020_map_special_wards.rds",
                             sep = ""),
                      compress = "xz")

saveRDS(sim_smc_special_wards, paste("data-out/plans/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims_special_wards * 4),
                               "_special_wards.Rds",
                               sep = ""))

########### Tama area ###############
# Make adjacency list
tamaadj <- redist::redist.adjacency(tama)

# No-ferry-related adjacencies to add in Tama

# Suggest connection between disconnected groups
suggest_tama <-  geomander::suggest_component_connection(shp = tama,
                                                    adj = tamaadj)
tamaadj <- geomander::add_edge(tamaadj,
                               suggest_tama$x,
                               suggest_tama$y,
                               zero = TRUE)

# Define pref_map object
tama_map <- redist::redist_map(tama,
                               ndists = ndists_new_tama,
                               pop_tol= pop_tol_tama,
                               total_pop = pop,
                               adj = tamaadj)

# Define constraints
constr_tama = redist::redist_constr(tama_map)
constr_tama = redist::add_constr_splits(constr_tama,
                                        strength = 1,
                                        admin = tama_map$code)
constr_tama = redist::add_constr_multisplits(constr_tama,
                                             strength = 2,
                                             admin = tama_map$code)

# Run simulation
set.seed(2020)
sim_smc_tama <- redist::redist_smc(
  map = tama_map,
  nsims = nsims_tama,
  runs = 4L,
  counties = tama_map$code,
  constraints = constr_tama,
  pop_temper = 0.05)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_tama)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_tama))

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(tama, paste("data-out/pref/",
                      as.character(pref_code),
                      "_",
                      as.character(pref_name),
                      "_tama.Rds",
                      sep = ""))

saveRDS(tamaadj, paste("data-out/pref/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_adj_tama.Rds",
                        sep = ""))

# pref_map object: to be uploaded to Dataverse
write_rds(tama_map, paste("data-out/maps/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_hr_2020_map_tama.rds",
                            sep = ""),
          compress = "xz")

saveRDS(sim_smc_tama, paste("data-out/plans/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims_tama * 4),
                              "_tama.Rds",
                              sep = ""))
