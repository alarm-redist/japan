###############################################################################
# Simulations for `12_chiba`
# © ALARM Project, April 2023
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####
# Re-order and add 郡 codes
pref <- pref_mun %>%
  arrange(code, sub_code) %>%
  merge_gun()

# Make adjacency list
prefadj <- redist::redist.adjacency(pref)

# Modify according to ferry adjacencies
# ferries <- add_ferries(pref)
# prefadj <- geomander::add_edge(prefadj,
#                                ferries[, 1],
#                                ferries[, 2],
#                                zero = TRUE)

# Suggest connection between disconnected groups
# suggest <-  geomander::suggest_component_connection(shp = pref,
#                                                     adj = prefadj)
# prefadj <- geomander::add_edge(prefadj,
#                                suggest$x,
#                                suggest$y,
#                                zero = TRUE)

# TODO Repair adjacencies if necessary, and document these changes.
# In Chiba, we will add edges between the 船橋市丸山,
# a discontiguous part of 船橋市, to the main parts of 船橋市.
# Although that area is not adjacent in the map,
# the newly enacted plan treats 船橋市丸山 as contiguous
# and the newly enacted plan cannot be replicated without adding these edges.
# Hence, we fix the adjacency list in the following way.
pref_add_edge <-
  matrix(c(
    # 船橋市丸山-藤原
    which(pref$code == 12204 &
            pref$sub_code == 420),
    which(pref$code == 12204 &
            pref$sub_code == 390),

    # 船橋市丸山-馬込西
    which(pref$code == 12204 &
            pref$sub_code == 420),
    which(pref$code == 12204 &
            pref$sub_code == 970),

    # 船橋市丸山-馬込町
    which(pref$code == 12204 &
            pref$sub_code == 420),
    which(pref$code == 12204 &
            pref$sub_code == 410)

  ), ncol = 2, byrow = TRUE)

# Add edges
prefadj <- geomander::add_edge(prefadj,
                               pref_add_edge[,1],
                               pref_add_edge[,2])

# Create redist.map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj,
                               planarize = 4612)

# Merge gun
pref_map_merged <- pref_map %>%
  # Convert codes to character
  mutate(code = as.character(code),
         sub_code = as.character(sub_code),
         gun_code = as.character(gun_code)) %>%
  # Only freeze the "gun" that are kept together in the same district under the old plan
  # Make a code to determine which gun to freeze
  # If a gun is one of the gun in `gun_exception`, don't freeze it
  # For Chiba, we freeze all the municipalities except the two that
  # the newly enacted plan splits (船橋市 and 市川市).
  # This is because the default method (allowing to split 5 municipalities
  # that are split in the old plan) does not converge, and partial SMC method
  # is not appropriate because the both new and old plan does not respect
  # the 地域振興事務所.
  mutate(freeze_code = if_else(gun_code %in% c(gun_exception),
                               code,
                               gun_code)) %>%
  # Group by and merge by `gun_code`
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Drop column `freeze_code`
  select(-freeze_code)

# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_pref = redist::redist_constr(pref_map_merged)
# constr_pref = redist::add_constr_splits(constr_pref, strength = 1, admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref, strength = 1.5, admin = pref_map_merged$code)

# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 8L,
  # Vector of municipality codes
  counties = pref_map_merged$code,
  constraints = constr_pref,
  pop_temper = 0.02)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_pref)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_pref))

# Pull back plans to unmerged units
sim_smc_pref_pullback <- pullback(sim_smc_pref)

# Add reference plan
# Write csv file
pref %>%
  as.data.frame() %>%
  select("code",
         "gun_code",
         "pop",
         "mun_name",
         "sub_name") %>%
  write_excel_csv(here(paste("temp/",
                             as.character(pref_code),
                             "_",
                             as.character(pref_name),
                             "_lh_2022.csv",
                             sep = "")))

# Read back the CSV to environment
dist_lh_2022 <- read_csv(here(paste("data-raw/lh_2022/",
                                    as.character(pref_code),
                                    "_",
                                    as.character(pref_name),
                                    "_lh_2022.csv",
                                    sep = "")))

# Add reference plan
pref_map$lh_2022 <- dist_lh_2022$lh_2022
sim_smc_pref_ref <- add_reference(plans = sim_smc_pref_pullback,
                                  ref_plan = as.numeric(dist_lh_2022$lh_2022),
                                  name = "lh_2022")

# Add `total_pop`
for(i in 1:ndists_new){
  sim_smc_pref_ref$total_pop[which(sim_smc_pref_ref$draw == "lh_2022" &
                                     sim_smc_pref_ref$district == i)] <-
    # Population in District i
    sum(dist_lh_2022$pop[which(dist_lh_2022$lh_2022 == i)])
}

# Add precinct population
attr(sim_smc_pref_ref, "prec_pop") <- pref_map$pop

# Save pref object, pref_map object, adjacency list, and simulation data
saveRDS(pref, here(paste("data-out/shapefile/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         ".Rds",
                         sep = "")))

saveRDS(prefadj, here(paste("data-out/adj/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_adj.Rds",
                            sep = "")))

# pref_map object: to be uploaded to Dataverse
write_rds(pref_map, here(paste("data-out/map/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_lh_2022_map.rds",
                               sep = "")),
          compress = "xz")

saveRDS(sim_smc_pref_ref, here(paste("data-out/smc-out/",
                                     as.character(pref_code),
                                     "_",
                                     as.character(pref_name),
                                     "_",
                                     as.character(sim_type),
                                     "_",
                                     as.character(nsims * 8),
                                     ".Rds",
                                     sep = "")))
