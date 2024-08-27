###############################################################################
# Simulations for `04_miyagi`
# © ALARM Project, March 2023
###############################################################################

####-------------- 1. Method for Rural Prefectures-------------------------####
# Split the municipalities that are split under the status quo
split_code <- as.character(split_code)
# Note that the size of Japanese population in the object census_mun_old_2020 is defined differently
# reflect_old_boundaries() automatically estimates the size of the Japanese population
# based on the official definition (total population - foreign population)
pref_mun_old <- reflect_old_boundaries(pref_mun, old_mun, census_mun_old_2020, split_code)

# Replace NA values in `old_mun_name`
pref_mun_old$old_mun_name <- replace_na(pref_mun_old$old_mun_name, "-")

# Re-order and add 郡 codes
pref <- pref_mun_old %>%
  arrange(code, pre_gappei_code) %>%
  merge_gun()

# Make adjacency list
prefadj <- redist::redist.adjacency(pref)

# Modify according to ferry adjacencies
# There are no ferry routes within Miyagi so no ferry-related edges are added.
# ferries <- add_ferries(pref)
# prefadj <- geomander::add_edge(prefadj,
#                                ferries[, 1],
#                                ferries[, 2],
#                                zero = TRUE)

# Suggest connection between disconnected groups
# No additional edges are added.
# suggest <-  geomander::suggest_component_connection(shp = pref,
#                                                     adj = prefadj)
# prefadj <- geomander::add_edge(prefadj,
#                                suggest$x,
#                                suggest$y,
#                                zero = TRUE)

# TODO Repair adjacencies if necessary, and document these changes.
# prefadj <- geomander::add_edge(prefadj,
#                                which(pref$code == ),
#                                which(pref$code == ))

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
  mutate(pre_gappei_code = as.character(pre_gappei_code),
         code = as.character(code),
         gun_code = as.character(gun_code)) %>%
  # Only freeze the "gun" that are kept together in the same district under the old plan
  # Make a code to determine which gun to freeze
  # If a gun is one of the gun in `gun_exception`, don't freeze it
  mutate(freeze_code = if_else(gun_code %in% gun_exception,
                               pre_gappei_code,
                               gun_code)) %>%
  # Group by and merge by `gun_code`
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Drop column `freeze_code`
  select(-freeze_code)

# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref,
                                        strength = 1,
                                        admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref,
                                             strength = 1, # set strength of constraint
                                             admin = pref_map_merged$code)

# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims,
  runs = 4L,
  # Vector of municipality codes
  counties = pref_map_merged$code, # Comment out if you are not adding any constraints
  constraints = constr_pref, # Comment out if you are not adding any constraints
  pop_temper = 0.05
)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_pref)

# For Miyagi, the plan diversity could not be increased further.

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
  select("pre_gappei_code",
         "old_mun_name",
         "code",
         "gun_code",
         "pop",
         "mun_name") %>%
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
sim_smc_pref_ref <- add_reference(plans = sim_smc_pref_pullback,
                                  ref_plan = as.numeric(dist_lh_2022$lh_2022),
                                  name = "lh_2022")

# Add `total_pop`
pref_map$lh_2022 <- dist_lh_2022$lh_2022
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
                                     as.character(nsims * 4),
                                     ".Rds",
                                     sep = "")))

