###############################################################################
# Simulations for `11_saitama`
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
# pref_add_edge <-
#   matrix(c(), ncol = 2, byrow = TRUE)
#
# # Add edges
# prefadj <- geomander::add_edge(prefadj,
#                                 pref_add_edge[,1],
#                                 pref_add_edge[,2])

# Create redist.map object
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= pop_tol,
                               total_pop = pop,
                               adj = prefadj,
                               planarize = 4612)

# Define unique id (necessary for partial SMC)
pref_map <- pref_map %>%
  mutate(uid = row_number())

# Merge gun
pref_map_merged <- pref_map %>%
  # Convert codes to character
  mutate(code = as.character(code),
         sub_code = as.character(sub_code),
         gun_code = as.character(gun_code)) %>%
  # Only freeze the "gun" that are kept together in the same district under the old plan
  # Make a code to determine which gun to freeze
  # If a gun is one of the gun in `gun_exception`, don't freeze it
  mutate(freeze_code = if_else(gun_code %in% c(gun_exception),
                               code,
                               # For Saitama, we will merge 秩父市 and 秩父郡.
                               # 秩父郡 is made up of discontiguous parts.
                               # In order to make sure that 秩父郡 is not split
                               # and to ensure that we do not create discontiguous districts,
                               # we merge it with its adjacent municipality (秩父市).
                               if_else(gun_code %in% as.character(c(110322, 11360)),
                                       as.character(11360), # Assign code for 秩父郡
                                       gun_code))) %>%
  # Group by and merge by `gun_code`
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Drop column `freeze_code`
  select(-freeze_code)

# Because the SMC algorithm does not converge when running it for the entire prefecture, we separate Saitma into two regions using administrative boundaries (地域振興センター)
# This administrative unit is used by the Saitama Prefectural Government (https://www.pref.saitama.lg.jp/a0106/chiikisinkoucenter.html and https://www.pref.saitama.lg.jp/a0107/jyuuminnokatahe/kanko-event.html).
# We group 地域振興センター in the following way and assigned seats proportionally to each region.
# South: 南西部地域振興センター, 西部地域振興センター, and 川越比企地域振興.
# North: Others (Since we don’t allow 秩父郡 to be split into different districts, we assigned 秩父郡東秩父村 within 秩父地域振興センター)
# Both the old enacted plan and the newly enacted plan respect these two regions, except 秩父郡東秩父村.
south <- c(11227, 11228, 11229, 11230, 11235, 11245, 11324,
           11208, 11209, 11215, 11225, 11242,
           11201, 11212, 11239, 11241, 11326, 11327,
           "11341~11342~11343~11346~11347~11348~11349")
north <- setdiff(unique(pref_map_merged$code), south)
# Calculate target population & number of districts
target_pop <- sum(pref_map$pop)/ndists_new
# Calculate number of districts per region
ndists_new_south <- round(
  sum(pref_map_merged$pop[which(pref_map_merged$code %in% south)]) / target_pop
)
# The remaining
ndists_new_north <- round(
  sum(pref_map_merged$pop[which(pref_map_merged$code %in% north)]) / target_pop
)

# Saitama-south Map
south_map <- pref_map_merged %>%
  filter(code %in% south) %>%
  `attr<-`("ndists", ndists_new_south) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

####################
### Partial SMC  ###
####################
# Simulate South
# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
# constr_south = redist::redist_constr(south_map)
# constr_south = redist::add_constr_splits(constr_south,
#                                          strength = 1,
#                                          admin = south_map$code)
# constr_south = redist::add_constr_multisplits(constr_south,
#                                               strength = 1,
#                                               admin = south_map$code)

# Run simulation
set.seed(2020)
sim_smc_south <- redist::redist_smc(
  map = south_map,
  nsims = nsims_init,
  runs = 4L,
  # Vector of municipality codes
  # counties = south_map$code,
  # constraints = constr_south,
  pop_temper = 0.05)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_south)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_south))

######
# Filter out valid plans for Saitama-south
# Get plans matrix
south_smc_plans <- redist::get_plans_matrix(sim_smc_south)

# Calculate max:min ratio
wgt_smc_south <- simulation_weight_disparity_table(sim_smc_south)

# Count number of municipality splits
num_mun_split_south <- count_splits(south_smc_plans, south_map$code)
mun_split_south <- redist::redist.splits(south_smc_plans, south_map$code) %>%
  matrix(ncol = ndists_new_south, byrow = TRUE)
mun_split_south <- mun_split_south[,1]

# Count number of gun splits
gun_split_south <- redist::redist.splits(south_smc_plans, south_map$gun_code) %>%
  matrix(ncol = ndists_new_south, byrow = TRUE)
gun_split <- gun_split_south[,1]

# Compile results
results_south <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_south)))
results_south$max_to_min <- wgt_smc_south$max_to_min
results_south$gun_split <- gun_split
results_south$num_mun_split <- num_mun_split_south
results_south$mun_split <- mun_split_south
results_south$multi <-  num_mun_split_south - mun_split_south
results_south$draw <- wgt_smc_south$draw

# Filter out plans with 0 multi-splits
no_multi_south <- results_south %>%
  filter(multi == 0) %>%
  pull(draw)

# Results for plans with 0 multi-splits
results_south_no_multi <- results_south %>%
  dplyr::filter(draw %in% no_multi_south)

# Plans with 0 multi-splits
sim_smc_south_no_multi <- sim_smc_south %>%
  dplyr::filter(draw %in% no_multi_south)

# Get plans matrix
south_smc_plans_no_multi <- redist::get_plans_matrix(sim_smc_south_no_multi)

## Check contiguity
# Create new data frames
cols <- c("unit", "code", "sub_code", "sub_name",
          "mun_name", "gun_code", "geometry")
new_rows <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
south_sep <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:nrow(south_map)) {
  # Convert multipolygons to polygons
  new_rows <- data.frame(unit = i,
                         code = south_map[i, ]$code,
                         sub_code = south_map[i, ]$sub_code,
                         mun_name = south_map[i, ]$mun_name,
                         sub_name = south_map[i, ]$sub_name,
                         gun_code = south_map[i, ]$gun_code,
                         geometry = sf::st_cast(south_map[i, ]$geometry, "POLYGON"))

  # Order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Ignore all areas except for the largest areas in each municipality
    dplyr::filter(row_number()==1) %>%
    dplyr::select(-area)

  south_sep <- rbind(south_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert to sf
south_largest <- sf::st_as_sf(south_sep)

# TODO (Optional)
# Add other smaller areas to `pref_largest` as necessary
# By default, `pref_largest` only includes the largest areas in each municipality/gun
# If it is necessary to check the contiguity with other smaller areas,
# add those areas

# Create new data frame
add_small <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# Municipality codes of the areas to add
add_small_code <- c(11245, #ふじみ野市
                    11340) #比企郡

add_small_unit <- south_sep$unit[south_sep$code %in% add_small_code | south_sep$gun_code %in% add_small_code]

# Create data frame
south_sep_add <- south_sep

# # To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:length(add_small_unit)){
  add_small <-
    data.frame(unit = add_small_unit[i],
               code = south_map[add_small_unit[i], ]$code,
               sub_code = south_map[add_small_unit[i], ]$sub_code,
               mun_name = south_map[add_small_unit[i], ]$mun_name,
               sub_name = south_map[add_small_unit[i], ]$sub_name,
               gun_code = south_map[add_small_unit[i], ]$gun_code,
               geometry = sf::st_cast(south_map[add_small_unit[i], ]$geometry, "POLYGON"))

  # order by size
  add_small <- add_small %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Add areas that are not the largest polygon within the municipality/gun
    # Note: Change which area to add back to south_sep depending on needs
    # By default, all the areas that belong to the municipalities in `add_small_code` are added back
    dplyr::filter(row_number()!=1) %>%
    dplyr::select(-area)

  # row bind
  south_sep_add <- rbind(south_sep_add, add_small)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert into shapefile
south_largest <- sf::st_as_sf(south_sep_add)

# Ignore islands and isolated areas
south_largest_adj <- redist::redist.adjacency(south_largest)
mainland_south <- south_largest[which(unlist(lapply(south_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland
mainland_south_adj <- redist::redist.adjacency(mainland_south)

# TODO: Repair adjacency list if necessary
# Suggest connection between disconnected groups
# suggest <- geomander::suggest_component_connection(shp = mainland_south,
#                                                    adj = mainland_south_adj)
# mainland_south_adj <- geomander::add_edge(mainland_south_adj,
#                                                   suggest$x,
#                                                   suggest$y)
# mainland_south_add_edge <-
#   matrix(c(
#   ), ncol = 2, byrow = TRUE)
# # Add edges
# mainland_south_adj <- geomander::add_edge(mainland_south_adj,
#                                     mainland_south_add_edge[,1],
#                                     mainland_south_add_edge[,2])

# Check valid results
results_south_no_multi$valid <- check_contiguous(south_smc_plans_no_multi,
                                                 mainland_south,
                                                 mainland_south_adj)

# Filter out plans with discontiguities
functioning_results_south <- results_south_no_multi %>%
  dplyr::filter(valid == TRUE)

# nrow(functioning_results_south) must be over `nsims_final`.
# If not, increase nsims and run more simulations.

# Sample `nsims_all` plans
set.seed(2020)
valid_sample_south <- functioning_results_south %>%
  pull(draw) %>%
  sample(nsims_all, replace = FALSE)

# Sampled plans & reference plan
results_sample_south <- results_south_no_multi %>%
  dplyr::filter(draw %in% valid_sample_south | draw == "lh_2022")

# Sample plans
sim_smc_south_sample <- sim_smc_south %>%
  dplyr::filter(draw %in% valid_sample_south | draw == "lh_2022")

# Check summary statistics: sampled plans
sim_smc_south %>%
  dplyr::filter(draw %in% valid_sample_south | draw == "lh_2022") %>%
  partisan_metrics_japan(south_map) %>%
  dplyr::left_join(results_sample_south %>%
                     dplyr::select(mun_split,
                                   gun_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()

# All simulated plans
sim_smc_south %>%
  partisan_metrics_japan(south_map) %>%
  dplyr::left_join(results_south %>%
                     dplyr::select(mun_split,
                                   gun_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()

##### Pull everything together #####
init <- prep_particles(
  map = pref_map_merged,
  map_plan_list = list(
    south = list(map = south_map,
                 plans = sim_smc_south_sample %>%
                   mutate(keep = district > 0))
  ),
  uid = uid,
  dist_keep = keep,
  nsims = nsims_all)

# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref, strength = 1, admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref, strength = 1, admin = pref_map_merged$code)
# Run simulation
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims_all,
  runs = 8L,
  # Vector of municipality codes
  counties = pref_map_merged$code,
  constraints = constr_pref,
  init_particles = init,
  pop_temper = 0.02,
  seq_alpha = 0.90)

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
                                     as.character(nsims_all * 8),
                                     ".Rds",
                                     sep = "")))
