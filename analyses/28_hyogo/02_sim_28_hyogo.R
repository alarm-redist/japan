###############################################################################
# Simulations for `28_hyogo`
# © ALARM Project, May 2023
###############################################################################

####-------------- 2. Method for Urban Prefectures-------------------------####
# Re-order and add 郡 codes
pref <- pref_mun %>%
  arrange(code, sub_code) %>%
  merge_gun()

# Make adjacency list
prefadj <- redist::redist.adjacency(pref)

# Modify according to ferry adjacencies
ferries <- add_ferries(pref)
prefadj <- geomander::add_edge(prefadj,
                               ferries[, 1],
                               ferries[, 2],
                               zero = TRUE)

# Suggest connection between disconnected groups
suggest <-  geomander::suggest_component_connection(shp = pref,
                                                    adj = prefadj)
prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)

# Repair adjacencies
# 芦屋市-西宮市西宮浜
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 28206),
                               which(pref$code == 28204 & pref$sub_code == 1910))
# 西宮市西宮浜-西宮市甲子園浜
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 28204 & pref$sub_code == 1910),
                               which(pref$code == 28204 & pref$sub_code == 2650))
# 西宮市甲子園浜-西宮市鳴尾浜
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 28204 & pref$sub_code == 2650),
                               which(pref$code == 28204 & pref$sub_code == 2710))
# 西宮市鳴尾浜-尼崎市
prefadj <- geomander::add_edge(prefadj,
                               which(pref$code == 28204 & pref$sub_code == 2710),
                               which(pref$code == 28202))

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
  mutate(freeze_code = if_else(gun_code %in% c(gun_exception, split_code_lh_2022),
                               code,
                               gun_code)) %>%
  # Group by and merge by `gun_code`
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Drop column `freeze_code`
  select(-freeze_code)

# 西播磨県民局: 相生市、たつの市、赤穂市、宍粟市、太子町、上郡町、佐用町
# 中播磨県民センター: 姫路市、神河町、市川町、福崎町
# 東播磨県民局: 明石市、加古川市、高砂市、稲美町、播磨町
# 淡路県民局: 洲本市、南あわじ市、淡路市
west_code <- c(28208, 28229, 28212, 28227, 28464, 28481, 28501,
               28201, "28442~28443~28446",
               28203, 28210, 28216, "28381~28382",
               28205, 28224, 28226)
east_code <- setdiff(unique(pref_map_merged$code), west_code)

# Calculate target population & number of districts
# Target population
target_pop <- sum(pref_map_merged$pop)/ndists_new

# West
ndists_new_west <-
  round(
    sum(pref_map_merged$pop[which(pref_map_merged$code %in% west_code)]) / target_pop
  )
# East
ndists_new_east <-
  round(
    sum(pref_map_merged$pop[which(pref_map_merged$code %in% east_code)]) / target_pop
  )

# Western Hyogo Map
west_map <- pref_map_merged %>%
  filter(code %in% west_code) %>%
  `attr<-`("ndists", ndists_new_west) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

###### Western Hyogo ######
# Simulate Western Hyogo
# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_west = redist::redist_constr(west_map)
# constr_west = redist::add_constr_splits(constr_west,
#                                         strength = 1,
#                                         admin = west_map$code)
constr_west = redist::add_constr_multisplits(constr_west,
                                             strength = 1,
                                             admin = west_map$code)

# Run simulation
set.seed(2020)
sim_smc_west <- redist::redist_smc(
  map = west_map,
  nsims = nsims_init,
  runs = 8L,
  # Vector of municipality codes
  counties = west_map$code,
  constraints = constr_west,
  pop_temper = 0.03,
  seq_alpha = 0.95)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_west)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_west))

######
# Filter out valid plans for the Special Wards area
# Get plans matrix
west_smc_plans <- redist::get_plans_matrix(sim_smc_west)

# Calculate max:min ratio
wgt_smc_west <- simulation_weight_disparity_table(sim_smc_west)

# Count number of municipality splits
num_mun_split_west <- count_splits(west_smc_plans, west_map$code)
mun_split_west <- redist::redist.splits(west_smc_plans, west_map$code) %>%
  matrix(ncol = ndists_new_west, byrow = TRUE)
mun_split_west <- mun_split_west[,1]

# Compile results
results_west <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_west)))
results_west$max_to_min <- wgt_smc_west$max_to_min
results_west$num_mun_split <- num_mun_split_west
results_west$mun_split <- mun_split_west
results_west$multi <-  num_mun_split_west - mun_split_west
results_west$draw <- wgt_smc_west$draw

# Filter out plans with 0 multi-splits
no_multi_west <- results_west %>%
  filter(multi == 0) %>%
  pull(draw)

# Results for plans with 0 multi-splits
results_west_no_multi <- results_west %>%
  dplyr::filter(draw %in% no_multi_west)

# Plans with 0 multi-splits
sim_smc_west_no_multi <- sim_smc_west %>%
  dplyr::filter(draw %in% no_multi_west)

# Get plans matrix
west_smc_plans_no_multi <- redist::get_plans_matrix(sim_smc_west_no_multi)

## Check contiguity
# Create new data frames
cols <- c("unit", "code", "sub_code", "sub_name",
          "mun_name", "gun_code", "geometry")
new_rows <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
west_sep <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:nrow(west_map)) {
  # Convert multipolygons to polygons
  new_rows <- data.frame(unit = i,
                         code = west_map[i, ]$code,
                         sub_code = west_map[i, ]$sub_code,
                         mun_name = west_map[i, ]$mun_name,
                         sub_name = west_map[i, ]$sub_name,
                         gun_code = west_map[i, ]$gun_code,
                         geometry = sf::st_cast(west_map[i, ]$geometry, "POLYGON"))

  # Order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Ignore all areas except for the largest areas in each municipality
    dplyr::filter(row_number()==1) %>%
    dplyr::select(-area)

  west_sep <- rbind(west_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert to sf
west_largest <- sf::st_as_sf(west_sep)

# TODO (Optional)
# Add other smaller areas to `pref_largest` as necessary
# By default, `pref_largest` only includes the largest areas in each municipality/gun
# If it is necessary to check the contiguity with other smaller areas,
# add those areas

# # Create new data frame
# add_small <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
#
# # Municipality codes of the areas to add
# add_small_code <- c()
#
# add_small_unit <- west_sep$unit[west_sep$code %in% add_small_code | west_sep$gun_code %in% add_small_code]
#
# # Create data frame
# west_sep_add <- west_sep
#
# # # To calculate area size, switch off `geometry (s2)`
# sf_use_s2(FALSE)
#
# for (i in 1:length(add_small_unit)){
#   add_small <-
#     data.frame(unit = add_small_unit[i],
#                code = west_map[add_small_unit[i], ]$code,
#                sub_code = west_map[add_small_unit[i], ]$sub_code,
#                mun_name = west_map[add_small_unit[i], ]$mun_name,
#                sub_name = west_map[add_small_unit[i], ]$sub_name,
#                gun_code = west_map[add_small_unit[i], ]$gun_code,
#                geometry = sf::st_cast(west_map[add_small_unit[i], ]$geometry, "POLYGON"))
#
#   # order by size
#   add_small <- add_small %>%
#     dplyr::mutate(area = sf::st_area(geometry)) %>%
#     dplyr::arrange(desc(area)) %>%
#     # Add areas that are not the largest polygon within the municipality/gun
#     # Note: Change which area to add back to west_sep depending on needs
#     # By default, all the areas that belong to the municipalities in `add_small_code` are added back
#     dplyr::filter(row_number()!=1) %>%
#     dplyr::select(-area)
#
#   # row bind
#   west_sep_add <- rbind(west_sep_add, add_small)
# }
#
# # switch on `geometry (s2)`
# sf_use_s2(TRUE)
#
# # Convert into shapefile
# west_largest <- sf::st_as_sf(west_sep_add)


# Ignore islands and isolated areas
west_largest_adj <- redist::redist.adjacency(west_largest)
mainland_west <- west_largest[which(unlist(lapply(west_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland
mainland_west_adj <- redist::redist.adjacency(mainland_west)

# TODO: Repair adjacency list if necessary
# Suggest connection between disconnected groups
suggest_west <- geomander::suggest_component_connection(shp = mainland_west,
                                                        adj = mainland_west_adj)
mainland_west_adj <- geomander::add_edge(mainland_west_adj,
                                         suggest_west$x,
                                         suggest_west$y)

# Check valid results
results_west_no_multi$valid <- check_contiguous(west_smc_plans_no_multi,
                                                mainland_west,
                                                mainland_west_adj)

# Filter out plans with discontiguities
functioning_results_west <- results_west_no_multi %>%
  dplyr::filter(valid == TRUE)

# nrow(functioning_results_west) must be over `nsims_final`.
# If not, increase nsims and run more simulations.

# Sample `nsims_all` plans
set.seed(2020)
valid_sample_west <- functioning_results_west %>%
  pull(draw) %>%
  sample(nsims_all, replace = FALSE)

# Sampled plans & reference plan
results_sample_west <- results_west_no_multi %>%
  dplyr::filter(draw %in% valid_sample_west | draw == "lh_2022")

# Sample plans
sim_smc_west_sample <- sim_smc_west %>%
  dplyr::filter(draw %in% valid_sample_west | draw == "lh_2022")

# Check summary statistics: sampled plans
sim_smc_west %>%
  dplyr::filter(draw %in% valid_sample_west | draw == "lh_2022") %>%
  partisan_metrics_japan(west_map) %>%
  dplyr::left_join(results_sample_west %>%
                     dplyr::select(mun_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()


# All simulated plans
sim_smc_west %>%
  partisan_metrics_japan(west_map) %>%
  dplyr::left_join(results_west %>%
                     dplyr::select(mun_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()

# Note that it was impossible to increase the diversity of the plans further.

##### Pull everything together #####
init <- prep_particles(
  map = pref_map_merged,
  map_plan_list = list(
    west = list(map = west_map,
                 plans = sim_smc_west_sample %>%
                   mutate(keep = district > 0))
  ),
  uid = uid,
  dist_keep = keep,
  nsims = nsims_all)

# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref,
                                        strength = 1,
                                        admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref,
                                             strength = 1,
                                             admin = pref_map_merged$code)

# Simulate the remainder (Eastern Region) and pull everything together
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims_all,
  runs = 8L,
  counties = pref_map_merged$code,
  constraints = constr_pref,
  init_particles = init,
  pop_temper = 0.03,
  seq_alpha = 0.95
)

# Add precinct population
attr(sim_smc_pref, "prec_pop") <- pref_map_merged$pop

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
