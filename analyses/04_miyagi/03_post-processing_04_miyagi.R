###############################################################################
# Post-processing for `04_miyagi`
# © ALARM Project, March 2023
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define using gun_code if gun was merged
koiki_1_codes <- c(4202, 4214, 4581)
koiki_2_codes <- c(4215, 4444, 4445, 4500)

# Load data
pref_map <- readRDS(here(paste("data-out/map/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_lh_2022_map.rds",
                               sep = "")))

prefadj <- readRDS(here(paste("data-out/adj/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_adj.Rds",
                              sep = "")))

sim_smc_pref_ref <- readRDS(here(paste("data-out/smc-out/",
                                       as.character(pref_code),
                                       "_",
                                       as.character(pref_name),
                                       "_",
                                       as.character(sim_type),
                                       "_",
                                       as.character(nsims * 4),
                                       ".Rds",
                                       sep = "")))

# Get plans matrix
pref_smc_plans <- redist::get_plans_matrix(sim_smc_pref_ref)

# Calculate max:min ratio
wgt_smc <- simulation_weight_disparity_table(sim_smc_pref_ref)

# Assign koiki_renkei area codes
koiki_1 <- pref_map$pre_gappei_code
koiki_1[pref_map$code %in% koiki_1_codes |
          pref_map$gun_code %in% koiki_1_codes] <- 1

koiki_2 <- pref_map$pre_gappei_code
koiki_2[pref_map$code %in% koiki_2_codes |
          pref_map$gun_code %in% koiki_2_codes] <- 2

# Count number of municipality splits
num_mun_split <- count_splits(pref_smc_plans, pref_map$code)
mun_split <- redist::redist.splits(pref_smc_plans, pref_map$code) %>%
  matrix(ncol = ndists_new, byrow = TRUE)
mun_split <- mun_split[,1]

# Count number of gun splits
gun_split <- redist::redist.splits(pref_smc_plans, pref_map$gun_code) %>%
  matrix(ncol = ndists_new, byrow = TRUE)
gun_split <- gun_split[,1]

# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(pref_smc_plans, koiki_1) +
  redist::redist.splits(pref_smc_plans, koiki_2)
koiki_split <- koiki_split %>%
  matrix(ncol = ndists_new, byrow = TRUE)
koiki_split <- koiki_split[,1]

# Compile results
results <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results$max_to_min <- wgt_smc$max_to_min
results$gun_split <- gun_split
results$num_mun_split <- num_mun_split
results$mun_split <- mun_split
results$multi <-  num_mun_split - mun_split
results$koiki_split <- koiki_split
results$draw <- wgt_smc$draw

## Check contiguity
# Create new data frames
cols <- c("unit", "code", "pre_gappei_code", "old_mun_name",
          "mun_name", "gun_code", "geometry")
new_rows <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
pref_sep <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:nrow(pref_map)) {
  # Convert multipolygons to polygons
  new_rows <- data.frame(unit = i,
                         code = pref_map[i, ]$code,
                         pre_gappei_code = pref_map[i, ]$pre_gappei_code,
                         mun_name = pref_map[i, ]$mun_name,
                         old_mun_name = pref_map[i, ]$old_mun_name,
                         gun_code = pref_map[i, ]$gun_code,
                         geometry = sf::st_cast(pref_map[i, ]$geometry, "POLYGON"))

  # Order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Ignore all areas except for the largest areas in each municipality
    dplyr::filter(row_number()==1) %>%
    dplyr::select(-area)

  pref_sep <- rbind(pref_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert to sf
pref_largest <- sf::st_as_sf(pref_sep)

# TODO (Optional)
# Add other smaller areas to `pref_largest` as necessary
# # By default, `pref_largest` only includes the largest areas in each municipality/gun
# # If it is necessary to check the contiguity with other smaller areas,
# # add those areas
#
# Create new data frame
# add_small <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
#
# # Municipality codes of the areas to add
# add_small_code <- c()
# add_small_unit <- pref_sep$unit[pref_sep$code %in% add_small_code]
#
# # Create data frame
# pref_sep_add <- pref_sep
#
# # To calculate area size, switch off `geometry (s2)`
# sf_use_s2(FALSE)
#
# for (i in 1:length(add_small_unit))
# {
#   add_small <-
#     data.frame(unit = add_small_unit[i],
#                code = pref_map[add_small_unit[i], ]$code,
#                pre_gappei_code = pref_map[add_small_unit[i], ]$pre_gappei_code,
#                mun_name = pref_map[add_small_unit[i], ]$mun_name,
#                old_mun_name = pref_map[add_small_unit[i], ]$old_mun_name,
#                gun_code = pref_map[add_small_unit[i], ]$gun_code,
#                geometry = sf::st_cast(pref_map[add_small_unit[i], ]$geometry, "POLYGON"))
#
#   # order by size
#   add_small <- add_small %>%
#     dplyr::mutate(area = sf::st_area(geometry)) %>%
#     dplyr::arrange(desc(area)) %>%
#     # Add areas that are not the largest polygon within the municipality/gun
#     # Note: Change which area to add back to pref_sep depending on needs
#     # By default, all the areas that belong to the municipalities in `add_small_code` are added back
#     dplyr::filter(row_number()!=1) %>%
#     dplyr::select(-area)
#
#   # row bind
#   pref_sep_add <- rbind(pref_sep_add, add_small)
# }
#
# # switch on `geometry (s2)`
# sf_use_s2(TRUE)
#
# # Convert into shapefile
# pref_largest <- sf::st_as_sf(pref_sep_add)

# Ignore islands and isolated areas
pref_largest_adj <- redist::redist.adjacency(pref_largest)
mainland <- pref_largest[which(unlist(lapply(pref_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland
mainland_adj <- redist::redist.adjacency(mainland)

# TODO: Repair adjacency list if necessary
# Suggest connection between disconnected groups
# suggest <- geomander::suggest_component_connection(shp = mainland,
#                                                     adj = mainland_adj)
# mainland_adj <- geomander::add_edge(mainland_adj,
#                                     suggest$x,
#                                     suggest$y)


# Check valid results
results$valid <- check_contiguous(pref_smc_plans,
                                  mainland,
                                  mainland_adj)

# Filter out plans with discontiguities
functioning_results <- results %>%
  dplyr::filter(multi == 0 &
                  valid == TRUE)

# nrow(functioning_results) must be over 5,000.
# If not, increase nsims and run more simulations.

# Sample 5,000 plans
set.seed(2020)
valid_sample <- functioning_results %>%
  filter(draw != "lh_2022") %>%
  pull(draw) %>%
  sample(5000, replace = FALSE)

# 5,000 sampled plans & reference plan
results_sample <- results %>%
  dplyr::filter(draw %in% valid_sample | draw == "lh_2022")

# Add summary statistics to the sampled `redist_plan`
sim_smc_pref_sample <- sim_smc_pref_ref %>%
  dplyr::filter(draw %in% valid_sample | draw == "lh_2022") %>%
  partisan_metrics_japan(pref_map) %>%
  dplyr::left_join(results_sample %>%
                     dplyr::select(mun_split,
                                   gun_split,
                                   koiki_split,
                                   max_to_min,
                                   draw),
                   by = "draw")

# Check the summary statistics
# Sampled 5,000 plans
summary(sim_smc_pref_sample)

# All simulated plans
sim_smc_pref_ref %>%
  partisan_metrics_japan(pref_map) %>%
  dplyr::left_join(results %>%
                     dplyr::select(mun_split,
                                   gun_split,
                                   koiki_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()

# Check the validation of the sampled plans
validate_analysis_japan(sim_smc_pref_sample, pref_map, pref_code, pref_name)

# Save relevant files to upload to Dataverse
# `redist_plans` object
write_rds(sim_smc_pref_sample,
          here(paste("data-out/plans/",
                     as.character(pref_code),
                     "_",
                     as.character(pref_name),
                     "_lh_2022_plans.rds",
                     sep = "")),
               compress = "xz")

# Export `redist_plans` summary statistics to a csv file
as_tibble(sim_smc_pref_sample) %>%
  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
  write_csv(here(paste("data-out/stats/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "_lh_2022_stats.csv",
                       sep = "")))
