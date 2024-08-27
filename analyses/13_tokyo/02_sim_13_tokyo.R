###############################################################################
# Simulations for `13_tokyo`
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
pref_add_edge <-
  matrix(c(
    #大田区東海-大田区平和島
    which(pref$code == 13111 & pref$sub_code == 580),
    which(pref$code == 13111 & pref$sub_code == 90),
    #大田区東海-大田区京浜島
    which(pref$code == 13111 & pref$sub_code == 580),
    which(pref$code == 13111 & pref$sub_code == 600),
    #大田区昭和島-大田区大森東
    which(pref$code == 13111 & pref$sub_code == 100),
    which(pref$code == 13111 & pref$sub_code == 20),
    #大田区羽田空港-大田区京浜島
    which(pref$code == 13111 & pref$sub_code == 430),
    which(pref$code == 13111 & pref$sub_code == 600),
    #大田区羽田空港-大田区大森東
    which(pref$code == 13111 & pref$sub_code == 430),
    which(pref$code == 13111 & pref$sub_code == 20),
    #大田区羽田空港-大田区東糀谷
    which(pref$code == 13111 & pref$sub_code == 430),
    which(pref$code == 13111 & pref$sub_code == 380),
    #大田区羽田空港-大田区羽田旭町
    which(pref$code == 13111 & pref$sub_code == 430),
    which(pref$code == 13111 & pref$sub_code == 410),
    #大田区令和島-大田区城南島
    which(pref$code == 13111 & pref$sub_code == 811),
    which(pref$code == 13111 & pref$sub_code == 590),
    #大田区平和島-品川区勝島
    which(pref$code == 13111 & pref$sub_code == 90),
    which(pref$code == 13109 & pref$sub_code == 40),
    #品川区八潮-品川区東八潮
    which(pref$code == 13109 & pref$sub_code == 250),
    which(pref$code == 13109 & pref$sub_code == 270),
    #品川区八潮-品川区東大井
    which(pref$code == 13109 & pref$sub_code == 250),
    which(pref$code == 13109 & pref$sub_code == 160),
    #品川区八潮-品川区東品川
    which(pref$code == 13109 & pref$sub_code == 250),
    which(pref$code == 13109 & pref$sub_code == 180),
    #品川区東大井-品川区勝島
    which(pref$code == 13109 & pref$sub_code == 160),
    which(pref$code == 13109 & pref$sub_code == 40),
    #港区海岸-港区台場
    which(pref$code == 13103 & pref$sub_code == 20),
    which(pref$code == 13103 & pref$sub_code == 300)
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
                               gun_code)) %>%
  # Group by and merge by `gun_code`
  merge_by(freeze_code, by_existing = FALSE, drop_geom = FALSE) %>%
  # Drop column `freeze_code`
  select(-freeze_code)


# Separate Tokyo into blocks
# Special Wards Area vs Tama Area
# According to the redistricting rules, no district should cut across the boundary
# between the Special Wards Area and the Tama Area. Thus, redistricting is conducted
# separately for those two areas.
special_wards_code <- c(13101:13123,
                        "13361~13362~13363~13364", "13381~13382", "13401~13402", 13421)
tama_code <- setdiff(unique(pref_map_merged$code), special_wards_code)

# Calculate target population & number of districts
# Target population
target_pop <- sum(pref_map_merged$pop)/ndists_new
# Koto Ward
ndists_new_koto <- 1
# Setagaya
ndists_new_setagaya <- round(
  sum(pref_map_merged$pop[which(pref_map_merged$code == 13112)]) / target_pop
  )
# Special Wards
ndists_new_special_wards <-
  round(
    sum(pref_map_merged$pop[which(pref_map_merged$code %in% special_wards_code)]) / target_pop
  ) -
  ndists_new_setagaya -
  ndists_new_koto
# Tama
ndists_new_tama <- round(
  sum(pref_map_merged$pop[which(pref_map_merged$code %in% tama_code)]) / target_pop
)

# Set aside Koto Ward and Setagaya Ward
# Koto Ward is set aside because its population exceeds the target population.
# Setagaya Ward is set aside because its population is more than twice as large as
# the target population. Thus, unless two seats are apportioned to Setagaya Ward,
# there is a high chance that it will be split into more than 2 districts in the simulations.

# Koto Ward Map
koto_map <- pref_map_merged %>%
  filter(code %in% 13108) %>%
  `attr<-`("ndists", ndists_new_koto) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

# Setagaya Ward Map
setagaya_map <- pref_map_merged %>%
  filter(code %in% 13112) %>%
  `attr<-`("ndists", ndists_new_setagaya) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

# Special Ward Map (excluding for Setagaya and Koto)
special_wards_map <- pref_map_merged %>%
  filter(code %in% special_wards_code) %>%
  filter(code %in% c(13108, 13112) == FALSE) %>%
  `attr<-`("ndists", ndists_new_special_wards) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

# Tama map (excluding for Setagaya and Koto)
tama_map <- pref_map_merged %>%
  filter(code %in% tama_code) %>%
  `attr<-`("ndists", ndists_new_tama) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

###### Special Wards ######
# Simulate Special Wards area
# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_special_wards = redist::redist_constr(special_wards_map)
constr_special_wards = redist::add_constr_splits(constr_special_wards,
                                                 strength = 1,
                                                 admin = special_wards_map$code)
constr_special_wards = redist::add_constr_multisplits(constr_special_wards,
                                                      strength = 3,
                                                      admin = special_wards_map$code)

# Run simulation
set.seed(2020)
sim_smc_special_wards <- redist::redist_smc(
  map = special_wards_map,
  nsims = nsims_init,
  runs = 8L,
  # Vector of municipality codes
  counties = special_wards_map$code,
  constraints = constr_special_wards,
  pop_temper = 0.01,
  seq_alpha = 0.95)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_special_wards)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_special_wards))


######
# Filter out valid plans for the Special Wards area
# Get plans matrix
special_wards_smc_plans <- redist::get_plans_matrix(sim_smc_special_wards)

# Calculate max:min ratio
wgt_smc_special_wards <- simulation_weight_disparity_table(sim_smc_special_wards)

# Count number of municipality splits
num_mun_split_special_wards <- count_splits(special_wards_smc_plans, special_wards_map$code)
mun_split_special_wards <- redist::redist.splits(special_wards_smc_plans, special_wards_map$code) %>%
  matrix(ncol = ndists_new_special_wards, byrow = TRUE)
mun_split_special_wards <- mun_split_special_wards[,1]

# Compile results
results_special_wards <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_special_wards)))
results_special_wards$max_to_min <- wgt_smc_special_wards$max_to_min
results_special_wards$num_mun_split <- num_mun_split_special_wards
results_special_wards$mun_split <- mun_split_special_wards
results_special_wards$multi <-  num_mun_split_special_wards - mun_split_special_wards
results_special_wards$draw <- wgt_smc_special_wards$draw

# Filter out plans with 0 multi-splits
no_multi_special_wards <- results_special_wards %>%
  filter(multi == 0) %>%
  pull(draw)

# Results for plans with 0 multi-splits
results_special_wards_no_multi <- results_special_wards %>%
  dplyr::filter(draw %in% no_multi_special_wards)

# Plans with 0 multi-splits
sim_smc_special_no_multi <- sim_smc_special_wards %>%
  dplyr::filter(draw %in% no_multi_special_wards)

# Get plans matrix
special_wards_smc_plans_no_multi <- redist::get_plans_matrix(sim_smc_special_no_multi)

## Check contiguity
# Create new data frames
cols <- c("unit", "code", "sub_code", "sub_name",
          "mun_name", "gun_code", "geometry")
new_rows <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
special_wards_sep <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:nrow(special_wards_map)) {
  # Convert multipolygons to polygons
  new_rows <- data.frame(unit = i,
                         code = special_wards_map[i, ]$code,
                         sub_code = special_wards_map[i, ]$sub_code,
                         mun_name = special_wards_map[i, ]$mun_name,
                         sub_name = special_wards_map[i, ]$sub_name,
                         gun_code = special_wards_map[i, ]$gun_code,
                         geometry = sf::st_cast(special_wards_map[i, ]$geometry, "POLYGON"))

  # Order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Ignore all areas except for the largest areas in each municipality
    dplyr::filter(row_number()==1) %>%
    dplyr::select(-area)

  special_wards_sep <- rbind(special_wards_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert to sf
special_wards_largest <- sf::st_as_sf(special_wards_sep)

# Ignore islands and isolated areas
special_wards_largest_adj <- redist::redist.adjacency(special_wards_largest)
mainland_special_wards <- special_wards_largest[which(unlist(lapply(special_wards_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland
mainland_special_wards_adj <- redist::redist.adjacency(mainland_special_wards)

# TODO: Repair adjacency list if necessary
# Suggest connection between disconnected groups
suggest <- geomander::suggest_component_connection(shp = mainland_special_wards,
                                                   adj = mainland_special_wards_adj)
mainland_special_wards_adj <- geomander::add_edge(mainland_special_wards_adj,
                                                  suggest$x,
                                                  suggest$y)
# Repair adjacencies
mainland_add_edge <-
  matrix(c(
    #大田区東海-大田区平和島
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 580),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 90),
    #大田区東海-大田区京浜島
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 580),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 600),
    #大田区昭和島-大田区大森東
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 100),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 20),
    #大田区羽田空港-大田区京浜島
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 430),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 600),
    #大田区羽田空港-大田区大森東
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 430),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 20),
    #大田区羽田空港-大田区東糀谷
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 430),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 380),
    #大田区羽田空港-大田区羽田旭町
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 430),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 410),
    #大田区令和島-大田区城南島
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 811),
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 590),
    #大田区平和島-品川区勝島
    which(mainland_special_wards$code == 13111 & mainland_special_wards$sub_code == 90),
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 40),
    #品川区八潮-品川区東八潮
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 250),
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 270),
    #品川区八潮-品川区東大井
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 250),
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 160),
    #品川区八潮-品川区東品川
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 250),
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 180),
    #品川区東大井-品川区勝島
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 160),
    which(mainland_special_wards$code == 13109 & mainland_special_wards$sub_code == 40),
    #港区海岸-港区台場
    which(mainland_special_wards$code == 13103 & mainland_special_wards$sub_code == 20),
    which(mainland_special_wards$code == 13103 & mainland_special_wards$sub_code == 300)
  ), ncol = 2, byrow = TRUE)


mainland_special_wards_adj <- geomander::add_edge(mainland_special_wards_adj,
                                                  mainland_add_edge[,1],
                                                  mainland_add_edge[,2])

# Check valid results
results_special_wards_no_multi$valid <- check_contiguous(special_wards_smc_plans_no_multi,
                                                         mainland_special_wards,
                                                         mainland_special_wards_adj)

# Filter out plans with discontiguities
functioning_results_special_wards <- results_special_wards_no_multi %>%
  dplyr::filter(valid == TRUE)

# nrow(functioning_results_special_wards) must be over `nsims_final`.
# If not, increase nsims and run more simulations.

# Sample `nsims_all` plans
set.seed(2020)
valid_sample_special_wards <- functioning_results_special_wards %>%
  pull(draw) %>%
  sample(nsims_all, replace = FALSE)

# Sampled plans & reference plan
results_sample_special_wards <- results_special_wards_no_multi %>%
  dplyr::filter(draw %in% valid_sample_special_wards | draw == "lh_2022")

# Sample plans
sim_smc_special_wards_sample <- sim_smc_special_wards %>%
  dplyr::filter(draw %in% valid_sample_special_wards | draw == "lh_2022")

# Check summary statistics: sampled plans
sim_smc_special_wards %>%
  dplyr::filter(draw %in% valid_sample_special_wards | draw == "lh_2022") %>%
  partisan_metrics_japan(special_wards_map) %>%
  dplyr::left_join(results_sample_special_wards %>%
                     dplyr::select(mun_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>% summary()


# All simulated plans
sim_smc_special_wards %>%
  partisan_metrics_japan(special_wards_map) %>%
  dplyr::left_join(results_special_wards %>%
                     dplyr::select(mun_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()

##### Setagaya Ward #####
# Run simulation: Setagaya
set.seed(2020)
sim_smc_setagaya <- redist::redist_smc(
  map = setagaya_map,
  nsims = nsims_setagaya,
  runs = 4L,
  pop_temper = 0,
  seq_alpha = 0.5)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_setagaya)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_setagaya))

# Subsample `nsims_all` plans to init next stage
# Setagaya Ward
set.seed(2020)
sample_setagaya <- sample(unique(sim_smc_setagaya$draw), nsims_all, replace = FALSE)
# Sample `nsims_all` plans
sim_smc_setagaya_sample <- sim_smc_setagaya %>%
  dplyr::filter(draw %in% sample_setagaya)

# Summary statistics: simulated plans
sim_smc_setagaya_sample %>%
  partisan_metrics_japan(setagaya_map) %>%
  summary()

# All simulated plans
sim_smc_setagaya %>%
  partisan_metrics_japan(setagaya_map) %>%
  summary()


####### Koto Ward #####
# Run simulation: Koto
# One district is allocated to Koto Ward, whose population exceeds the target population
set.seed(2020)
sim_smc_koto <- redist::redist_smc(
  map = koto_map,
  nsims = nsims_all) # Obtain `nsims_all` plans (all of them are the same)

##### Pull everything together #####
init <- prep_particles(
  map = pref_map_merged,
  map_plan_list = list(
    koto = list(
      map = koto_map,
      plans = sim_smc_koto %>% mutate(keep = district > 0)
    ),
    setagaya = list(
      map = setagaya_map,
      plans = sim_smc_setagaya_sample %>% mutate(keep = district > 0)
    ),
    special_wards = list(
      map = special_wards_map,
      plans = sim_smc_special_wards_sample %>% mutate(keep = district > 0)
    )
  ),
  uid = uid,
  dist_keep = keep,
  nsims = nsims_all
)

# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_pref = redist::redist_constr(pref_map_merged)
constr_pref = redist::add_constr_splits(constr_pref,
                                        strength = 1,
                                        admin = pref_map_merged$code)
constr_pref = redist::add_constr_multisplits(constr_pref,
                                             strength = 1,
                                             admin = pref_map_merged$code)

# Simulate the remainder (Tama area) and pull everything together
set.seed(2020)
sim_smc_pref <- redist::redist_smc(
  map = pref_map_merged,
  nsims = nsims_all,
  runs = 4L,
  counties = pref_map_merged$code,
  constraints = constr_pref,
  init_particles = init,
  pop_temper = 0.01,
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
                                     as.character(nsims_all * 4),
                                     ".Rds",
                                     sep = "")))
