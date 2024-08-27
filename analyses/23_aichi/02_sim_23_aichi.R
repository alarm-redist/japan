###############################################################################
# Simulations for `23_aichi`
# © ALARM Project, March 2023
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
pref_add_edge <-
  matrix(c(
    # 名古屋市中川区-中区
    which(pref$code == 23110),
    which(pref$code == 23106),

    # 名古屋市中村区-中区
    which(pref$code == 23105),
    which(pref$code == 23106),

    # 名古屋市熱田区-南区
    which(pref$code == 23109),
    which(pref$code == 23112),

    # 名古屋市熱田区-瑞穂区
    which(pref$code == 23109),
    which(pref$code == 23108),

    # 名古屋市熱田区-昭和区
    which(pref$code == 23109),
    which(pref$code == 23107),

    # 名古屋市港区-東海市
    which(pref$code == 23111),
    which(pref$code == 23222),

    # 名古屋市港区-海部郡飛島町
    which(pref$code == 23111),
    which(pref$code == 23427),

    # 名古屋市南区-東海市
    which(pref$code == 23112),
    which(pref$code == 23222)

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

# We separate Aichi into two regions using historical (pre-Meiji) boundaries
# because the SMC algorithm does not converge when running it for the entire prefecture.
# We assign seats proportionally to each region.
# The Owari region and Mikawa region are administrative units that are
# used by the Aichi Prefectural Government (https://www.pref.aichi.jp/site/userguide/link-citytown.html).
# Both the old enacted plan and the newly enacted plan respect these regions.
mikawa <- c(23202, 23209, 23210, 23211, 23212, # 岡崎市, 碧南市, 刈谷市, 豊田市, 安城市
            23213, 23225, 23227, 23236, 23501, # 西尾市, 知立市, 高浜市, みよし市, 幸田町
            23201, 23207, 23214, 23221, 23231, # 豊橋市, 豊川市, 蒲郡市, 新城市, 田原市
            "23561~23562~23563") # 設楽町, 東栄町, 豊根村
owari <- setdiff(unique(pref_map_merged$code), mikawa)
# Calculate target population & number of districts
target_pop <- sum(pref_map_merged$pop)/ndists_new
# Calculate number of districts per region
ndists_new_owari <- round(
  sum(pref_map_merged$pop[which(pref_map_merged$code %in% owari)]) / target_pop
  )
# Mikawa
ndists_new_mikawa <- round(
  sum(pref_map_merged$pop[which(pref_map_merged$code %in% mikawa)]) / target_pop
)

# Owari Region Map
owari_map <- pref_map_merged %>%
  filter(code %in% owari) %>%
  `attr<-`("ndists", ndists_new_owari) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

# Mikawa Region Map
mikawa_map <- pref_map_merged %>%
  filter(code %in% mikawa) %>%
  `attr<-`("ndists", ndists_new_mikawa) %>%
  `attr<-`("pop_bounds", attr(pref_map_merged, "pop_bounds"))

####################
### Partial SMC  ###
####################
# Simulate Owari Region
# If there is a possibility of a "multi-split," add a multi-split constraint
# Comment out the following lines if you are not adding any constraints
constr_owari = redist::redist_constr(owari_map)
constr_owari = redist::add_constr_splits(constr_owari,
                                         strength = 2,
                                         admin = owari_map$code)
constr_owari = redist::add_constr_multisplits(constr_owari,
                                              strength = 5,
                                              admin = owari_map$code)

# Run simulation
set.seed(2020)
sim_smc_owari <- redist::redist_smc(
  map = owari_map,
  nsims = nsims_init,
  runs = 8L,
  # Vector of municipality codes
  counties = owari_map$code,
  constraints = constr_owari,
  pop_temper = 0.04,
  seq_alpha = 0.95)

# Check to see whether there are SMC convergence warnings
# If there are warnings, increase `nsims`
summary(sim_smc_owari)

# Histogram showing plans diversity
# Ideally, the majority of mass to would be above 50% and
# we would not see a large spike at 0.
# However, for some prefectures, it is impossible to get a diverse set of plans
# because there are fewer possible plans.
hist(plans_diversity(sim_smc_owari))

######
# Filter out valid plans for Owari Region
# Get plans matrix
owari_smc_plans <- redist::get_plans_matrix(sim_smc_owari)

# Calculate max:min ratio
wgt_smc_owari <- simulation_weight_disparity_table(sim_smc_owari)

# Count number of municipality splits
num_mun_split_owari <- count_splits(owari_smc_plans, owari_map$code)
mun_split_owari <- redist::redist.splits(owari_smc_plans, owari_map$code) %>%
  matrix(ncol = ndists_new_owari, byrow = TRUE)
mun_split_owari <- mun_split_owari[,1]

# Count number of gun splits
gun_split_owari <- redist::redist.splits(owari_smc_plans, owari_map$gun_code) %>%
  matrix(ncol = ndists_new_owari, byrow = TRUE)
gun_split <- gun_split_owari[,1]

# Compile results
results_owari <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_owari)))
results_owari$max_to_min <- wgt_smc_owari$max_to_min
results_owari$gun_split <- gun_split
results_owari$num_mun_split <- num_mun_split_owari
results_owari$mun_split <- mun_split_owari
results_owari$multi <-  num_mun_split_owari - mun_split_owari
results_owari$draw <- wgt_smc_owari$draw

# Filter out plans with 0 multi-splits
no_multi_owari <- results_owari %>%
  filter(multi == 0) %>%
  pull(draw)

# Results for plans with 0 multi-splits
results_owari_no_multi <- results_owari %>%
  dplyr::filter(draw %in% no_multi_owari)

# Plans with 0 multi-splits
sim_smc_owari_no_multi <- sim_smc_owari %>%
  dplyr::filter(draw %in% no_multi_owari)

# Get plans matrix
owari_smc_plans_no_multi <- redist::get_plans_matrix(sim_smc_owari_no_multi)

## Check contiguity
# Create new data frames
cols <- c("unit", "code", "sub_code", "sub_name",
          "mun_name", "gun_code", "geometry")
new_rows <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)
owari_sep <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:nrow(owari_map)) {
  # Convert multipolygons to polygons
  new_rows <- data.frame(unit = i,
                         code = owari_map[i, ]$code,
                         sub_code = owari_map[i, ]$sub_code,
                         mun_name = owari_map[i, ]$mun_name,
                         sub_name = owari_map[i, ]$sub_name,
                         gun_code = owari_map[i, ]$gun_code,
                         geometry = sf::st_cast(owari_map[i, ]$geometry, "POLYGON"))

  # Order by size
  new_rows <- new_rows %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Ignore all areas except for the largest areas in each municipality
    dplyr::filter(row_number()==1) %>%
    dplyr::select(-area)

  owari_sep <- rbind(owari_sep, new_rows)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert to sf
owari_largest <- sf::st_as_sf(owari_sep)

# TODO (Optional)
# Add other smaller areas to `pref_largest` as necessary
# By default, `pref_largest` only includes the largest areas in each municipality/gun
# If it is necessary to check the contiguity with other smaller areas,
# add those areas

# Create new data frame
add_small <- setNames(data.frame(matrix(ncol = length(cols), nrow = 0)), cols)

# Municipality codes of the areas to add
add_small_code <- c(23109, 23110, 23111,
                    23420, 23440)
# 名古屋市熱田区, 名古屋市中川区, 名古屋市港区,
# 海部郡, 知多郡
add_small_unit <- owari_sep$unit[owari_sep$code %in% add_small_code | owari_sep$gun_code %in% add_small_code]

# Create data frame
owari_sep_add <- owari_sep

# # To calculate area size, switch off `geometry (s2)`
sf_use_s2(FALSE)

for (i in 1:length(add_small_unit)){
  add_small <-
    data.frame(unit = add_small_unit[i],
               code = owari_map[add_small_unit[i], ]$code,
               sub_code = owari_map[add_small_unit[i], ]$sub_code,
               mun_name = owari_map[add_small_unit[i], ]$mun_name,
               sub_name = owari_map[add_small_unit[i], ]$sub_name,
               gun_code = owari_map[add_small_unit[i], ]$gun_code,
               geometry = sf::st_cast(owari_map[add_small_unit[i], ]$geometry, "POLYGON"))

  # order by size
  add_small <- add_small %>%
    dplyr::mutate(area = sf::st_area(geometry)) %>%
    dplyr::arrange(desc(area)) %>%
    # Add areas that are not the largest polygon within the municipality/gun
    # Note: Change which area to add back to owari_sep depending on needs
    # By default, all the areas that belong to the municipalities in `add_small_code` are added back
    dplyr::filter(row_number()!=1) %>%
    dplyr::select(-area)

  # row bind
  owari_sep_add <- rbind(owari_sep_add, add_small)
}

# switch on `geometry (s2)`
sf_use_s2(TRUE)

# Convert into shapefile
owari_largest <- sf::st_as_sf(owari_sep_add)

# Ignore islands and isolated areas
owari_largest_adj <- redist::redist.adjacency(owari_largest)
mainland_owari <- owari_largest[which(unlist(lapply(owari_largest_adj, length)) > 0), ]

# Make adjacency list for the mainland
mainland_owari_adj <- redist::redist.adjacency(mainland_owari)

# TODO: Repair adjacency list if necessary
# Suggest connection between disconnected groups
# suggest <- geomander::suggest_component_connection(shp = mainland_owari,
#                                                    adj = mainland_owari_adj)
# mainland_owari_adj <- geomander::add_edge(mainland_owari_adj,
#                                                   suggest$x,
#                                                   suggest$y)
# Repair adjacencies
mainland_add_edge <-
  matrix(c(

    # 名古屋市熱田区
    which(mainland_owari$code == 23109)[1],
    which(mainland_owari$code == 23109)[2],

    # 名古屋市中川区
    which(mainland_owari$code == 23110)[1],
    which(mainland_owari$code == 23110)[2],

    # 名古屋市港区gun_code
    which(mainland_owari$code == 23111)[1],
    which(mainland_owari$code == 23111)[2],
    which(mainland_owari$code == 23111)[1],
    which(mainland_owari$code == 23111)[3],
    which(mainland_owari$code == 23111)[1],
    which(mainland_owari$code == 23111)[4],
    which(mainland_owari$code == 23111)[3],
    which(mainland_owari$code == 23111)[6],
    which(mainland_owari$code == 23111)[6],
    which(mainland_owari$code == 23111)[5],
    which(mainland_owari$code == 23111)[5],
    which(mainland_owari$code == 23111)[4],

    # 名古屋市中川区-中区
    which(mainland_owari$code == 23110)[2],
    which(mainland_owari$code == 23106),

    # 名古屋市中村区-中区
    which(mainland_owari$code == 23105),
    which(mainland_owari$code == 23106),

    # 名古屋市熱田区-南区
    which(mainland_owari$code == 23109)[2],
    which(mainland_owari$code == 23112),

    # 名古屋市熱田区-瑞穂区
    which(mainland_owari$code == 23109)[2],
    which(mainland_owari$code == 23108),

    # 名古屋市熱田区-昭和区
    which(mainland_owari$code == 23109)[2],
    which(mainland_owari$code == 23107),

    # 名古屋市港区-東海市
    which(mainland_owari$code == 23111)[4],
    which(mainland_owari$code == 23222),

    # 名古屋市南区-東海市
    which(mainland_owari$code == 23112),
    which(mainland_owari$code == 23222)

  ), ncol = 2, byrow = TRUE)


mainland_owari_adj <- geomander::add_edge(mainland_owari_adj,
                                                  mainland_add_edge[,1],
                                                  mainland_add_edge[,2])

# Check valid results
results_owari_no_multi$valid <- check_contiguous(owari_smc_plans_no_multi,
                                                         mainland_owari,
                                                         mainland_owari_adj)

# Filter out plans with discontiguities
functioning_results_owari <- results_owari_no_multi %>%
  dplyr::filter(valid == TRUE)

# nrow(functioning_results_owari) must be over `nsims_final`.
# If not, increase nsims and run more simulations.

# Sample `nsims_all` plans
set.seed(2020)
valid_sample_owari <- functioning_results_owari %>%
  pull(draw) %>%
  sample(nsims_all, replace = FALSE)

# Sampled plans & reference plan
results_sample_owari <- results_owari_no_multi %>%
  dplyr::filter(draw %in% valid_sample_owari | draw == "lh_2022")

# Sample plans
sim_smc_owari_sample <- sim_smc_owari %>%
  dplyr::filter(draw %in% valid_sample_owari | draw == "lh_2022")

# Check summary statistics: sampled plans
sim_smc_owari %>%
  dplyr::filter(draw %in% valid_sample_owari | draw == "lh_2022") %>%
  partisan_metrics_japan(owari_map) %>%
  dplyr::left_join(results_sample_owari %>%
                     dplyr::select(mun_split,
                                   gun_split,
                                   max_to_min,
                                   draw),
                   by = "draw") %>%
  summary()

# All simulated plans
sim_smc_owari %>%
  partisan_metrics_japan(owari_map) %>%
  dplyr::left_join(results_owari %>%
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
    owari = list(map = owari_map,
                 plans = sim_smc_owari_sample %>%
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
                                             strength = 2,
                                             admin = pref_map_merged$code)

# Simulate the remainder (Mikawa Region) and pull everything together
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
