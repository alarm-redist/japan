###############################################################################
# Data visualization for `09_tochigi`
# © ALARM Project, June 2022
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the codes in the column `pref$code`
# i.e. For rural prefectures, define using the municipality codes, not the gun codes
# i.e. For urban prefectures, define using gun codes if gun was merged
koiki_1_codes <- c(09208, 09216, 09364)
koiki_2_codes <- c(09210, 09213, 09407, 09411)

####-------------- 1. Method for Rural Prefectures-------------------------####
# Load data
for (i in 0:1)
{
  pref_map_n <- readRDS(paste("data-out/maps/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_hr_2020_map_",
                              as.character(i),
                              ".rds",
                              sep = ""))
  assign(paste("pref_map_", i, sep = ""), pref_map_n)

  pref_n <- readRDS(paste("data-out/pref/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_",
                          as.character(i),
                          ".Rds",
                          sep = ""))
  assign(paste("pref_", i, sep = ""), pref_n)

  prefadj_n <-readRDS(paste("data-out/pref/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_adj_",
                            as.character(i),
                            ".Rds",
                            sep = ""))
  assign(paste("prefadj_", i, sep = ""), prefadj_n)

  sim_smc_pref_n <- readRDS(paste("data-out/plans/",
                                  as.character(pref_code),
                                  "_",
                                  as.character(pref_name),
                                  "_",
                                  as.character(sim_type),
                                  "_",
                                  as.character(nsims * 4),
                                  "_",
                                  as.character(i),
                                  ".Rds",
                                  sep = ""), refhook = NULL)
  assign(paste("sim_smc_pref_", i, sep = ""), sim_smc_pref_n)

  # Get plans matrix
  pref_smc_plans_n <- redist::get_plans_matrix(sim_smc_pref_n)
  assign(paste("pref_smc_plans_", i, sep = ""), pref_smc_plans_n)
}

# Add 宇都宮市 back to result of simulation with 0 split
sim_smc_pref_0_with_utsunomiya <- NULL
for(i in 1:as.integer(nsims*4)){
  with_utsunomiya <-
    dplyr::bind_rows(as_tibble(sim_smc_pref_0 %>% filter(draw == i)),
                     data.frame(draw = as.factor(i),
                                district = as.integer(3),
                                total_pop = pref$pop[which(pref$code == 09201)]))
  sim_smc_pref_0_with_utsunomiya <- rbind(sim_smc_pref_0_with_utsunomiya, with_utsunomiya)
}

# Add 旧宇都宮市 back to result of simulation with 1 split
sim_smc_pref_1_with_utsunomiya <- NULL
for(i in 1:as.integer(nsims*4)){
  with_utsunomiya <-
    dplyr::bind_rows(as_tibble(sim_smc_pref_1 %>% filter(draw == i)),
                     data.frame(draw = as.factor(i),
                                district = as.integer(3),
                                total_pop = pref_1_with_utsunomiya$pop[which(pref_1_with_utsunomiya$pre_gappei_code == 09201)]))
  sim_smc_pref_1_with_utsunomiya <- rbind(sim_smc_pref_1_with_utsunomiya, with_utsunomiya)
}

# Calculate max:min ratio
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0_with_utsunomiya)
wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1_with_utsunomiya)

# Assign koiki_renkei area codes for simulation with 0 split
koiki_1_0 <- pref_0$code
koiki_1_0[koiki_1_0 %in% koiki_1_codes] <- 1
koiki_2_0 <- pref_0$code
koiki_2_0[koiki_2_0 %in% koiki_2_codes] <- 2

# Assign koiki_renkei area codes for simulation with 1 split
koiki_1_1 <- pref_1$pre_gappei_code
koiki_1_1[koiki_1_1 %in% koiki_1_codes] <- 1
koiki_2_1 <- pref_1$pre_gappei_code
koiki_2_1[koiki_2_1 %in% koiki_2_codes] <- 2

# Count number of municipality splits
# This is by nature 1 for Tochigi, where Utsunomiya-shi must be split.
num_mun_split_1 <- 1
mun_split_1 <- 1

# Count number of gun splits
gun_split_0 <- redist::redist.splits(pref_smc_plans_0, pref_map_0$gun_code) %>%
  matrix(ncol = ndists_new - 1, byrow = TRUE)
gun_split_0 <- gun_split_0[,1]
gun_split_1 <- redist::redist.splits(pref_smc_plans_1, pref_map_1$gun_code) %>%
  matrix(ncol = ndists_new - 1, byrow = TRUE)
gun_split_1 <- gun_split_1[,1]

# Count number of koiki renkei splits
koiki_split_0 <-
  1 + #We know that koiki_1 is split because Utsunomiya-shi is set aside.
  redist::redist.splits(pref_smc_plans_0, koiki_2_0)
koiki_split_0 <- koiki_split_0 %>%
  matrix(ncol = ndists_new - 1, byrow = TRUE)
koiki_split_0 <- koiki_split_0[,1]

koiki_split_1 <-
  1 + #We know that koiki_1 is split because Kyu-Utsunomiya-shi is set aside.
  redist::redist.splits(pref_smc_plans_1, koiki_2_1)
koiki_split_1 <- koiki_split_1 %>%
  matrix(ncol = ndists_new - 1, byrow = TRUE)
koiki_split_1 <- koiki_split_1[,1]

# Compile results: 0 split
results_0 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_0)))
results_0$max_to_min <- wgt_smc_0$max_to_min
results_0$gun_split <- gun_split_0
results_0$koiki_split <- koiki_split_0
results_0$index <- 1:nrow(wgt_smc_0)

# Compile results: 1 split
results_1 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_1)))
results_1$max_to_min <- wgt_smc_1$max_to_min
results_1$num_mun_split <- num_mun_split_1
results_1$mun_split <- mun_split_1
results_1$multi <-  num_mun_split_1 - mun_split_1
results_1$gun_split <- gun_split_1
results_1$koiki_split <- koiki_split_1
results_1$index <- 1:nrow(wgt_smc_1)

# Filter out discontiguous plans
# 佐野市仙波町(09204) is made up of discontiguous parts.
# However, in practice, 佐野市仙波町 is treated as a contiguous unit under the enacted plan.
# Thus, for the purpose of checking whether plans are contiguous or not,
# we remove 佐野市 as well as its neighboring municipality, 足利市(09202),  from the analysis.

# Filter out discontiguous area in 佐野市(09204)
discont_geom <- data.frame(unit = 1,
                           geometry = sf::st_cast(pref_0[which(pref_0$code == 9204),]$geometry,
                                                  "POLYGON"))
discont_geom <- sf::st_as_sf(discont_geom[2,])

# Edit pref_0 object by removing discontiguous area in 佐野市 (i.e. 佐野市仙波町)
pref_0_without_discont <- pref_0
pref_0_without_discont[which(pref_0_without_discont$code == 9204),]$geometry <- discont_geom$geometry
# Remove 足利市(09202) from the analysis
pref_0_without_discont <- pref_0_without_discont %>% filter(code %in% 9202 == FALSE)
pref_smc_plans_0_without_discont <- pref_smc_plans_0[-which(pref_0$code == 9202),]

# Edit pref_1 object by removing discontiguous area in 佐野市 (i.e. 佐野市仙波町)
pref_1_without_discont <- pref_1
pref_1_without_discont[which(pref_1_without_discont$code == 9204),]$geometry <- discont_geom$geometry
# Remove 足利市(09202) from the analysis
pref_1_without_discont <- pref_1_without_discont %>% filter(code %in% 9202 == FALSE)
pref_smc_plans_1_without_discont <- pref_smc_plans_1[-which(pref_1$code == 9202),]

# Add bridges and check if valid
bridges_0 <- c()
results_0$valid <- check_valid(pref_0_without_discont, pref_smc_plans_0_without_discont, bridges_0)
bridges_1 <- c()
results_1$valid <- check_valid(pref_1_without_discont, pref_smc_plans_1_without_discont, bridges_1)

# TODO: filter out plans with discontiguities
functioning_results_0 <- results_0 %>% dplyr::filter(valid)
functioning_results_1 <- results_1 %>% dplyr::filter(multi == 0 & valid)

# nrow(functioning_results_0) and nrow(functioning_results_1) must be over 5,000.
# If not, increase nsims and run more simulations.

# Sample 5,000 plans
set.seed(2020)
valid_sample_0 <- sample(functioning_results_0$index, 5000, replace = FALSE)
sim_smc_pref_0_sample <- sim_smc_pref_0 %>%
  filter(draw %in% valid_sample_0)

valid_sample_1 <- sample(functioning_results_1$index, 5000, replace = FALSE)
sim_smc_pref_1_sample <- sim_smc_pref_1 %>%
  filter(draw %in% valid_sample_1)

# Filter out sampled plans
results_0_sample <- functioning_results_0 %>%
  filter(index %in% valid_sample_0)
results_1_sample <- functioning_results_1 %>%
  filter(index %in% valid_sample_1)

# Find Optimal Plan
optimal_0 <- results_0_sample$index[which(results_0_sample$max_to_min ==
                                            min(results_0_sample$max_to_min))][1]
results_0_sample[which(results_0_sample$index == optimal_0),]
optimal_1 <- results_1_sample$index[which(results_1_sample$max_to_min ==
                                            min(results_1_sample$max_to_min))][1]
results_1_sample[which(results_1_sample$index == optimal_1),]

# Optimal Plan: 0 split
matrix_optimal_0 <- redist::get_plans_matrix(sim_smc_pref_0 %>% filter(draw == optimal_0))
colnames(matrix_optimal_0) <- "district"
optimal_boundary_0 <- cbind(pref_map_0, as_tibble(matrix_optimal_0))

# Match district numbers
optimal_split <- dplyr::inner_join(as.data.frame(pref_1),
                                   as.data.frame(optimal_boundary_0),
                                   by = "code")
sim_smc_pref_1_sample <- redist::match_numbers(sim_smc_pref_1_sample,
                                               # Add 旧北条市および旧中島町
                                               c(1, 1, optimal_split$district),
                                               col = "pop_overlap")

# Gun/Municipality/Koiki-renkei boundaries
mun_boundary <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary <- pref %>%
  filter(gun_code >= (pref_map_0$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Optimal Plan: 1 split
matrix_optimal_1 <- redist::get_plans_matrix(sim_smc_pref_1 %>% filter(draw == optimal_1))
colnames(matrix_optimal_1) <- "district"
optimal_boundary_1 <- cbind(pref_map_1, as_tibble(matrix_optimal_1))

# Combine municipality boundary data
mun <- mun_boundary %>% summarise(geometry = sf::st_combine(geometry))
mun$type <- "市区町村の境界"
# Combine gun boundary data
gun <- gun_boundary %>% summarise(geometry = sf::st_combine(geometry))
gun$type <- "郡の境界"

# Boundary for plot with 0 split
boundary_0 <- rbind(mun, gun)

# Boundary for split municipality
old_boundary <- optimal_boundary_1 %>%
  filter(code == split_code) %>%
  summarise(geometry = sf::st_combine(geometry))
old_boundary$type <- "合併前の市町村の境界"

# Match CRS
old_boundary <- sf::st_transform(old_boundary, crs = sf::st_crs(4612))

# Boundary for plot with 1 split
boundary_1 <- rbind(old_boundary, mun, gun)

# Co-occurrence
# Filter out plans with top 10% koiki-renkei areas
good_num_0 <- results_0_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_0_sample$index)*0.1)) %>%
  select(index)
good_num_0 <- as.vector(t(good_num_0))
sim_smc_pref_0_good <- sim_smc_pref_0 %>%
  filter(draw %in% good_num_0)

# Obtain co-occurrence matrix
m_co_0 = redist::prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

# Create clusters
cl_co_0 = cluster::agnes(m_co_0)
# Since we set aside 松山市 when running the simulations, the simulations yield redistricting plans
# with ndists_new-1 districts. Thus, we create ndists_new-1 clusters.
prec_clusters_0 = cutree(cl_co_0, ndists_new-1)
pref_membership_0 <- as_tibble(as.data.frame(prec_clusters_0))
names(pref_membership_0) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio <- vector(length = length(pref_0$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(pref_0$code))
{
  cooc_ratio[i] <- 1 -
    sum(pref_0$pop[relcomp(prefadj_0[[i]]+1,
                           which(prec_clusters_0 == prec_clusters_0[i]))] * m_co_0[i, relcomp(prefadj_0[[i]]+1,
                                                                                              which(prec_clusters_0 == prec_clusters_0[i]))])/
    sum(pref_0$pop[prefadj_0[[i]]+1] * m_co_0[i, prefadj_0[[i]]+1])
}

# Save files
rm(pref_smc_plans_0,
   pref_smc_plans_1,
   pref_smc_plans_n,
   sim_smc_pref_n,
   sim_smc_pref_0,
   sim_smc_pref_1,
   sim_smc_pref_0_good,
   wgt_smc_0,
   wgt_smc_1,
   num_mun_split_1,
   mun_split_1,
   gun_split_0,
   gun_split_1,
   koiki_split_0,
   koiki_split_1,
   matrix_optimal_0,
   matrix_optimal_1,
   census_mun_old_2020,
   geom,
   pop,
   pref_pop_2020,
   pref_shp_2015,
   pref_shp_cleaned,
   old_mun,
   functioning_results_0,
   functioning_results_1,
   results_0,
   results_1
)

save.image(paste("data-out/pref/",
                 as.character(pref_code),
                 "_",
                 as.character(pref_name),
                 "_data",
                 ".Rdata",
                 sep = ""))

# Save relevant files to upload to Dataverse
for (i in 0:1){
  # `redist_plans` object
  write_rds(paste("sim_smc_pref_", as.character(i), "_sample", sep = ""),
            paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_plans_",
                  as.character(i),
                  ".rds",
                  sep = ""),
            compress = "xz")
}


# Export `redist_plans` summary statistics to a csv file
# Merge with Utsunomiya
sim_smc_pref_0_with_utsunomiya %>%
  filter(draw %in% valid_sample_0) %>%

  # Remove the column "pop_overlap" that was created when renumbering the district numbers
  select("draw", "district", "total_pop") %>%

  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%

  write_csv(paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_stats_",
                  as.character(0),
                  ".csv",
                  sep = ""))

# Merge with utsunomiya
sim_smc_pref_1_with_utsunomiya %>%
  filter(draw %in% valid_sample_1) %>%

  # Remove the column "pop_overlap" that was created when renumbering the district numbers
  select("draw", "district", "total_pop") %>%

  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%

  write_csv(paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_stats_",
                  as.character(1),
                  ".csv",
                  sep = ""))
