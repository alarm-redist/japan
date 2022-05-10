###############################################################################
# Data visualization for `00_pref`
# © ALARM Project, April 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the municipality codes, not the gun codes
koiki_1_codes <- c()
koiki_2_codes <- c()

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
                                  as.character(nsims),
                                  "_",
                                  as.character(i),
                                  ".Rds",
                                  sep = ""), refhook = NULL)
  assign(paste("sim_smc_pref_", i, sep = ""), sim_smc_pref_n)

  # Get plans matrix
  pref_smc_plans_n <- redist::get_plans_matrix(sim_smc_pref_n)
  assign(paste("pref_smc_plans_", i, sep = ""), pref_smc_plans_n)
}

# Calculate max:min ratio
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1)

# Assign koiki_renkei area codes for simulation with 0 split
koiki_1_0 <- pref_0$code
koiki_1_0[koiki_1_0 %in% koiki_1_codes] <- 1
koiki_2_0 <- pref_0$code
koiki_2_0[koiki_2_0 %in% koiki_2_codes] <- 2

# Assign koiki_renkei area codes for simulation with 1 split
# When a municipality that belongs to a koiki-renkei area is split:
koiki_1_1 <- pref_1$pre_gappei_code
koiki_1_1[koiki_1_1 %in% c(koiki_1_codes,
                           setdiff(pref_1$pre_gappei_code[which(pref_1$code == split_code)], split_code))] <- 1
koiki_2_1 <- pref_1$pre_gappei_code
koiki_2_1[koiki_2_1 %in% koiki_2_codes] <- 2

# Count number of municipality splits
num_mun_split_1 <- count_splits(pref_smc_plans_1, pref_map_1$code)
mun_split_1 <- redist::redist.splits(pref_smc_plans_1, pref_map_1$code)

# Count number of gun splits
gun_split_0 <- redist::redist.splits(pref_smc_plans_0, pref_map_0$gun_code)
gun_split_1 <- redist::redist.splits(pref_smc_plans_1, pref_map_1$gun_code)

# Count number of koiki renkei splits
koiki_split_0 <-
  redist::redist.splits(pref_smc_plans_0, koiki_1_0) +
  redist::redist.splits(pref_smc_plans_0, koiki_2_0)
koiki_split_1 <-
  redist::redist.splits(pref_smc_plans_1, koiki_1_1) +
  redist::redist.splits(pref_smc_plans_1, koiki_2_1)

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

# Add bridges and check if valid
bridges_0 <- c()
results_0$valid <- check_valid(pref_0, pref_smc_plans_0, bridges_0)
bridges_1 <- c()
results_1$valid <- check_valid(pref_1, pref_smc_plans_1, bridges_1)

# TODO: filter out plans with discontiguities
functioning_results_0 <- results_0 %>% dplyr::filter(valid)
functioning_results_1 <- results_1 %>% dplyr::filter(multi == 0 & valid)

# nrow(functioning_results_0) and nrow(functioning_results_1) must be over 5,000.
# If not, increase nsims and run more simulations.

# Sample 5,000 plans
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
optimal_split <- dplyr::inner_join(as.data.frame(pref_1), as.data.frame(optimal_boundary_0),
                                   by = "code")
sim_smc_pref_1_sample <- redist::match_numbers(sim_smc_pref_1_sample,
                                               optimal_split$district,
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
# Filter out plans with top 10% maxmin ratio
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
prec_clusters_0 = cutree(cl_co_0, ndists_new)
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

  # Export `redist_plans` summary statistics to a csv file
  as_tibble(eval(parse(text = paste("sim_smc_pref_",
                                    as.character(i),
                                    "_sample", sep = "")))) %>%
     mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%

     # Remove the column "pop_overlap" that was created when renumbering the district numbers
     select(1:3) %>%

     write_csv(paste("data-out/plans/",
                     as.character(pref_code),
                     "_",
                     as.character(pref_name),
                     "_hr_2020_stats_",
                     as.character(i),
                     ".csv",
                     sep = ""))
}

####-------------- 2. Method for Urban Prefectures-------------------------####
pref_map <- readRDS(paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_hr_2020_map.rds",
                           sep = ""))

prefadj <- readRDS(paste("data-out/pref/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         as.character(nsims),
                         "_adj",
                         ".Rds",
                         sep = ""))

sim_smc_pref <- readRDS(paste("data-out/plans/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              ".Rds",
                              sep = ""), refhook = NULL)

# Get plans matrix
pref_smc_plans <- redist::get_plans_matrix(sim_smc_pref)

# Calculate max:min ratio
wgt_smc <- simulation_weight_disparity_table(sim_smc_pref)

# Assign koiki_renkei area codes
koiki_1 <- pref$code
koiki_1[koiki_1 %in% koiki_1_codes] <- 1
koiki_1[!koiki_1 %in% 1] <-
  seq(1000, 1000 +length(koiki_1[!koiki_1 %in% c(koiki_1_codes, 1)]) - 1, by = 1)
koiki_2 <- pref$code
koiki_2[koiki_2 %in% koiki_2_codes] <- 2
koiki_2[!koiki_2 %in% 2] <-
  seq(1000, 1000 +length(koiki_2[!koiki_2 %in% c(koiki_2_codes, 2)]) - 1, by = 1)

# Count number of municipality splits
num_mun_split <- count_splits(pref_smc_plans, pref_map$code)
mun_split <- redist::redist.splits(pref_smc_plans, pref_map$code)

# Count number of gun splits
gun_index <- pref$gun_code
gun_index[gun_index < (pref_map$code[1]%/%1000)*1000+300] <-
  seq(100000, 100000 + length(gun_index[gun_index < (pref_map$code[1]%/%1000)*1000+300])-1, by = 1)

gun_split <- redist::redist.splits(pref_smc_plans, gun_index)

# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(pref_smc_plans, koiki_1) +
  redist::redist.splits(pref_smc_plans, koiki_2)

# Compile results
results <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results$max_to_min <- wgt_smc$max_to_min
results$gun_split <- gun_split
results$num_mun_split <- num_mun_split
results$mun_split <- mun_split
results$multi <-  num_mun_split - mun_split
results$koiki_split <- koiki_split
results$index <- 1:nrow(wgt_smc)


# Confirm that the gun boundaries that are respected under the enacted plan
# are respected under the simulated plans

# Define the codes of the gun that must be kept together in the same district
respect_gun_code <- setdiff(unique(gun_index[which(gun_index < 100000)]), gun_exception)

# Evaluate whether the gun that must be kept together in the same district are
# in the same district in the simulated plans
respect_gun_matrix <- matrix(0, nrow = length(respect_gun_code), ncol = ncol(pref_smc_plans))
for(i in 1:length(respect_gun_code)){
  for(j in 1:ncol(pref_smc_plans)){
    respect_gun_matrix[i, j] <-
      length(pref_smc_plans[which(pref$code == respect_gun_code[i]),j]) -1 ==
      sum(duplicated(pref_smc_plans[which(pref$code == respect_gun_code[i]),j]))
  }
}

# Store result
results$respect_gun <- colSums(respect_gun_matrix)

# Filter out plans with multi-splits
# as well as plans that split gun that should have been respected
functioning_results <- results %>%
  filter(respect_gun == length(respect_gun_code), multi == 0)

# Sample 5,000 plans
valid_sample_pref <- sample(functioning_results$index, 5000, replace = FALSE)
sim_smc_pref_sample <- sim_smc_pref %>%
  filter(draw %in% valid_sample_pref)

# Filter out sampled plans
results_sample <- functioning_results %>%
  filter(index %in% valid_sample_pref)

# Find Optimal Plan
optimal <- results_sample$index[which(
  results_sample$max_to_min ==
    min(results_sample$max_to_min))][1]
results_sample[which(results_sample$index == optimal),]

# Gun/Municipality boundaries
mun_boundary <- pref_shp_cleaned %>%
  mutate(code = as.numeric(substr(code, 1, 5))) %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary <- pref %>%
  filter(gun_code >= (pref_map$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun <- mun_boundary %>% summarise(geometry = sf::st_combine(geometry))
mun$type <- "市区町村の境界"
# Combine gun boundary data
gun <- gun_boundary %>% summarise(geometry = sf::st_combine(geometry))
gun$type <- "郡の境界"

# Municipality/Gun boundary
boundary <- rbind(mun, gun)

# District Boundary of Optimal Plan
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref %>% filter(draw == optimal))
colnames(matrix_optimal) <- "district"
optimal_boundary <- cbind(pref_map, as_tibble(matrix_optimal))

# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num <-  functioning_results %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(functioning_results$index)*0.1)) %>%
  select(index)
good_num <- as.vector(t(good_num))
sim_smc_pref_good <- sim_smc_pref %>%
  filter(draw %in% good_num)

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)

# Compute clustering
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue")

prec_clusters = cutree(cl_co, ndists_new)  # change ndists_new to an appropriate number

pref_membership <- as_tibble(as.data.frame(prec_clusters))
names(pref_membership) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio <- vector(length = length(pref$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(pref$code))
{
  cooc_ratio[i] <- 1 -
    sum(pref$pop[relcomp(prefadj[[i]]+1,
                         which(prec_clusters == prec_clusters[i]))] * m_co[i, relcomp(prefadj[[i]]+1,
                                                                   which(prec_clusters == prec_clusters[i]))])/
    sum(pref$pop[prefadj[[i]]+1] * m_co[i, prefadj[[i]]+1])
}


# Save files
rm(cl_co,
   constr,
   m_co,
   mun,
   gun,
   mun_boundary,
   gun_boundary,
   pref_shp_cleaned,
   pref_gun,
   pref_non_gun,
   pref_pop_2020,
   pref_shp_2015,
   pref_mutual,
   pref_pop_only,
   pref_geom_only,
   pref_smc_plans,
   sim_smc_pref_good,
   sim_smc_pref,
   wgt_smc,
   num_mun_split,
   mun_split,
   gun_split,
   koiki_split,
   matrix_optimal,
   functioning_results,
   results,
   respect_gun_matrix,
   pref_sep
)

save.image(paste("data-out/pref/",
                 as.character(pref_code),
                 "_",
                 as.character(pref_name),
                 "_data",
                 ".Rdata",
                 sep = ""))

# Save relevant files to upload to Dataverse
# `redist_plans` object
write_rds(sim_smc_pref_sample,
          paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_plans.rds",
                  sep = ""),
            compress = "xz")

# Export `redist_plans` summary statistics to a csv file
as_tibble(sim_smc_pref_sample) %>%
    mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
    write_csv(paste("data-out/plans/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_hr_2020_stats.csv",
                    sep = ""))

