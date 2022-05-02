###############################################################################
# Data visualization for `13_tokyo`
# © ALARM Project, April 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# There are no koiki-renkei areas in Tokyo

##### Special Wards #####
# Load data
special_wards_map <- readRDS(paste("data-out/maps/",
                                   as.character(pref_code),
                                   "_",
                                   as.character(pref_name),
                                   "_hr_2020_map_special_wards.rds",
                                   sep = ""))

special_wards <- readRDS(paste("data-out/pref/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_special_wards.Rds",
                          sep = ""))

special_wardsadj <- readRDS(paste("data-out/pref/",
                                as.character(pref_code),
                                "_",
                                as.character(pref_name),
                                "_adj_special_wards.Rds",
                                sep = ""))

sim_smc_special_wards <- readRDS(paste("data-out/plans/",
                                     as.character(pref_code),
                                     "_",
                                     as.character(pref_name),
                                     "_",
                                     as.character(sim_type),
                                     "_",
                                     as.character(nsims),
                                     "_special_wards.Rds",
                                     sep = ""))

# Get plans matrix
special_wards_smc_plans <- redist::get_plans_matrix(sim_smc_special_wards)

# Calculate max:min ratio
wgt_smc_special_wards <- simulation_weight_disparity_table(sim_smc_special_wards)

# Count number of municipality splits
num_mun_split_special_wards <- count_splits(special_wards_smc_plans, special_wards_map$code)
mun_split_special_wards <- redist::redist.splits(special_wards_smc_plans, special_wards_map$code)

# Count number of gun splits
gun_split_special_wards <- 0 # There are no 郡 in 東京23区

# Count number of koiki renkei splits
koiki_split_special_wards <- 0 # There are no 広域連携 areas in Tokyo

# Compile results
results_special_wards <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_special_wards)))
results_special_wards$max_to_min <- wgt_smc_special_wards$max_to_min
results_special_wards$gun_split <- gun_split_special_wards
results_special_wards$num_mun_split <- num_mun_split_special_wards
results_special_wards$mun_split <- mun_split_special_wards
results_special_wards$multi <-  num_mun_split_special_wards - mun_split_special_wards
results_special_wards$koiki_split <- koiki_split_special_wards
results_special_wards$index <- 1:nrow(wgt_smc_special_wards)

# To-do: filter out plans with multi-splits
functioning_results_special_wards <- filter(results_special_wards, multi == 0)

# Sample 5,000 plans
valid_sample_special_wards <- sample(functioning_results_special_wards$index, 5000, replace = FALSE)
sim_smc_special_wards_sample <- sim_smc_special_wards %>%
  filter(draw %in% valid_sample_special_wards)

# Filter out sampled plans
results_special_wards_sample <- functioning_results_special_wards %>%
  filter(index %in% valid_sample_special_wards)

# Find Optimal Plan
optimal_special_wards <- results_special_wards_sample$index[which(
    results_special_wards_sample$max_to_min ==
    min(results_special_wards_sample$max_to_min))][1]
results_special_wards_sample[which(results_special_wards_sample$index == optimal_special_wards),]

# Gun/Municipality boundaries
mun_boundary_special_wards <- pref_shp_cleaned %>%
  mutate(code = as.numeric(substr(code, 1, 5))) %>%
  filter(code %in% c(13101:13123)) %>% # exclude islands
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun_special_wards <- mun_boundary_special_wards %>% summarise(geometry = sf::st_combine(geometry))
mun_special_wards$type <- "市区町村の境界"
# There are no gun in the special wards area

# Municipality/Gun boundary
boundary_special_wards <- rbind(mun_special_wards)

# District Boundary of Optimal Plan
matrix_optimal_special_wards <- redist::get_plans_matrix(sim_smc_special_wards %>%
                                                           filter(draw == optimal_special_wards))
colnames(matrix_optimal_special_wards) <- "district"
optimal_boundary_special_wards <- cbind(special_wards_map, as_tibble(matrix_optimal_special_wards))

# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num_special_wards <- results_special_wards_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_special_wards_sample$index)*0.1)) %>%
  select(index)
good_num_special_wards <- as.vector(t(good_num_special_wards))
sim_smc_special_wards_good <- sim_smc_special_wards_sample %>%
  filter(draw %in% good_num_special_wards)

# Obtain co-occurrence matrix
m_co_special_wards = redist::prec_cooccurrence(sim_smc_special_wards_good, sampled_only=TRUE)

# Create clusters
cl_co_special_wards = cluster::agnes(m_co_special_wards)
# analyze the dendogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co_special_wards))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue") # explore different depths

prec_clusters_special_wards = cutree(cl_co_special_wards, 36)
special_wards_membership <- as_tibble(as.data.frame(prec_clusters_special_wards))
names(special_wards_membership) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio_special_wards <- vector(length = length(special_wards$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(special_wards$code))
{
  cooc_ratio_special_wards[i] <- 1 -
    sum(special_wards$pop[relcomp(special_wardsadj[[i]]+1,
                         which(prec_clusters_special_wards == prec_clusters_special_wards[i]))] *
          m_co_special_wards[i, relcomp(special_wardsadj[[i]]+1,
      which(prec_clusters_special_wards == prec_clusters_special_wards[i]))])/
    sum(special_wards$pop[special_wardsadj[[i]]+1] * m_co_special_wards[i, special_wardsadj[[i]]+1])
}


##### Tama #####
# Load data
tama_map <- readRDS(paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_hr_2020_map_tama.rds",
                          sep = ""))

tama <- readRDS(tama, paste("data-out/pref/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_tama.Rds",
                    sep = ""))

tamaadj <- readRDS(paste("data-out/pref/",
                       as.character(pref_code),
                       "_",
                       as.character(pref_name),
                       "_adj_tama.Rds",
                       sep = ""))

sim_smc_tama <- readRDS(paste("data-out/plans/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            "_tama.Rds",
                            sep = ""))

# Get plans matrix
tama_smc_plans <- redist::get_plans_matrix(sim_smc_tama)

# Calculate max:min ratio
wgt_smc_tama <- simulation_weight_disparity_table(sim_smc_tama)

# Count number of municipality splits
num_mun_split_tama <- count_splits(tama_smc_plans, tama_map$code)
mun_split_tama <- redist::redist.splits(tama_smc_plans, tama_map$code)

# Count number of gun splits
gun_index <- tama$gun_code
gun_index[gun_index < (tama_map$code[1]%/%1000)*1000+300] <-
  seq(100000, 100000 + length(gun_index[gun_index < (tama_map$code[1]%/%1000)*1000+300])-1, by = 1)
gun_split_tama <- redist::redist.splits(tama_smc_plans, gun_index)

# Count number of koiki renkei splits
koiki_split_tama <- 0 # There are no 広域連携 areas in Tokyo

# Compile results
results_tama <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_tama)))
results_tama$max_to_min <- wgt_smc_tama$max_to_min
results_tama$gun_split <- gun_split_tama
results_tama$num_mun_split <- num_mun_split_tama
results_tama$mun_split <- mun_split_tama
results_tama$multi <-  num_mun_split_tama - mun_split_tama
results_tama$koiki_split <- koiki_split_tama
results_tama$index <- 1:nrow(wgt_smc_tama)

# Confirm that the gun boundaries that are respected under the enacted plan
# are respected under the simulated plans

# Define the codes of the gun that must be kept together in the same district
respect_gun_code <- setdiff(unique(gun_index[which(gun_index < 100000)]), gun_exception)

# Evaluate whether the gun that must be kept together in the same district are
# in the same district in the simulated plans
respect_gun_matrix <- matrix(0, nrow = length(respect_gun_code), ncol = ncol(tama_smc_plans))
for(i in 1:length(respect_gun_code)){
  for(j in 1:ncol(tama_smc_plans)){
    respect_gun_matrix[i, j] <-
    length(tama_smc_plans[which(tama$code == respect_gun_code[i]),j]) -1 ==
      sum(duplicated(tama_smc_plans[which(tama$code == respect_gun_code[i]),j]))
  }
}

# Store result
results_tama$respect_gun <- colSums(respect_gun_matrix)

# Filter out plans with multi-splits
# as well as plans that split gun that should have been respected
functioning_results_tama <- results_tama %>%
  filter(respect_gun == length(respect_gun_code), multi == 0)

# Sample 5,000 plans
valid_sample_tama <- sample(functioning_results_tama$index, 5000, replace = FALSE)
sim_smc_tama_sample <- sim_smc_tama %>%
  filter(draw %in% valid_sample_tama)

# Filter out sampled plans
results_tama_sample <- functioning_results_tama %>%
  filter(index %in% valid_sample_tama)

# Find Optimal Plan
optimal_tama <- results_tama_sample$index[which(
  results_tama_sample$max_to_min ==
    min(results_tama_sample$max_to_min))][1]
results_tama_sample[which(results_tama_sample$index == optimal_tama),]

# Gun/Municipality boundaries
mun_boundary_tama <-  pref_shp_cleaned %>%
  mutate(code = as.numeric(substr(code, 1, 5))) %>%
  filter(code %in% c(13101:13123) == FALSE) %>%
  filter(code < 13360) %>% #Filter out islands
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary_tama <- tama %>%
  filter(gun_code >= (tama$code[1]%/%1000)* 1000 + 300) %>%
  filter(code  %in% c(13101:13123,
                      13360, 13380, 13400, 13420) == FALSE) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality/gun boundary data
mun_tama <- mun_boundary_tama %>% summarise(geometry = sf::st_combine(geometry))
mun_tama$type <- "市区町村の境界"
gun_tama <- gun_boundary_tama %>% summarise(geometry = sf::st_combine(geometry))
gun_tama$type <- "郡の境界"

# Municipality/Gun boundary
boundary_tama <- rbind(mun_tama, gun_tama)

# District Boundary of Optimal Plan
matrix_optimal_tama <- redist::get_plans_matrix(sim_smc_tama %>%
                                                           filter(draw == optimal_tama))
colnames(matrix_optimal_tama) <- "district"
optimal_boundary_tama<- cbind(tama_map, as_tibble(matrix_optimal_tama))


# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num_tama <- results_tama_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_tama_sample$index)*0.1)) %>%
  select(index)
good_num_tama <- as.vector(t(good_num_tama))
sim_smc_tama_good <- sim_smc_tama_sample %>%
  filter(draw %in% good_num_tama)

# Obtain co-occurrence matrix
m_co_tama = redist::prec_cooccurrence(sim_smc_tama_good, sampled_only=TRUE)

# Create clusters
cl_co_tama = cluster::agnes(m_co_tama)

# Analyze the dendogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co_tama))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue")

prec_clusters_tama = cutree(cl_co_tama, 16)
tama_membership <- as_tibble(as.data.frame(prec_clusters_tama))
names(tama_membership) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio_tama <- vector(length = length(tama$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(tama$code))
{
  cooc_ratio_tama[i] <- 1 -
    sum(tama$pop[relcomp(tamaadj[[i]]+1,
                                  which(prec_clusters_tama == prec_clusters_tama[i]))] *
          m_co_tama[i, relcomp(tamaadj[[i]]+1,
                                        which(prec_clusters_tama == prec_clusters_tama[i]))])/
    sum(tama$pop[tamaadj[[i]]+1] * m_co_tama[i, tamaadj[[i]]+1])
}

###### Save files #####
rm(cl_co_special_wards,
   cl_co_tama,
   constr_special_wards,
   constr_tama,
   m_co_special_wards,
   m_co_tama,
   mun_special_wards,
   mun_tama,
   gun_tama,
   mun_boundary_special_wards,
   mun_boundary_tama,
   gun_boundary_tama,
   pref_shp_cleaned,
   pref_gun,
   pref_non_gun,
   pref_pop_2020,
   pref_shp_2015,
   pref_mutual,
   pref_pop_only,
   pref_geom_only,
   special_wards_smc_plans,
   tama_smc_plans,
   sim_smc_special_wards_good,
   sim_smc_tama_good,
   sim_smc_special_wards,
   sim_smc_tama,
   wgt_smc_special_wards,
   wgt_smc_tama,
   num_mun_split_special_wards,
   num_mun_split_tama,
   mun_split_special_wards,
   mun_split_tama,
   gun_split_special_wards,
   gun_split_tama,
   koiki_split_special_wards,
   koiki_split_tama,
   matrix_optimal_special_wards,
   matrix_optimal_tama,
   functioning_results_special_wards,
   functioning_results_tama,
   results_special_wards,
   results_tama,
   respect_gun_matrix,
   pref_sep
)

# Further remove files specific to Tokyo
rm(sq_pref,
   pref_geom_only_1)

save.image(paste("data-out/pref/",
                 as.character(pref_code),
                 "_",
                 as.character(pref_name),
                 "_data",
                 ".Rdata",
                 sep = ""))

# Save relevant files to upload to Dataverse
# `redist_plans` object
write_rds(sim_smc_special_wards_sample,
          paste("data-out/plans/",
                as.character(pref_code),
                "_",
                as.character(pref_name),
                "_hr_2020_plans_special_wards.rds",
                sep = ""),
          compress = "xz")
write_rds(sim_smc_tama_sample,
          paste("data-out/plans/",
                as.character(pref_code),
                "_",
                as.character(pref_name),
                "_hr_2020_plans_tama.rds",
                sep = ""),
          compress = "xz")

# Export `redist_plans` summary statistics to a csv file
as_tibble(sim_smc_special_wards_sample) %>%
  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
  write_csv(paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_stats_special_wards.csv",
                  sep = ""))
as_tibble(sim_smc_tama_sample) %>%
  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
  write_csv(paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_stats_tama.csv",
                  sep = ""))

