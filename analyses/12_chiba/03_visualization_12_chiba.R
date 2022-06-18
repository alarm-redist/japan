###############################################################################
# Data visualization for `00_pref`
# © ALARM Project, April 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the codes in the column `pref$code`
# i.e. For rural prefectures, define using the municipality codes, not the gun codes
# i.e. For urban prefectures, define using gun codes if gun was merged
koiki_1_codes <- c(12205, 12234)

##### West #####
# Load data
west_map <- readRDS(paste("data-out/maps/",
                                   as.character(pref_code),
                                   "_",
                                   as.character(pref_name),
                                   "_hr_2020_map_west.rds",
                                   sep = ""))

west <- readRDS(paste("data-out/pref/",
                               as.character(pref_code),
                               "_",
                               as.character(pref_name),
                               "_west.Rds",
                               sep = ""))

westadj <- readRDS(paste("data-out/pref/",
                                  as.character(pref_code),
                                  "_",
                                  as.character(pref_name),
                                  "_adj_west.Rds",
                                  sep = ""))

sim_smc_west <- readRDS(paste("data-out/plans/",
                                       as.character(pref_code),
                                       "_",
                                       as.character(pref_name),
                                       "_",
                                       as.character(sim_type),
                                       "_",
                                       as.character(nsims * 4),
                                       "_west.Rds",
                                       sep = ""), refhook = NULL)

# Get plans matrix
west_smc_plans <- redist::get_plans_matrix(sim_smc_west)

# Calculate max:min ratio
wgt_smc_west <- simulation_weight_disparity_table(sim_smc_west)

# Count number of municipality splits
num_mun_split_west <- count_splits(west_smc_plans, west_map$code)
mun_split_west <- redist::redist.splits(west_smc_plans, west_map$code) %>%
  matrix(ncol = ndists_new_west, byrow = TRUE)
mun_split_west <- mun_split_west[,1]

# Count number of gun splits
gun_split_west <- 0 # There are no 郡 in `west`

# Count number of koiki renkei splits
koiki_split_west <- 0 # There are no 広域連携 areas in `west`

# Compile results
results_west <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_west)))
results_west$max_to_min <- wgt_smc_west$max_to_min
results_west$gun_split <- gun_split_west
results_west$num_mun_split <- num_mun_split_west
results_west$mun_split <- mun_split_west
results_west$multi <-  num_mun_split_west - mun_split_west
results_west$koiki_split <- koiki_split_west
results_west$index <- 1:nrow(wgt_smc_west)

# To-do: filter out plans with multi-splits
functioning_results_west <- filter(results_west, multi == 0)

# Sample 5,000 plans
valid_sample_west <- sample(functioning_results_west$index, 5000, replace = FALSE)
sim_smc_west_sample <- sim_smc_west %>%
  filter(draw %in% valid_sample_west)

# Filter out sampled plans
results_west_sample <- functioning_results_west %>%
  filter(index %in% valid_sample_west)

# Find Optimal Plan
optimal_west <- results_west_sample$index[which(
  results_west_sample$max_to_min ==
    min(results_west_sample$max_to_min))][1]
results_west_sample[which(results_west_sample$index == optimal_west),]

# Gun/Municipality boundaries
mun_boundary_west <- pref_shp_cleaned %>%
  mutate(code = as.numeric(substr(code, 1, 5))) %>%
  filter(code %in% c(13101:13123)) %>% # exclude islands
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun_west <- mun_boundary_west %>% summarise(geometry = sf::st_combine(geometry))
mun_west$type <- "市区町村の境界"
# There are no gun in the special wards area

# Municipality/Gun boundary
boundary_west <- rbind(mun_west)

# District Boundary of Optimal Plan
matrix_optimal_west <- redist::get_plans_matrix(sim_smc_west %>%
                                                           filter(draw == optimal_west))
colnames(matrix_optimal_west) <- "district"
optimal_boundary_west <- cbind(west_map, as_tibble(matrix_optimal_west))

# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num_west <- results_west_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_west_sample$index)*0.1)) %>%
  select(index)
good_num_west <- as.vector(t(good_num_west))
sim_smc_west_good <- sim_smc_west_sample %>%
  filter(draw %in% good_num_west)

# Obtain co-occurrence matrix
m_co_west = redist::prec_cooccurrence(sim_smc_west_good, sampled_only=TRUE)

# Create clusters
cl_co_west = cluster::agnes(m_co_west)
# analyze the dendogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co_west))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue") # explore different depths

prec_clusters_west = cutree(cl_co_west, 36)
west_membership <- as_tibble(as.data.frame(prec_clusters_west))
names(west_membership) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio_west <- vector(length = length(west$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(west$code))
{
  cooc_ratio_west[i] <- 1 -
    sum(west$pop[relcomp(westadj[[i]]+1,
                                  which(prec_clusters_west == prec_clusters_west[i]))] *
          m_co_west[i, relcomp(westadj[[i]]+1,
                                        which(prec_clusters_west == prec_clusters_west[i]))])/
    sum(west$pop[westadj[[i]]+1] * m_co_west[i, westadj[[i]]+1])
}











###### East ##########
east_map <- readRDS(paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "_hr_2020_map_east.rds",
                          sep = ""))

east <- readRDS(paste("data-out/pref/",
                      as.character(pref_code),
                      "_",
                      as.character(pref_name),
                      "_east.Rds",
                      sep = ""))

eastadj <- readRDS(paste("data-out/pref/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         "_adj_east.Rds",
                         sep = ""))

sim_smc_east <- readRDS(paste("data-out/plans/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims * 4),
                              "_east.Rds",
                              sep = ""), refhook = NULL)

# Get plans matrix
east_smc_plans <- redist::get_plans_matrix(sim_smc_east)

# Calculate max:min ratio
wgt_smc_east <- simulation_weight_disparity_table(sim_smc_east)

# Assign koiki_renkei area codes
koiki_1 <- pref$code
koiki_1[koiki_1 %in% koiki_1_codes] <- 1
koiki_1[!koiki_1 %in% 1] <-
  seq(1000, 1000 +length(koiki_1[!koiki_1 %in% c(koiki_1_codes, 1)]) - 1, by = 1)

# Count number of municipality splits
num_mun_split <- count_splits(east_smc_plans, east_map$code)
mun_split <- redist::redist.splits(east_smc_plans, east_map$code) %>%
  matrix(ncol = ndists_new_east, byrow = TRUE)
mun_split <- mun_split[,1]

# Count number of gun splits
gun_index <- east$gun_code
gun_index[gun_index < (east_map$code[1]%/%1000)*1000+300] <-
  seq(100000, 100000 + length(gun_index[gun_index < (east_map$code[1]%/%1000)*1000+300])-1, by = 1)

gun_split <- redist::redist.splits(east_smc_plans, gun_index) %>%
  matrix(ncol = ndists_new_east, byrow = TRUE)
gun_split <- gun_split[,1]
# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(east_smc_plans, koiki_1)
koiki_split <- koiki_split %>%
  matrix(ncol = ndists_new_east, byrow = TRUE)
koiki_split <- koiki_split[,1]
# Compile results
results <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_east)))
results$max_to_min <- wgt_smc_east$max_to_min
results$gun_split <- gun_split
results$num_mun_split <- num_mun_split
results$mun_split <- mun_split - gun_split
results$multi <-  num_mun_split - mun_split
results$koiki_split <- koiki_split
results$index <- 1:nrow(wgt_smc_east)


# Confirm that the gun boundaries that are respected under the enacted plan
# are respected under the simulated plans
# For Chiba, we will assign `gun_exception` here that allows splits.
# This is because we assigned `gun_exception` in `01_prep_12_chiba` uniquely,
# in order to deal with discontiguous 郡
# Code of 郡 that are split under the status quo
gun_exception <- c(12400) # Sanbu
respect_gun_code <- setdiff(unique(gun_index[which(gun_index < 100000)]), gun_exception)

# Evaluate whether the gun that must be kept together in the same district are
# in the same district in the simulated plans
respect_gun_matrix <- matrix(0, nrow = length(respect_gun_code), ncol = ncol(east_smc_plans))
for(i in 1:length(respect_gun_code)){
  for(j in 1:ncol(east_smc_plans)){
    respect_gun_matrix[i, j] <-
      length(east_smc_plans[which(east$code == respect_gun_code[i]),j]) -1 ==
      sum(duplicated(east_smc_plans[which(east$code == respect_gun_code[i]),j]))
  }
}

# Store result
results$respect_gun <- colSums(respect_gun_matrix)

# Filter out plans with multi-splits
# as well as plans that split gun that should have been respected
functioning_results_east <- results %>%
  filter(respect_gun == length(respect_gun_code),
         multi == 0,
         mun_split <= sq_mun_splits)

# Sample 5,000 plans
set.seed(2020)
valid_sample_east <- sample(functioning_results_east$index, 5000, replace = FALSE)
sim_smc_east_sample <- sim_smc_east %>%
  filter(draw %in% valid_sample_east)

# Filter out sampled plans
results_sample <- functioning_results_east %>%
  filter(index %in% valid_sample_east)

# Find Optimal Plan
optimal <- results_sample$index[which(
  results_sample$max_to_min ==
    min(results_sample$max_to_min))][1]
results_sample[which(results_sample$index == optimal),]

# Gun/Municipality boundaries
mun_boundary <- east_shp_cleaned %>%
  mutate(code = as.numeric(substr(code, 1, 5))) %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary <- east %>%
  filter(gun_code >= (east_map$code[1]%/%1000)* 1000 + 300) %>%
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
matrix_optimal <- redist::get_plans_matrix(sim_smc_east %>% filter(draw == optimal))
colnames(matrix_optimal) <- "district"
optimal_boundary <- cbind(east_map, as_tibble(matrix_optimal))

# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num <-  results_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_sample$index)*0.1)) %>%
  select(index)
good_num <- as.vector(t(good_num))
sim_smc_east_good <- sim_smc_east_sample %>%
  filter(draw %in% good_num)

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_east_good, sampled_only=TRUE)

# Compute clustering
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue")

prec_clusters = cutree(cl_co, ndists_new)  # change ndists_new to an appropriate number

east_membership <- as_tibble(as.data.frame(prec_clusters))
names(east_membership) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio <- vector(length = length(east$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(east$code))
{
  cooc_ratio[i] <- 1 -
    sum(east$pop[relcomp(eastadj[[i]]+1,
                         which(prec_clusters == prec_clusters[i]))] * m_co[i, relcomp(eastadj[[i]]+1,
                                                                   which(prec_clusters == prec_clusters[i]))])/
    sum(east$pop[eastadj[[i]]+1] * m_co[i, eastadj[[i]]+1])
}


# Save files
rm(cl_co_west,
   cl_co_east,
   constr_west,
   constr_east,
   m_co_west,
   m_co_east,
   mun_west,
   mun_east,
   gun_east,
   mun_boundary_west,
   mun_boundary_east,
   gun_boundary_east,
   pref_shp_cleaned,
   pref_gun,
   pref_non_gun,
   pref_pop_2020,
   pref_shp_2015,
   pref_mutual,
   pref_pop_only,
   pref_geom_only,
   west_smc_plans,
   east_smc_plans,
   sim_smc_west_good,
   sim_smc_east_good,
   sim_smc_west,
   sim_smc_east,
   wgt_smc_west,
   wgt_smc_east,
   num_mun_split_west,
   num_mun_split_east,
   mun_split_west,
   mun_split_east,
   gun_split_west,
   gun_split_east,
   koiki_split_west,
   koiki_split_east,
   matrix_optimal_west,
   matrix_optimal_east,
   functioning_results_west,
   functioning_results_east,
   results_west,
   results_east,
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
write_rds(sim_smc_west_sample,
          paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_plans_west.rds",
                  sep = ""),
            compress = "xz")
write_rds(sim_smc_east_sample,
          paste("data-out/plans/",
                as.character(pref_code),
                "_",
                as.character(pref_name),
                "_hr_2020_plans_east.rds",
                sep = ""),
          compress = "xz")

# Export `redist_plans` summary statistics to a csv file
as_tibble(sim_smc_west_sample) %>%
    mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
    select("draw", "district", "total_pop") %>%
    write_csv(paste("data-out/plans/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_hr_2020_stats_west.csv",
                    sep = ""))
as_tibble(sim_smc_east_sample) %>%
  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
  select("draw", "district", "total_pop") %>%
  write_csv(paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "_hr_2020_stats_east.csv",
                  sep = ""))
