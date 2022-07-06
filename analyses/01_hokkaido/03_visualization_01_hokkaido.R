###############################################################################
# Data visualization for `01_hokkaido`
# © ALARM Project, June 2022
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the codes in the column `pref$code`
# i.e. For rural prefectures, define using the municipality codes, not the gun codes
# i.e. For urban prefectures, define using gun codes if gun was merged

# さっぽろ連携中枢都市圈 is split in every simulation.

# 北しべりし定住自立圈, 上川中部定住自立圈, 西いぶり定住自立圈, 釧路定住自立圈,
# 北見地域定住自立圈, 十勝定住自立圈, 網走市大空町定住自立圈, 東胆振定住自立圈,
# 宗谷定住自立圈, 中空知定住自立圈, 北空知定住自立港, and 富良野地区定住自立圈
# are not split, because it is within the boundaries of the corresponding 振興局.

# 南北海道定住自立圈
koiki_1_codes <- c(01202,
                   01236,
                   01331,
                   01332,
                   01333,
                   01334,
                   01337,
                   01343,
                   01345,
                   01346,
                   01347,
                   01361,
                   01362,
                   01363,
                   01364,
                   01367,
                   01370,
                   01371)
# 北・北海道中央圈域定住自立圈
koiki_2_codes <- c(01220,
                   01221,
                   01464,
                   01465,
                   01468,
                   01469,
                   01470,
                   01471,
                   01472,
                   01512,
                   01513,
                   01514,
                   01562)

####-------------- 2. Method for Urban Prefectures-------------------------####
####-------------- Non-Ishikari -------------------------####
non_ishikari_map <- readRDS(paste("data-out/maps/",
                          as.character(pref_code),
                          "_",
                          as.character(pref_name),
                          "hr_2020_map_non_ishikari.rds",
                           sep = ""))

non_ishikariadj <- readRDS(paste("data-out/pref/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         "adj_non_ishikari.Rds",
                         sep = ""))

sim_smc_non_ishikari <- readRDS(paste("data-out/plans/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims * 4),
                              "_non_ishikari.Rds",
                              sep = ""), refhook = NULL)

# Get plans matrix
non_ishikari_smc_plans <- redist::get_plans_matrix(sim_smc_non_ishikari)

# Calculate max:min ratio
wgt_smc <- simulation_weight_disparity_table(sim_smc_non_ishikari)

# Assign koiki_renkei area codes
koiki_1 <- non_ishikari$code
koiki_1[koiki_1 %in% koiki_1_codes] <- 1
koiki_2 <- non_ishikari$code
koiki_2[koiki_2 %in% koiki_2_codes] <- 2


# Count number of municipality splits
num_mun_split <- count_splits(non_ishikari_smc_plans, non_ishikari_map$code)
mun_split <- redist::redist.splits(non_ishikari_smc_plans, non_ishikari_map$code) %>%
  matrix(ncol = ndists_non_ishikari, byrow = TRUE)
mun_split <- mun_split[,1]

# Count number of gun splits
gun_split <- redist::redist.splits(non_ishikari_smc_plans, non_ishikari_map$gun_code) %>%
  matrix(ncol = ndists_non_ishikari, byrow = TRUE)
gun_split <- gun_split[,1]

# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(non_ishikari_smc_plans, koiki_1) +
  redist::redist.splits(non_ishikari_smc_plans, koiki_2)
koiki_split <- koiki_split %>%
  matrix(ncol = ndists_new, byrow = TRUE)
koiki_split <- koiki_split[,1]

# Compile results
results_non_ishikari <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results_non_ishikari$max_to_min <- wgt_smc$max_to_min
results_non_ishikari$gun_split <- gun_split
results_non_ishikari$num_mun_split <- num_mun_split
results_non_ishikari$mun_split <- mun_split
results_non_ishikari$multi <-  num_mun_split - mun_split
results_non_ishikari$koiki_split <- koiki_split
results_non_ishikari$index <- 1:nrow(wgt_smc)

# Confirm that the 郡 that are kept together in the same district under the enacted plan
# are not split in the simulated plans

# Add bridges and check if valid
bridges <- c()
results_non_ishikari$valid <- check_valid(non_ishikari, non_ishikari_smc_plans, bridges)

# Discard plans with multi-splits as well as plans that split 郡/municipalities that
# should not be split
functioning_results_non_ishikari <- results_non_ishikari %>%
  filter(valid & multi == 0)

# Sample 5,000 plans
set.seed(2020)
valid_sample_non_ishikari <- sample(functioning_results_non_ishikari$index, 5000, replace = FALSE)
sim_smc_non_ishikari_sample <- sim_smc_non_ishikari %>%
  filter(draw %in% valid_sample_non_ishikari)

# Filter out sampled plans
results_sample_non_ishikari <- functioning_results_non_ishikari %>%
  filter(index %in% valid_sample_non_ishikari)

# Find Optimal Plan
optimal_non_ishikari <- results_sample_non_ishikari$index[which(
  results_sample_non_ishikari$max_to_min ==
    min(results_sample_non_ishikari$max_to_min))][1]
results_sample_non_ishikari[which(results_sample_non_ishikari$index == optimal_non_ishikari),]

# Gun/Municipality boundaries
mun_boundary_non_ishikari <- pref %>%
  mutate(code = as.numeric(substr(code, 1, 5))) %>%
  filter(code %in% c(01101,
                     01102,
                     01103,
                     01104,
                     01105,
                     01106,
                     01107,
                     01108,
                     01109,
                     01110,
                     01217,
                     01224,
                     01231,
                     01234,
                     01235,
                     01303,
                     01304) == FALSE) %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary_non_ishikari <- pref %>%
  filter(code %in% c(01101,
                     01102,
                     01103,
                     01104,
                     01105,
                     01106,
                     01107,
                     01108,
                     01109,
                     01110,
                     01217,
                     01224,
                     01231,
                     01234,
                     01235,
                     01303,
                     01304) == FALSE) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun_non_ishikari <- mun_boundary_non_ishikari %>%
  summarise(geometry = sf::st_combine(geometry))
mun_non_ishikari$type <- "市区町村の境界"
# Combine gun boundary data
gun_non_ishikari <- gun_boundary_non_ishikari %>%
  summarise(geometry = sf::st_combine(geometry))
gun_non_ishikari$type <- "振興局の境界"

# Municipality/Gun boundary
boundary_non_ishikari <- rbind(mun_non_ishikari, gun_non_ishikari)

# District boundary of optimal plan
matrix_optimal_non_ishikari <- redist::get_plans_matrix(sim_smc_non_ishikari %>%
                                                          filter(draw == optimal_non_ishikari))
colnames(matrix_optimal_non_ishikari) <- "district"
optimal_boundary_non_ishikari <- cbind(non_ishikari_map, as_tibble(matrix_optimal_non_ishikari))

# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num_non_ishikari <- results_sample_non_ishikari %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_sample_non_ishikari$index)*0.1)) %>%
  select(index)
good_num_non_ishikari <- as.vector(t(good_num_non_ishikari))
sim_smc_non_ishikari_good <- sim_smc_non_ishikari %>%
  filter(draw %in% good_num_non_ishikari)

# Obtain co-occurrence matrix
m_co_0 = redist::prec_cooccurrence(sim_smc_non_ishikari_good, sampled_only=TRUE)

# Create clusters
cl_co_0 = cluster::agnes(m_co_0)
prec_clusters_non_ishikari = cutree(cl_co_0, ndists_non_ishikari)
pref_membership_non_ishikari <- as_tibble(as.data.frame(prec_clusters_non_ishikari))
names(pref_membership_non_ishikari) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio <- vector(length = length(non_ishikari$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(non_ishikari$code))
{
  cooc_ratio[i] <- 1 -
    sum(non_ishikari$pop[relcomp(non_ishikariadj[[i]]+1,
                           which(prec_clusters_non_ishikari == prec_clusters_non_ishikari[i]))] * m_co_0[i, relcomp(non_ishikariadj[[i]]+1,
                                                                                              which(prec_clusters_non_ishikari == prec_clusters_non_ishikari[i]))])/
    sum(non_ishikari$pop[non_ishikariadj[[i]]+1] * m_co_0[i, non_ishikariadj[[i]]+1])
}

####-------------- Ishikari Shinko-kyoku -------------------------####
ishikari_map <- readRDS(paste("data-out/maps/",
                                  as.character(pref_code),
                                  "_",
                                  as.character(pref_name),
                                  "hr_2020_map_ishikari.rds",
                                  sep = ""))

ishikariadj <- readRDS(paste("data-out/pref/",
                                 as.character(pref_code),
                                 "_",
                                 as.character(pref_name),
                                 "adj_ishiraki.Rds",
                                 sep = ""))

sim_smc_ishikari <- readRDS(paste("data-out/plans/",
                                      as.character(pref_code),
                                      "_",
                                      as.character(pref_name),
                                      "_",
                                      as.character(sim_type),
                                      "_",
                                      as.character(nsims * 4),
                                      "_ishikari.Rds",
                                      sep = ""), refhook = NULL)

# Get plans matrix
ishikari_smc_plans <- redist::get_plans_matrix(sim_smc_ishikari)

# Calculate max:min ratio
wgt_smc <- simulation_weight_disparity_table(sim_smc_ishikari)

# Count number of municipality splits
num_mun_split <- count_splits(ishikari_smc_plans, ishikari_map$code)
mun_split <- redist::redist.splits(ishikari_smc_plans, ishikari_map$code) %>%
  matrix(ncol = ndists_ishikari, byrow = TRUE)
mun_split <- mun_split[,1]

# Count number of gun splits
gun_index <- ishikari$gun_code
gun_index[gun_index < (ishikari_map$code[1]%/%1000)*1000+300] <-
  seq(100000, 100000 + length(gun_index[gun_index < (ishikari_map$code[1]%/%1000)*1000+300])-1, by = 1)

gun_split <- redist::redist.splits(ishikari_smc_plans, gun_index) %>%
  matrix(ncol = ndists_ishikari, byrow = TRUE)
gun_split <- gun_split[,1]

# Count number of koiki renkei splits
# さっぽろ連携中枢都市圈 is split in every simulation.
koiki_split <- 1

# Compile results
results_ishikari <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results_ishikari$max_to_min <- wgt_smc$max_to_min
results_ishikari$gun_split <- gun_split
results_ishikari$num_mun_split <- num_mun_split
results_ishikari$mun_split <- mun_split
results_ishikari$multi <-  num_mun_split - mun_split
results_ishikari$koiki_split <- koiki_split
results_ishikari$index <- 1:nrow(wgt_smc)

# Confirm that the 郡 that are kept together in the same district under the enacted plan
# are not split in the simulated plans
# For Hokkaido, no 郡 should be split.

# Confirm that the municipalities that are not split under the enacted plan
# are not split in the simulated plans

# Define the codes of the municipalities that must not be split
respect_mun_code <- setdiff(unique(ishikari$code), mun_not_freeze)

# Evaluate whether the municipalities that must be not be split are split in the simulated plans
respect_mun_matrix <- matrix(0, nrow = length(respect_mun_code), ncol = ncol(ishikari_smc_plans))
for(i in 1:length(respect_mun_code)){
  for(j in 1:ncol(ishikari_smc_plans)){
    respect_mun_matrix[i, j] <-
      length(ishikari_smc_plans[which(ishikari$code == respect_mun_code[i]),j]) -1 ==
      sum(duplicated(ishikari_smc_plans[which(ishikari$code == respect_mun_code[i]),j]))
  }
}

# Store results
results_ishikari$respect_mun <- colSums(respect_mun_matrix) == length(respect_mun_code)

# Discard plans with multi-splits as well as plans that split 郡/municipalities that
# should not be split
functioning_results_ishikari <- results_ishikari %>%
  filter(respect_mun == TRUE, gun_split == 0, multi == 0)

# Sample 5,000 plans
set.seed(2020)
valid_sample_ishikari <- sample(functioning_results_ishikari$index, 5000, replace = FALSE)
sim_smc_ishikari_sample <- sim_smc_ishikari %>%
  filter(draw %in% valid_sample_ishikari)

# Filter out sampled plans
results_sample_ishikari <- functioning_results_ishikari %>%
  filter(index %in% valid_sample_ishikari)

# Find Optimal Plan
optimal_ishikari <- results_sample_ishikari$index[which(
  results_sample_ishikari$max_to_min ==
    min(results_sample_ishikari$max_to_min))][1]
results_sample_ishikari[which(results_sample_ishikari$index == optimal_ishikari),]

# Gun/Municipality boundaries
mun_boundary_ishikari <- pref %>%
  mutate(code = as.numeric(substr(code, 1, 5))) %>%
  filter(code %in% c(01101,
                     01102,
                     01103,
                     01104,
                     01105,
                     01106,
                     01107,
                     01108,
                     01109,
                     01110,
                     01217,
                     01224,
                     01231,
                     01234,
                     01235,
                     01303,
                     01304)) %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary_ishikari <- pref %>%
  filter(code %in% c(01101,
                     01102,
                     01103,
                     01104,
                     01105,
                     01106,
                     01107,
                     01108,
                     01109,
                     01110,
                     01217,
                     01224,
                     01231,
                     01234,
                     01235,
                     01303,
                     01304)) %>%
  mutate(gun_code = if_else(
    code %in% c(1303, 1304),
    01300,
    code)) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun_ishikari <- mun_boundary_ishikari %>%
  summarise(geometry = sf::st_combine(geometry))
mun_ishikari$type <- "市区町村の境界"
# Combine gun boundary data
gun_ishikari <- gun_boundary_ishikari %>%
  summarise(geometry = sf::st_combine(geometry))
gun_ishikari$type <- "郡の境界"

# Municipality/Gun boundary
boundary_ishikari <- rbind(mun_ishikari, gun_ishikari)

# District boundary of optimal plan
matrix_optimal_ishikari <- redist::get_plans_matrix(sim_smc_ishikari %>%
                                                          filter(draw == optimal_ishikari))
colnames(matrix_optimal_ishikari) <- "district"
optimal_boundary_ishikari <- cbind(ishikari_map, as_tibble(matrix_optimal_ishikari))

# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num_ishikari <-  results_sample_ishikari %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_sample_ishikari$index)*0.1)) %>%
  select(index)
good_num_ishikari <- as.vector(t(good_num_ishikari))
sim_smc_ishikari_good <- sim_smc_ishikari_sample %>%
  filter(draw %in% good_num_ishikari)

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_ishikari_good, sampled_only=TRUE)

# Compute clustering
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co))
abline(h = 2, col = "red") # explore different depths
abline(h = 3, col = "blue")

prec_clusters_ishikari = cutree(cl_co, ndists_ishikari)  # change ndists_new to an appropriate number

ishikari_membership <- as_tibble(as.data.frame(prec_clusters_ishikari))
names(ishikari_membership) <- "membership"

# Obtain co-occurrenc ratio
cooc_ratio_ishikari <- vector(length = length(ishikari$code))
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

for (i in 1:length(ishikari$code))
{
  cooc_ratio_ishikari[i] <- 1 -
    sum(ishikari$pop[relcomp(ishikariadj[[i]]+1,
                                 which(prec_clusters_ishikari == prec_clusters_ishikari[i]))] * m_co[i, relcomp(ishikariadj[[i]]+1,
                                                                                                                        which(prec_clusters_ishikari == prec_clusters_ishikari[i]))])/
    sum(ishikari$pop[ishikariadj[[i]]+1] * m_co[i, ishikariadj[[i]]+1])
}

# Save files
rm(cl_co,
   cl_co_0,
   constr,
   m_co,
   m_co_0,
   mun_ishikari,
   mun_non_ishikari,
   gun_ishikari,
   gun_non_ishikari,
   mun_boundary_ishikari,
   mun_boundary_non_ishikari,
   gun_boundary_ishikari,
   gun_boundary_non_ishikari,
   pref_shp_cleaned,
   ishikari_gun,
   ishikari_non_gun,
   pref_pop_2020,
   pref_shp_2015,
   pref_mutual,
   pref_pop_only,
   pref_geom_only,
   ishikari_smc_plans,
   non_ishikari_smc_plans,
   sim_smc_ishikari_good,
   sim_smc_non_ishikari_good,
   sim_smc_ishikari,
   sim_smc_non_ishikari,
   wgt_smc,
   num_mun_split,
   mun_split,
   gun_split,
   koiki_split,
   matrix_optimal_ishikari,
   matrix_optimal_non_ishikari,
   functioning_results_ishikari,
   functioning_results_non_ishikari,
   results_ishikari,
   results_non_ishikari,
   respect_mun_matrix,
   pref_sep,
   pref_freeze
)

save.image(paste("data-out/pref/",
                 as.character(pref_code),
                 "_",
                 as.character(pref_name),
                 "_data",
                 ".Rdata",
                 sep = ""),
           compress = "xz")

# Save relevant files to upload to Dataverse
# `redist_plans` object
# Ishikari Sinko-kyoku
write_rds(sim_smc_ishikari_sample,
          paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "hr_2020_plans_ishikari.rds",
                  sep = ""),
            compress = "xz")

# Non-Ishikari
write_rds(sim_smc_non_ishikari_sample,
          paste("data-out/plans/",
                as.character(pref_code),
                "_",
                as.character(pref_name),
                "hr_2020_plans_non_ishikari.rds",
                sep = ""),
          compress = "xz")


# Export `redist_plans` summary statistics to a csv file
# Ishikari Sinko-kyoku
as_tibble(sim_smc_ishikari_sample) %>%
    mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
    select("draw", "district", "total_pop") %>%
    write_csv(paste("data-out/plans/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "hr_2020_stats_ishikari.csv",
                    sep = ""))

# Non-Ishikari
as_tibble(sim_smc_non_ishikari_sample) %>%
  mutate(across(where(is.numeric), format, digits = 4, scientific = FALSE)) %>%
  select("draw", "district", "total_pop") %>%
  write_csv(paste("data-out/plans/",
                  as.character(pref_code),
                  "_",
                  as.character(pref_name),
                  "hr_2020_stats_non_ishikari.csv",
                  sep = ""))
