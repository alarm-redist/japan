###############################################################################
# Data visualization for `Ehime`
# © ALARM Project, November 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the municipality codes, not the gun codes
koiki_1_codes <- c(38201, 38210, 38215, 38386, 38401, 38402)
koiki_2_codes <- c(38203, 38484, 38488, 38506)

# Load data
for (i in 0:1)
{
  pref_map_n <- readRDS(paste("data-out/maps/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_map_",
                            as.character(nsims),
                            "_",
                            as.character(i),
                            ".Rds",
                            sep = ""))
  assign(paste("pref_map_", i, sep = ""), pref_map_n)

  prefadj_n <-readRDS(paste("data-out/pref/",
                           as.character(pref_code),
                           "_",
                           as.character(pref_name),
                           "_",
                           as.character(nsims),
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

# Add Matsuyama-shi back to result of simulation with 0 split
sim_smc_pref_0_with_Matsuyama <- NULL
for(i in 1:nsims){
  with_Matsuyama <-
    dplyr::bind_rows(as_tibble(sim_smc_pref_0 %>% filter(draw == i)),
                     data.frame(draw = as.factor(i),
                                district = as.integer(3),
                                total_pop = pref$pop[which(pref$code == 38201)]))
  sim_smc_pref_0_with_Matsuyama <- rbind(sim_smc_pref_0_with_Matsuyama, with_Matsuyama)
}


# Add Kyu-Matsuyma-shi back to result of simulation with 1 split
pop_Kyu_Matsuyama <- reflect_old_boundaries(pref_0_with_matsuyama, old_boundary, census2020, new_1) %>%
                        filter(pre_gappei_code == 38201) %>%
                        pull(pop)
sim_smc_pref_1_with_Matsuyama <- NULL
for(i in 1:nsims){
  with_Matsuyama <-
    dplyr::bind_rows(as_tibble(sim_smc_pref_1 %>% filter(draw == i)),
                     data.frame(draw = as.factor(i),
                                district = as.integer(3),
                                total_pop = pop_Kyu_Matsuyama))
  sim_smc_pref_1_with_Matsuyama <- rbind(sim_smc_pref_1_with_Matsuyama, with_Matsuyama)
}


# Calculate max:min ratio
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0_with_Matsuyama)
wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1_with_Matsuyama)

# Assign koiki_renkei area codes for simulation with 0 split
# There is no need to count the number of splits for koiki_1 because Matsuyama-shi,
# which belongs to koiki_1, is set aside and thus koiki_1 is automatically split.
koiki_2_0 <- pref_0$code
koiki_2_0[koiki_2_0 %in% koiki_2_codes] <- 2

# Assign koiki_renkei area codes for simulation with 1 split
# When a municipality that belongs to a koiki-renkei area is split:
koiki_2_1 <- pref_1$pre_gappei_code
koiki_2_1[koiki_2_1 %in% koiki_2_codes] <- 2

# Count number of municipality splits
# This is by nature 1 for Ehime, where Matsuyama-shi must be split.
num_mun_split_1 <- 1
mun_split_1 <- 1

# Count number of gun splits
num_gun_split_0 <- count_splits(pref_smc_plans_0, pref_map_0$gun_code)
gun_split_0 <- redist::redist.splits(pref_smc_plans_0, pref_map_0$gun_code)
num_gun_split_1 <- count_splits(pref_smc_plans_1, pref_map_1$gun_code)
gun_split_1 <- redist::redist.splits(pref_smc_plans_1, pref_map_1$gun_code)

# Count number of koiki renkei splits
koiki_split_0 <-
  1 + #We know that koiki_1 is split because Matsuyama-shi is set aside.
  redist::redist.splits(pref_smc_plans_0, koiki_2_0)
koiki_split_1 <-
  1 + #We know that koiki_1 is split because Kyu-Matsuyama-shi is set aside.
  redist::redist.splits(pref_smc_plans_1, koiki_2_1)

# Compile results: 0 split
results_0 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_0)))
results_0$max_to_min <- wgt_smc_0$max_to_min
results_0$num_gun_split <- num_gun_split_0
results_0$gun_split <- gun_split_0
results_0$koiki_split <- koiki_split_0
results_0$index <- 1:nrow(wgt_smc_0)

# Compile results: 1 split
results_1 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_1)))
results_1$max_to_min <- wgt_smc_1$max_to_min
results_1$num_mun_split <- num_mun_split_1
results_1$mun_split <- mun_split_1
results_1$multi <-  num_mun_split_1 - mun_split_1
results_1$num_gun_split <- num_gun_split_1
results_1$gun_split <- gun_split_1
results_1$koiki_split <- koiki_split_1
results_1$index <- 1:nrow(wgt_smc_1)


# TODO: filter out plans with discontiguities
functioning_results_0 <- results_0
functioning_results_1 <- results_1

# Find Optimal Plan
optimal_0 <- functioning_results_0$index[which(functioning_results_0$max_to_min ==
                                                 min(functioning_results_0$max_to_min))][1]
results_0[optimal_0,]
optimal_1 <- functioning_results_1$index[which(functioning_results_1$max_to_min ==
                                                 min(functioning_results_1$max_to_min))][1]
results_1[optimal_1,]

# Optimal Plan: 0 split
matrix_optimal_0 <- redist::get_plans_matrix(sim_smc_pref_0 %>% filter(draw == optimal_0))
colnames(matrix_optimal_0) <- "district"
optimal_boundary_0 <- cbind(pref_map_0, as_tibble(matrix_optimal_0))

# Gun/Municipality/Koiki-renkei boundaries
mun_boundary <- pref_0 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary <- pref_0 %>%
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
good_num_0 <-  functioning_results_0 %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(functioning_results_0$index)*0.1)) %>%
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
   sim_smc_pref_0,
   sim_smc_pref_1,
   sim_smc_pref_n,
   wgt_smc_0,
   wgt_smc_1,
   num_mun_split_1,
   mun_split_1,
   num_gun_split_0,
   gun_split_0,
   num_gun_split_1,
   gun_split_1,
   koiki_split_0,
   koiki_split_1,
   matrix_optimal_0,
   matrix_optimal_1
   )
save.image(paste("data-out/pref/",
                 as.character(pref_code),
                 "_",
                 as.character(pref_name),
                 "_data",
                 ".Rdata",
                 sep = ""))
