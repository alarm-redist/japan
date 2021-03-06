###############################################################################
# Data visualization for `07_fukushima`
# © ALARM Project, June 2022
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the codes in the column `pref$code`
# i.e. For rural prefectures, define using the municipality codes, not the gun codes
# i.e. For urban prefectures, define using gun codes if gun was merged
koiki_1_codes <- c(7203, 7207, 7210, 7211, 7214, 7322, 7342, 7408, 7501, 7521)
koiki_2_codes <- c(7205, 7461, 7464, 7465, 7466, 7481)
koiki_3_codes <- c(7208, 7402)
koiki_4_codes <- c(7212, 7564)
koiki_5_codes <- c(7481)

####-------------- 1. Method for Rural Prefectures-------------------------####
# Load data
for (i in 0)
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

# Calculate max:min ratio
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)

# Assign koiki_renkei area codes for simulation with 0 split
koiki_1_0 <- pref_0$code
koiki_1_0[koiki_1_0 %in% koiki_1_codes] <- 1
koiki_2_0 <- pref_0$code
koiki_2_0[koiki_2_0 %in% koiki_2_codes] <- 2
koiki_3_0 <- pref_0$code
koiki_3_0[koiki_3_0 %in% koiki_3_codes] <- 3
koiki_4_0 <- pref_0$code
koiki_4_0[koiki_4_0 %in% koiki_4_codes] <- 4
koiki_5_0 <- pref_0$code
koiki_5_0[koiki_5_0 %in% koiki_5_codes] <- 5

# Count number of gun splits
gun_split_0 <- redist::redist.splits(pref_smc_plans_0, pref_map_0$gun_code) %>%
  matrix(ncol = ndists_new, byrow = TRUE)
gun_split_0 <- gun_split_0[,1]

# Count number of koiki renkei splits
koiki_split_0 <-
  redist::redist.splits(pref_smc_plans_0, koiki_1_0) +
  redist::redist.splits(pref_smc_plans_0, koiki_2_0) +
  redist::redist.splits(pref_smc_plans_0, koiki_3_0) +
  redist::redist.splits(pref_smc_plans_0, koiki_4_0) +
  redist::redist.splits(pref_smc_plans_0, koiki_5_0)
koiki_split_0 <- koiki_split_0 %>%
  matrix(ncol = ndists_new, byrow = TRUE)
koiki_split_0 <- koiki_split_0[,1]

# Compile results: 0 split
results_0 <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_0)))
results_0$max_to_min <- wgt_smc_0$max_to_min
results_0$gun_split <- gun_split_0
results_0$koiki_split <- koiki_split_0
results_0$index <- 1:nrow(wgt_smc_0)

# Add bridges and check if valid
bridges_0 <- c()
results_0$valid <- check_valid(pref_0, pref_smc_plans_0, bridges_0)

# TODO: filter out plans with discontiguities
functioning_results_0 <- results_0 %>% dplyr::filter(valid)

# nrow(functioning_results_0) and nrow(functioning_results_1) must be over 5,000.
# If not, increase nsims and run more simulations.

# Sample 5,000 plans
set.seed(2020)
valid_sample_0 <- sample(functioning_results_0$index, 5000, replace = FALSE)
sim_smc_pref_0_sample <- sim_smc_pref_0 %>%
  filter(draw %in% valid_sample_0)

# Filter out sampled plans
results_0_sample <- functioning_results_0 %>%
  filter(index %in% valid_sample_0)

# Find Optimal Plan
optimal_0 <- results_0_sample$index[which(results_0_sample$max_to_min ==
                                            min(results_0_sample$max_to_min))][1]
results_0_sample[which(results_0_sample$index == optimal_0),]

# Optimal Plan: 0 split
matrix_optimal_0 <- redist::get_plans_matrix(sim_smc_pref_0 %>% filter(draw == optimal_0))
colnames(matrix_optimal_0) <- "district"
optimal_boundary_0 <- cbind(pref_map_0, as_tibble(matrix_optimal_0))

# Gun/Municipality/Koiki-renkei boundaries
mun_boundary <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary <- pref %>%
  filter(gun_code >= (pref_map_0$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# Combine municipality boundary data
mun <- mun_boundary %>% summarise(geometry = sf::st_combine(geometry))
mun$type <- "市区町村の境界"
# Combine gun boundary data
gun <- gun_boundary %>% summarise(geometry = sf::st_combine(geometry))
gun$type <- "郡の境界"

# Boundary for plot with 0 split
boundary_0 <- rbind(mun, gun)

# Co-occurrence
# Filter out plans with top 10% koiki-renkei areas
good_num_0 <-  results_0_sample %>%
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
   pref_smc_plans_n,
   sim_smc_pref_n,
   sim_smc_pref_0,
   sim_smc_pref_0_good,
   wgt_smc_0,
   gun_split_0,
   koiki_split_0,
   matrix_optimal_0,
   census_mun_old_2020,
   geom,
   pop,
   pref_pop_2020,
   pref_shp_2015,
   pref_shp_cleaned,
   old_mun,
   functioning_results_0,
   results_0
)

save.image(paste("data-out/pref/",
                 as.character(pref_code),
                 "_",
                 as.character(pref_name),
                 "_data",
                 ".Rdata",
                 sep = ""))

# Save relevant files to upload to Dataverse
for (i in 0){
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
    select("draw", "district", "total_pop") %>%

    write_csv(paste("data-out/plans/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_hr_2020_stats_",
                    as.character(i),
                    ".csv",
                    sep = ""))
}
