###############################################################################
# Data visualization for `[TODO]`
# © ALARM Project, November 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Make sure to convert municipality codes into to "gun" codes if "gun" was merged
koiki_code_1 <- c()
koiki_code_2 <- c()
koiki_code_3 <- c()

####-------------- 1. Method for Rural Prefectures-------------------------####
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

#assign koiki_renkei area codes
#assign which municipality/gun belongs to which koiki renkei area
#make sure to convert municipality codes into to "gun" codes
koiki_1_codes <-  c(34101, 34102, 34103, 34104, 34105, 34106, 34107, 34108,
                    34202, 34203, 34204, 34211, 34212, 34213, 34214, 34215,
                    34300, 34360, 34420, 34460)
koiki_2_codes <- c(34207, 34204, 34205, 34208, 34460, 34540)
koiki_3_codes <- c(34202, 34203, 34212, 34215, 34300, 34420)

# Assign koiki_renkei area codes for simulation with 0 split
koiki_1_0 <- pref_0$gun_code
koiki_1_0[koiki_1_0 %in% koiki_1_codes] <- 1
koiki_2_0 <- pref_0$gun_code
koiki_2_0[koiki_2_0 %in% koiki_2_codes] <- 2
koiki_3_0 <- pref_0$gun_code
koiki_3_0[koiki_3_0 %in% koiki_3_codes] <- 3

# Assign koiki_renkei area codes for simulation with 1 split
koiki_1_1 <- pref_1$gun_code
koiki_1_1[koiki_1_1 %in% koiki_1_codes] <- 1
koiki_2_1 <- pref_1$gun_code
koiki_2_1[koiki_2_1 %in% c(koiki_2_codes,
                           setdiff(pref_1$gun_code[which(pref_1$code == split_code)], split_code))] <- 2
# split_code: 34207
koiki_3_1 <- pref_1$gun_code
koiki_3_1[koiki_3_1 %in% koiki_3_codes] <- 3

# Count number of municipality splits
num_mun_split_1 <- count_splits(pref_smc_plans_1, pref_map_1$code)
mun_split_1 <- redist::redist.splits(pref_smc_plans_1, pref_map_1$code)

# Count number of gun splits
num_gun_split_0 <- count_splits(pref_smc_plans_0, pref_map_0$gun_code)
gun_split_0 <- redist::redist.splits(pref_smc_plans_0, pref_map_0$gun_code)
num_gun_split_1 <- count_splits(pref_smc_plans_1, pref_map_1$gun_code)
gun_split_1 <- redist::redist.splits(pref_smc_plans_1, pref_map_1$gun_code)

# Count number of koiki renkei splits
koiki_split_0 <-
  redist::redist.splits(pref_smc_plans_0, koiki_1_0) +
  redist::redist.splits(pref_smc_plans_0, koiki_2_0) +
  redist::redist.splits(pref_smc_plans_0, koiki_3_0)
koiki_split_1 <-
  redist::redist.splits(pref_smc_plans_1, koiki_1_1) +
  redist::redist.splits(pref_smc_plans_1, koiki_2_1) +
  redist::redist.splits(pref_smc_plans_1, koiki_3_1)

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
results_1$nun_mun_split <- num_mun_split_1
results_1$mun_split <- mun_split_1
results_1$multi <-  results_1$nun_mun_split - results_1$mun_split
results_1$num_gun_split <- num_gun_split_1
results_1$gun_split <- gun_split_1
results_1$koiki_split <- koiki_split_1
results_1$index <- 1:nrow(wgt_smc_1)

# To-do: filter out plans with discontiguities
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

gun_boundary <- pref_0 %>%
  filter(gun_code >= (pref_map_0$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

koiki_boundary_1 <- pref_0 %>%
  filter(gun_code %in% koiki_1_codes) %>%
  summarise(geometry = sf::st_union(geometry))
koiki_boundary_2 <- pref_0 %>%
  filter(gun_code %in% koiki_2_codes) %>%
  summarise(geometry = sf::st_union(geometry))
koiki_boundary_3 <- pref_0 %>%
  filter(gun_code %in% koiki_3_codes) %>%
  summarise(geometry = sf::st_union(geometry))

mun_boundary <- pref_0 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality/gun/koiki-renkei boundary
ggplot() +
  geom_sf(data = optimal_boundary_0, aes(fill = factor(district))) +
  scale_fill_manual(values = c("orange", "green", "blue", "yellow", "brown", "purple")) +
  geom_sf(data = gun_boundary, fill = NA, color = "black", lwd = 1.0) +
  geom_sf(data = mun_boundary, fill = NA, color = "black", lwd = 0.4) +
  #geom_sf(data = koiki_boundary_1, fill = "plum1", alpha = 0.5, color = "plum1", lwd = 0.2) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

# Optimal Plan: 1 split
matrix_optimal_1 <- redist::get_plans_matrix(sim_smc_pref_1 %>% filter(draw == optimal_1))
colnames(matrix_optimal_1) <- "district"
optimal_boundary_1 <- cbind(pref_map_1, as_tibble(matrix_optimal_1))

#map with district data + municipality/gun/koiki-renkei boundary
ggplot() +
  geom_sf(data = optimal_boundary_1, aes(fill = factor(district))) +
  scale_fill_manual(values = c("1" = "yellow", "2" = "brown", "3" = "orange",
                               "4" = "purple", "5" = "green", "6" = "blue")) +
  geom_sf(data = mun_boundary, fill = NA, color = "black", lwd = 0.4) +
  geom_sf(data = gun_boundary, fill = NA, color = "black", lwd = 1.0) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())


