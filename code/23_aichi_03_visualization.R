###############################################################################
# Data visualization for `Aichi`
# © ALARM Project, November 2021
###############################################################################

# Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Make sure to convert municipality codes into to "gun" codes if "gun" was merged
koiki_code_1 <- c(23210, 23225, 23227, 23300)

# Get plans matrix
pref_smc_plans <- redist::get_plans_matrix(sim_smc_pref)

# Calculate max:min ratio
wgt_smc <- simulation_weight_disparity_table(sim_smc_pref)

# Assign koiki_renkei area codes for simulation with 0 split
koiki_1_0 <- pref$gun_code
koiki_1_0[koiki_1_0 %in% koiki_1_codes] <- 1
koiki_2_0 <- pref$gun_code
koiki_2_0[koiki_2_0 %in% koiki_2_codes] <- 2
koiki_3_0 <- pref$gun_code
koiki_3_0[koiki_3_0 %in% koiki_3_codes] <- 3

# Count number of municipality splits
num_mun_split <- count_splits(pref_smc_plans, pref_map$code)
mun_split <- redist::redist.splits(pref_smc_plans, pref_map$code)

# Count number of gun splits
num_gun_split <- count_splits(pref_smc_plans, pref_map$gun_code)
gun_split <- redist::redist.splits(pref_smc_plans, pref_map$gun_code)

# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(pref_smc_plans, koiki_1_0) +
  redist::redist.splits(pref_smc_plans, koiki_2_0) +
  redist::redist.splits(pref_smc_plans, koiki_3_0)

# Compile results
results <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results$max_to_min <- wgt_smc$max_to_min
results$num_gun_split <- num_gun_split
results$gun_split <- gun_split
results$num_mun_split <- num_mun_split
results$mun_split <- mun_split
results$multi <-  num_mun_split - mun_split
results$koiki_split <- koiki_split
results$index <- 1:nrow(wgt_smc)

# Add bridges and check if valid
bridges <- c()
results$valid <- check_valid(pref, pref_smc_plans, bridges)

# To-do: filter out plans with discontiguities/multi-splits
functioning_results <- results %>% dplyr::filter(multi == 0 && valid)

# Find Optimal Plan
optimal <- functioning_results$index[which(functioning_results$max_to_min ==
                                             min(functioning_results$max_to_min))][1]
results[optimal,]

# Gun/Municipality boundaries
mun_boundary <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary <- pref %>%
  filter(gun_code >= (pref_map$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# District Boundary of Optimal Plan
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref %>% filter(draw == optimal))
colnames(matrix_optimal) <- "district"
optimal_boundary <- cbind(pref_map, as_tibble(matrix_optimal))

# Map with district data + municipality/gun/koiki-renkei boundary
ggplot() +
  geom_sf(data = optimal_boundary, aes(fill = factor(district))) +
  scale_fill_manual(values = as.vector(pals::polychrome(ndists_new)))+
  geom_sf(data = gun_boundary, fill = NA, color = "black", lwd = 1.0) +
  geom_sf(data = mun_boundary, fill = NA, color = "black", lwd = 0.4) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())
