###############################################################################
# Data visualization for Tokyo
# © ALARM Project, November 2021
###############################################################################

#######Urban area###############
# Get plans matrix
urban_smc_plans <- redist::get_plans_matrix(sim_smc_urban)

# Calculate max:min ratio
wgt_smc_urban <- simulation_weight_disparity_table(sim_smc_urban)

# Count number of municipality splits
num_mun_split_urban <- count_splits(urban_smc_plans, urban_map$code)
mun_split_urban <- redist::redist.splits(urban_smc_plans, urban_map$code)

# Count number of gun splits
num_gun_split_urban <- count_splits(urban_smc_plans, urban_map$gun_code)
gun_split_urban <- redist::redist.splits(urban_smc_plans, urban_map$gun_code)

# Compile results: 0 split
results_urban <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_urban)))
results_urban$max_to_min <- wgt_smc_urban$max_to_min
results_urban$num_gun_split <- 0
results_urban$gun_split <- 0
results_urban$nun_mun_split <- num_mun_split_urban
results_urban$mun_split <- mun_split_urban
results_urban$multi <-  num_mun_split_urban - mun_split_urban
results_urban$index <- 1:nrow(wgt_smc_urban)

# To-do: filter out plans with discontiguities/multi-splits
functioning_results_urban <- results_urban %>% dplyr::filter(multi == 0)

# Find Optimal Plan
optimal_urban <- functioning_results_urban$index[which(functioning_results_urban$max_to_min ==
                                             min(functioning_results_urban$max_to_min))][1]
results_urban[optimal_urban,]

# Gun/Municipality boundaries
mun_boundary_urban <- urban %>%
  filter(code %in% 13101:13123) %>% #filter out islands
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

# District Boundary of Optimal Plan
matrix_optimal_urban <- redist::get_plans_matrix(sim_smc_urban %>% filter(draw == optimal_urban))
colnames(matrix_optimal_urban) <- "district"
matrix_optimal_urban <- head(matrix_optimal_urban,
                             length((urban %>% filter(code %in% 13101:13123))$code)) #filter out islands
optimal_boundary_urban <- cbind(head(urban_map, length((urban %>% filter(code %in% 13101:13123))$code)),
                                as_tibble(matrix_optimal_urban))

# Map with district data + municipality/gun/koiki-renkei boundary
ggplot() +
  geom_sf(data = optimal_boundary_urban, aes(fill = factor(district))) +
  scale_fill_manual(values = as.vector(pals::polychrome(ndists_new)))+
  geom_sf(data = mun_boundary_urban, fill = NA, color = "black", lwd = 0.4) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

# max/min pop
max((sim_smc_urban %>% filter(draw == optimal_urban))$total_pop)
min((sim_smc_urban %>% filter(draw == optimal_urban))$total_pop)


### Rural area
# Get plans matrix
rural_smc_plans <- redist::get_plans_matrix(sim_smc_rural)

# Calculate max:min ratio
wgt_smc_rural <- simulation_weight_disparity_table(sim_smc_rural)

# Count number of municipality splits
num_mun_split_rural <- count_splits(rural_smc_plans, rural_map$code)
mun_split_rural <- redist::redist.splits(rural_smc_plans, rural_map$code)

# Count number of gun splits
num_gun_split_rural <- count_splits(rural_smc_plans, rural_map$gun_code)
gun_split_rural <- redist::redist.splits(rural_smc_plans, rural_map$gun_code)

# Compile results: 0 split
results_rural<- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_rural)))
results_rural$max_to_min <- wgt_smc_rural$max_to_min
results_rural$num_gun_split <- 0
results_rural$gun_split <- 0
results_rural$nun_mun_split <- num_mun_split_rural
results_rural$mun_split <- mun_split_rural
results_rural$multi <-  num_mun_split_rural - mun_split_rural
results_rural$index <- 1:nrow(wgt_smc_rural)

# Filter out plans with discontiguities/multi-splits
functioning_results_rural <- results_rural %>% dplyr::filter(multi == 0)

# Find Optimal Plan
optimal_rural <- functioning_results_rural$index[which(functioning_results_rural$max_to_min ==
                                                         min(functioning_results_rural$max_to_min))][1]
results_rural[optimal_rural,]

# Gun/Municipality boundaries
mun_boundary_rural <- rural %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
gun_boundary_rural <- rural %>%
  filter(gun_code >= (rural_map$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))

# District Boundary of Optimal Plan
matrix_optimal_rural <- redist::get_plans_matrix(sim_smc_rural %>% filter(draw == optimal_rural))
colnames(matrix_optimal_rural) <- "district"
optimal_boundary_rural <- cbind(rural_map, as_tibble(matrix_optimal_rural))

# Map with district data + municipality/gun/koiki-renkei boundary
ggplot() +
  geom_sf(data = optimal_boundary_rural, aes(fill = factor(district))) +
  scale_fill_manual(values = as.vector(pals::polychrome(ndists_new)))+
  geom_sf(data = mun_boundary_rural, fill = NA, color = "black", lwd = 0.4) +
  geom_sf(data = gun_boundary_rural, fill = NA, color = "black", lwd = 1.0) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

# max/min pop
max((sim_smc_rural %>% filter(draw == optimal_rural))$total_pop)
min((sim_smc_rural %>% filter(draw == optimal_rural))$total_pop)








