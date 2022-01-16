###############################################################################
# Data visualization for `[TODO]`
# © ALARM Project, November 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Make sure to convert municipality codes into to "gun" codes if "gun" was merged
koiki_1_codes <- c()
koiki_2_codes <- c()
koiki_3_codes <- c()

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
# When a municipality that belongs to a koiki renkei area is split:
koiki_2_1 <- pref_1$gun_code
koiki_2_1[koiki_2_1 %in% c(koiki_2_codes,
                           setdiff(pref_1$gun_code[which(pref_1$code == split_code)], split_code))] <- 2
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
results_1$num_mun_split <- num_mun_split_1
results_1$mun_split <- mun_split_1
results_1$multi <-  num_mun_split_1 - mun_split_1
results_1$num_gun_split <- num_gun_split_1
results_1$gun_split <- gun_split_1
results_1$koiki_split <- koiki_split_1
results_1$index <- 1:nrow(wgt_smc_1)

# Add bridges and check if valid
bridges_0 <- c()
results_0$valid <- check_valid(pref_0, pref_smc_plans_0, bridges_0)
bridges_1 <- c()
results_1$valid <- check_valid(pref_1, pref_smc_plans_1, bridges_1)

# To-do: filter out plans with discontiguities
functioning_results_0 <- results_0 %>% dplyr::filter(valid)
functioning_results_1 <- results_1 %>% dplyr::filter(multi == 0 & valid)

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

# Match district numbers
optimal_split <- dplyr::inner_join(as.data.frame(pref_1), as.data.frame(optimal_boundary_0),
                                   by = "code")
sim_smc_pref_1 <- redist::match_numbers(sim_smc_pref_1,
                                        optimal_split$district,
                                        col = "pop_overlap")

# Gun/Municipality/Koiki-renkei boundaries
mun_boundary <- pref_0 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
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

# Map with district data + municipality/gun/koiki-renkei boundary
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

# Map with district data + municipality/gun/koiki-renkei boundary
ggplot() +
  geom_sf(data = optimal_boundary_1, aes(fill = factor(district))) +
  scale_fill_manual(values = c("orange", "green", "blue", "yellow", "brown", "purple")) +
  geom_sf(data = mun_boundary, fill = NA, color = "black", lwd = 0.4) +
  geom_sf(data = gun_boundary, fill = NA, color = "black", lwd = 1.0) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

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

# Match membership data with map object
pref_0_membership <- cbind(pref_0, cooc_ratio, pref_membership_0)
pref_0_membership_1 <- pref_0_membership %>% dplyr::filter(membership == 1)
pref_0_membership_2 <- pref_0_membership %>% dplyr::filter(membership == 2)
pref_0_membership_3 <- pref_0_membership %>% dplyr::filter(membership == 3)
pref_0_membership_4 <- pref_0_membership %>% dplyr::filter(membership == 4)
pref_0_membership_5 <- pref_0_membership %>% dplyr::filter(membership == 5)
pref_0_membership_6 <- pref_0_membership %>% dplyr::filter(membership == 6)

"orange", "green", "blue", "yellow", "brown", "purple"

# Co-occurrence plot
ggplot() +
  geom_sf(data = pref_0_membership_1, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low = "lightsalmon", high = "orange") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = pref_0_membership_2, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="palegreen", high="green") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = pref_0_membership_3, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low = "skyblue", high = "blue") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = pref_0_membership_4, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low="yellow", high="yellow3") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = pref_0_membership_5, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low = "brown1", high = "brown4") +

  ggnewscale::new_scale_fill() +
  geom_sf(data = pref_0_membership_6, aes(fill = cooc_ratio), show.legend = FALSE) +
  scale_fill_gradient(low = "purple", high = "purple4") +

  labs(color = "Co-occurrence",
       title = "Co-occurrence Analysis: Plans with Top 10% Max-min Ratio") +

  geom_sf(data = gun_boundary, fill = NA, color = "black", lwd = 1.0) +

  theme(legend.box = "vertical", legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank())

####-------------- 2. Method for Urban Prefectures-------------------------####
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
