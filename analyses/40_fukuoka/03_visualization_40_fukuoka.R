###############################################################################
# Data visualization for `40_fukuoka`
# © ALARM Project, July 2022
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the codes in the column `pref$code`
# i.e. For rural prefectures, define using the municipality codes, not the gun codes
# i.e. For urban prefectures, define using gun codes if gun was merged
koiki_1_codes <- c(40203, 40212, 40216, 40225, 40500, 40520)
koiki_2_codes <- c(40101, 40103, 40105, 40106, 40107, 40108, 40109,
                   40204, 40213, 40214, 40215, 40226,
                   40380, 40400, 40600, 40620, 40640)
koiki_3_codes <- c(40202, 40229, 40207)
koiki_4_codes <- c(40205, 40227, 40420)
koiki_5_codes <- c(40206, 40600)
koiki_6_codes <- c(40214, 40640)

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
                         "_adj.Rds",
                         sep = ""))

sim_smc_pref <- readRDS(paste("data-out/plans/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims * 4),
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
koiki_3 <- pref$code
koiki_3[koiki_3 %in% koiki_3_codes] <- 3
koiki_3[!koiki_3 %in% 3] <-
  seq(1000, 1000 +length(koiki_3[!koiki_3 %in% c(koiki_3_codes, 3)]) - 1, by = 1)
koiki_4 <- pref$code
koiki_4[koiki_4 %in% koiki_4_codes] <- 4
koiki_4[!koiki_4 %in% 4] <-
  seq(1000, 1000 +length(koiki_4[!koiki_4 %in% c(koiki_4_codes, 4)]) - 1, by = 1)
koiki_5 <- pref$code
koiki_5[koiki_5 %in% koiki_5_codes] <- 5
koiki_5[!koiki_5 %in% 5] <-
  seq(1000, 1000 +length(koiki_5[!koiki_5 %in% c(koiki_5_codes, 5)]) - 1, by = 1)
koiki_6 <- pref$code
koiki_6[koiki_6 %in% koiki_6_codes] <- 6
koiki_6[!koiki_6 %in% 6] <-
  seq(1000, 1000 +length(koiki_6[!koiki_6 %in% c(koiki_6_codes, 6)]) - 1, by = 1)

# Count number of municipality splits
num_mun_split <- count_splits(pref_smc_plans, pref_map$code)
mun_split <- redist::redist.splits(pref_smc_plans, pref_map$code) %>%
  matrix(ncol = ndists_new, byrow = TRUE)
mun_split <- mun_split[,1]

# Count number of gun splits
gun_index <- pref$gun_code
gun_index[gun_index < (pref_map$code[1]%/%1000)*1000+300] <-
  seq(100000, 100000 + length(gun_index[gun_index < (pref_map$code[1]%/%1000)*1000+300])-1, by = 1)

gun_split <- redist::redist.splits(pref_smc_plans, gun_index) %>%
  matrix(ncol = ndists_new, byrow = TRUE)
gun_split <- gun_split[,1]

# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(pref_smc_plans, koiki_1) +
  redist::redist.splits(pref_smc_plans, koiki_2) +
  redist::redist.splits(pref_smc_plans, koiki_3) +
  redist::redist.splits(pref_smc_plans, koiki_4) +
  redist::redist.splits(pref_smc_plans, koiki_5) +
  redist::redist.splits(pref_smc_plans, koiki_6)
koiki_split <- koiki_split %>%
  matrix(ncol = ndists_new, byrow = TRUE)
koiki_split <- koiki_split[,1]

# Compile results
results <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results$max_to_min <- wgt_smc$max_to_min
results$gun_split <- gun_split
results$num_mun_split <- num_mun_split
results$mun_split <- mun_split
results$multi <-  num_mun_split - mun_split
results$koiki_split <- koiki_split
results$index <- 1:nrow(wgt_smc)

# Confirm that the 郡 that are kept together in the same district under the enacted plan
# are not split in the simulated plans

# Define the codes of the 郡 that must be kept together in the same district
respect_gun_code <- setdiff(unique(gun_index[which(gun_index < 100000)]), gun_exception)

# Evaluate whether the 郡 that must be kept together in the same district are
# in the same district in the simulated plans
respect_gun_matrix <- matrix(0, nrow = length(respect_gun_code), ncol = ncol(pref_smc_plans))
for(i in 1:length(respect_gun_code)){
  for(j in 1:ncol(pref_smc_plans)){
    respect_gun_matrix[i, j] <-
      length(pref_smc_plans[which(pref$code == respect_gun_code[i]),j]) -1 ==
      sum(duplicated(pref_smc_plans[which(pref$code == respect_gun_code[i]),j]))
  }
}

# Store results
results$respect_gun <- colSums(respect_gun_matrix) == length(respect_gun_code)

# Confirm that the municipalities that are not split under the enacted plan
# are not split in the simulated plans

# Define the codes of the municipalities that must not be split
respect_mun_code <- setdiff(unique(pref$code), mun_not_freeze)

# Evaluate whether the municipalities that must be not be split are split in the simulated plans
respect_mun_matrix <- matrix(0, nrow = length(respect_mun_code), ncol = ncol(pref_smc_plans))
for(i in 1:length(respect_mun_code)){
  for(j in 1:ncol(pref_smc_plans)){
    respect_mun_matrix[i, j] <-
      length(pref_smc_plans[which(pref$code == respect_mun_code[i]),j]) -1 ==
      sum(duplicated(pref_smc_plans[which(pref$code == respect_mun_code[i]),j]))
  }
}

# Store results
results$respect_mun <- colSums(respect_mun_matrix) == length(respect_mun_code)

# Discard plans with multi-splits as well as plans that split 郡/municipalities that
# should not be split
functioning_results <- results %>%
  filter(respect_gun == TRUE, respect_mun == TRUE, multi == 0)

# Sample 5,000 plans
set.seed(2020)
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

# District boundary of optimal plan
matrix_optimal <- redist::get_plans_matrix(sim_smc_pref %>% filter(draw == optimal))
colnames(matrix_optimal) <- "district"
optimal_boundary <- cbind(pref_map, as_tibble(matrix_optimal))

# Co-occurrence
# Filter out plans with top 10% maxmin ratio
good_num <-  results_sample %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(results_sample$index)*0.1)) %>%
  select(index)
good_num <- as.vector(t(good_num))
sim_smc_pref_good <- sim_smc_pref_sample %>%
  filter(draw %in% good_num)

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)

# Compute clustering
cl_co = cluster::agnes(m_co)

# Analyze the dendrogram and pick an appropriate number of clusters
plot(as.dendrogram(cl_co))
abline(h = 2, col = "red") # explore different depths
abline(h = 1.5, col = "blue")

prec_clusters = cutree(cl_co, 18)  # change ndists_new to an appropriate number

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
    select("draw", "district", "total_pop") %>%
    write_csv(paste("data-out/plans/",
                    as.character(pref_code),
                    "_",
                    as.character(pref_name),
                    "_hr_2020_stats.csv",
                    sep = ""))

