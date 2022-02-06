###############################################################################
# Data visualization for `Aichi`
# © ALARM Project, November 2021
###############################################################################

# TODO Define the koiki-renkei areas (広域連携)
# Define which municipality/gun belongs to which koiki renkei area
# Define using the municipality codes, not the gun codes
koiki_1_codes <- c(23210, 23225, 23227, 23302)

pref_map <- readRDS(paste("data-out/maps/",
                           as.character(pref_code),
                           "_",
                           as.character(pref_name),
                           "_map_",
                           as.character(nsims),
                           ".Rds",
                           sep = ""))

prefadj <- readRDS(paste("data-out/pref/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         "_",
                         as.character(nsims),
                         "_adj",
                         ".Rds",
                         sep = ""))

sim_smc_pref <- readRDS(paste("data-out/plans/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              ".Rds",
                              sep = ""), refhook = NULL)

# Get plans matrix
pref_smc_plans <- redist::get_plans_matrix(sim_smc_pref)

# Calculate max:min ratio
wgt_smc <- simulation_weight_disparity_table(sim_smc_pref)

# Assign koiki_renkei area codes for simulation with 0 split
koiki_1 <- pref$code
koiki_1[koiki_1 %in% koiki_1_codes] <- 1
koiki_1[!koiki_1 %in% 1] <-
  seq(1000, 1000 +length(koiki_1[!koiki_1 %in% c(koiki_1_codes, 1)]) - 1, by = 1)

# Count number of municipality splits
num_mun_split <- count_splits(pref_smc_plans, pref_map$code)
mun_split <- redist::redist.splits(pref_smc_plans, pref_map$code)

# Count number of gun splits
gun_index <- pref$gun_code
gun_index[gun_index < (pref_map$code[1]%/%1000)*1000+300] <-
  seq(100000, 100000 + length(gun_index[gun_index < (pref_map$code[1]%/%1000)*1000+300])-1, by = 1)

gun_split <- redist::redist.splits(pref_smc_plans, gun_index)

# Count number of koiki renkei splits
koiki_split <-
  redist::redist.splits(pref_smc_plans, koiki_1)

# Compile results
results <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc)))
results$max_to_min <- wgt_smc$max_to_min
results$gun_split <- gun_split
results$num_mun_split <- num_mun_split
results$mun_split <- mun_split
results$multi <-  num_mun_split - mun_split
results$koiki_split <- koiki_split
results$index <- 1:nrow(wgt_smc)

# Filter out plans with discontiguities/multi-splits
# Amagun (code: 23420) is consisted of three disconnected municipalities.
# There are four ways to avoid separating Amagun into different districts:
#1. Ensuring that Amagun (code: 23420), Yatomishi (code: 23235),and Amashi (code: 23237) are in the same district
#2. Ensuring that Amagun (code: 23420), Minatoku (code: 23111), and Amashi (code: 23237) are in the same district
#3. Ensuring that Amagun (code: 23420), Yatomishi (code: 23235),and Nakagawaku (code: 23110) are in the same district
#4. Ensuring that Amagun (code: 23420), Minatoku (code: 23111), and Nakagawaku (code: 23110) are in the same district

contiguous_1 <- 1:nsims
for(i in 1:nsims){
  contiguous_1[i] <-
    #Returns 1 if  Amagun (code: 23420), Yatomishi (code: 23235), and Amashi (code: 23237) are in the same district
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23235)])) *
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23237)])) +
    #Returns 1 if  Amagun (code: 23420), Minatoku (code: 23111), and Amashi (code: 23237) are in the same district
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23111)])) *
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23237)])) +
    #Returns 1 if  Amagun (code: 23420), Yatomishi (code: 23235), and Nakagawaku (code: 23110) are in the same district
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23235)])) *
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23110)])) +
    #Returns 1 if Amagun (code: 23420), Minatoku (code: 23111), and Nakagawaku (code: 23110) are in the same district
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23111)])) *
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23420)] == pref_smc_plans[,i][which(pref$code == 23110)]))
}
results$contiguous_1 <- contiguous_1

# Chitagun (code: 23440) is consisted of two disconnected parts.
#In order to avoid separating Chitagun into different districts,
# we must ensure that Chitagun is in the same district as either Handa-shi or Tokoname-shi.
contiguous_2 <- 1:nsims
for(i in 1:nsims){
  contiguous_2[i] <-
    #Returns 1 if  Chitagun (code: 23440) and Handa-shi (code: 23205) are in the same district.
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23440)] == pref_smc_plans[,i][which(pref$code == 23205)])) +
    #Returns 1 if  Chitagun (code: 23440) and Tokoname-shi (code: 23216) are in the same district
    max(as.integer(pref_smc_plans[,i][which(pref$code == 23440)] == pref_smc_plans[,i][which(pref$code == 23216)]))
}
results$contiguous_2 <- contiguous_2

# Filter out plans with multi-splits and discontiguities
functioning_results <- results[which(results$multi == 0 & results$contiguous_1 > 0 & results$contiguous_2 > 0), ]

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

# Combine municipality boundary data
mun <- mun_boundary %>% summarise(geometry = sf::st_combine(geometry))
mun$type <- "市区町村の境界"
# Combine gun boundary data
gun <- gun_boundary %>% summarise(geometry = sf::st_combine(geometry))
gun$type <- "郡の境界"

# Boundary for plot with 0 split
boundary <- rbind(mun, gun)

# Co-occurrence
# Filter out plans with top 10% koiki-renkei areas
good_num <-  functioning_results %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(length(functioning_results$index)*0.1)) %>%
  select(index)
good_num <- as.vector(t(good_num))
sim_smc_pref_good <- sim_smc_pref %>%
  filter(draw %in% good_num)

# Obtain co-occurrence matrix
m_co = redist::prec_cooccurrence(sim_smc_pref_good, sampled_only=TRUE)

# Create clusters
cl_co = cluster::agnes(m_co)
prec_clusters = cutree(cl_co, ndists_new)
pref_membership <- as_tibble(as.data.frame(prec_clusters))
names(pref_membership) <- "membership"

# Obtain co-occurrence ratio
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
rm(census2020,
   census2020_current_municipalities,
   cl_co,
   constr,
   dem_pops,
   m_co,
   mun,
   gun,
   mun_boundary,
   gun_boundary,
   pref_cleaned,
   pref_gun,
   pref_map,
   pref_non_gun,
   pref_raw,
   pref_smc_plans,
   sim_smc_pref,
   sim_smc_pref_good,
   wgt_smc,
   num_mun_split,
   mun_split,
   gun_split,
   koiki_split,
   matrix_optimal
)

save.image(paste("data-out/pref/",
                 as.character(pref_code),
                 "_",
                 as.character(pref_name),
                 "_data",
                 ".Rdata",
                 sep = ""))
