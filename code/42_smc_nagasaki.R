# ----------- set up -------------#
library(tidyverse)
set.seed(12345)

remotes::install_github("alarm-redist/redist@dev")

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- Set-up -----------#
# shapefile
pref_code <- 42
pref_name <- "nagasaki"
sim_type <- "smc"
nsims <- 25000

nsplits_0 <- 0

# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4

pref_raw <- download_shp(pref_code)

pref <- clean_jcdf(pref_raw = pref_raw)

# 2020 census
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")

# clean it
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# combining two data
pref <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

# remove lake, not necessarily for Nagasaki
# pref <- remove_lake(pref, [insert lake string])

# check map
pref %>%
  ggplot() +
  geom_sf(fill = "red")

# download historical boundary data
old_pref <- download_old_shp(pref_code)

# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

# split Nagasaki-shi and Sasebo-shi
old_42201 <- find_old_codes(42201, pop_by_old_boundary)
old_42202 <- find_old_codes(42202, pop_by_old_boundary)

# -------- Zero-Split ------------#

# initialize the zero-split shp
pref_0 <- pref

# merge guns, exception of 北松浦郡
pref_0 <- merge_gun(pref_0, exception = 42383)

# simulation parameters
prefadj_0 <- redist::redist.adjacency(shp = pref_0) # Adjacency list

# add ferries
ferries_0 <- add_ferries(pref_0)
prefadj_0 <- geomander::add_edge(prefadj_0, ferries_0[, 1], ferries_0[, 2], zero = TRUE)

# check contiguity
suggest_0 <- geomander::suggest_component_connection(shp = pref_0, adj = prefadj_0)
prefadj_0 <- geomander::add_edge(prefadj_0, suggest_0$x,
                               suggest_0$y, zero = TRUE) # Fixing 壱岐市、対島市 isolation
prefadj_0 <- lapply(prefadj_0, unique)

pref_map_0 <- redist::redist_map(pref_0,
                               ndists = ndists_new,
                               pop_tol= 0.20,
                               total_pop = pop,
                               adj = prefadj_0)

# simulation
sim_smc_pref_0 <- redist::redist_smc(pref_map_0,
                                   nsims = nsims,
                                   pop_temper = 0.05)

saveRDS(sim_smc_pref_0, paste("simulation/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            "_",
                            as.character(nsplits_0),
                            ".Rds",
                            sep = ""))

# --------- One-Split ----------------#

nsplits_1 <- 1

# initialize pref_1 object
pref_1 <- pref

# merge guns, exception of 北松浦郡
pref_1 <- merge_gun(pref_1, exception = 42383)

# reflect old boundaries
pref_1 <- reflect_old_boundaries(pref_1, old_pref, pop_by_old_boundary, old_42201, 42201)

# estimation of old-boundary level national populations
pref_1 <- estimate_old_boundary_pop(old_42201, 42201, pref_1, census2020)

# simulation parameters
prefadj_1 <- redist::redist.adjacency(shp = pref_1) # Adjacency list

# add ferries
ferries_1 <- add_ferries(pref_1)
prefadj_1 <- geomander::add_edge(prefadj_1, ferries_1[, 1], ferries_1[, 2], zero = TRUE)

# check contiguity
suggest_1 <- geomander::suggest_component_connection(shp = pref_1, adj = prefadj_1)
prefadj_1 <- geomander::add_edge(prefadj_1, suggest_1$x,
                               suggest_1$y, zero = TRUE) # Fixing 壱岐市、対島市 isolation
prefadj_1 <- lapply(prefadj_1, unique)

pref_map_1 <- redist::redist_map(pref_1,
                               ndists = ndists_new,
                               pop_tol = 0.15,
                               total_pop = pop,
                               adj = prefadj_1)

sim_smc_pref_1 <- redist::redist_smc(pref_map_1,
                                   nsims = nsims,
                                   pop_temper = 0.05)

saveRDS(sim_smc_pref_1, paste("simulation/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            "_",
                            as.character(nsplits_1),
                            ".Rds",
                            sep = ""))

# --------- Two-Split ----------------#

nsplits_2 <- 2

# initialize pref_2 object
pref_2 <- pref

pref_2 <- merge_gun(pref_2, exception = 42383) # allowing 北松浦郡 split

# reflect old boundaries
pref_2 <- reflect_old_boundaries(pref_2, old_pref, pop_by_old_boundary, old_42201, 42201)
pref_2 <- reflect_old_boundaries(pref_2, old_pref, pop_by_old_boundary, old_42202, 42202)

# estimation of old-boundary level national populations
pref_2 <- estimate_old_boundary_pop(old_42201, 42201, pref_2, census2020)
pref_2 <- estimate_old_boundary_pop(old_42202, 42202, pref_2, census2020)

# simulation parameters
prefadj_2 <- redist::redist.adjacency(shp = pref_2) # Adjacency list

# add ferries
ferries_2 <- add_ferries(pref_2)
prefadj_2 <- geomander::add_edge(prefadj_2, ferries_2[, 1], ferries_2[, 2], zero = TRUE)

# check contiguity
suggest_2 <- geomander::suggest_component_connection(shp = pref_2, adj = prefadj_2)
prefadj_2 <- geomander::add_edge(prefadj_2, suggest_2$x,
                                 suggest_2$y, zero = TRUE) # Fixing 壱岐市、対島市 isolation
prefadj_2 <- lapply(prefadj_2, unique)

pref_map_2 <- redist::redist_map(pref_2,
                                 ndists = ndists_new,
                                 pop_tol = 0.20,
                                 total_pop = pop,
                                 adj = prefadj_2)

sim_smc_pref_2 <- redist::redist_smc(pref_map_2,
                                     nsims = nsims,
                                     pop_temper = 0.05)


saveRDS(sim_smc_pref_2, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_",
                              as.character(nsplits_2),
                              ".Rds",
                              sep = ""))

# ------ Analysis ------ -#

# import as files, if necessary
sim_smc_pref_0 <- readRDS("simulation/42_nagasaki_smc_25000_0.Rds")
sim_smc_pref_1 <- readRDS("simulation/42_nagasaki_smc_25000_1.Rds")
sim_smc_pref_2 <- readRDS("simulation/42_nagasaki_smc_25000_2.Rds")

# extract plans
pref_smc_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)
pref_smc_plans_1 <- redist::get_plans_matrix(sim_smc_pref_1)
pref_smc_plans_2 <- redist::get_plans_matrix(sim_smc_pref_2)

# get disparity table data
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1)
wgt_smc_2 <- simulation_weight_disparity_table(sim_smc_pref_2)

# Cooccurence analysis
status_quo <- status_quo_match(pref_2)

# establish keys to map 0-split, 1-split plans to 2-split plans
key <- vector(length = length(pref_2$code))

for (i in 1:length(pref_2$code)) {
  if (pref_2$code[i] %in% old_42201) {key[i] <- 42201}
  else if (pref_2$code[i] %in% old_42202) {key[i] <- 42202}
  else {key[i] <- pref_2$code[i]}
}

# map 0-split plans to 2-split plans
modified_smc_0 <- matrix(0, nrow = dim(pref_smc_plans_2)[1],
                         ncol = dim(pref_smc_plans_0)[2])

for (i in 1:dim(pref_smc_plans_2)[1]) {
  if (pref_2$code[i] %in% pref_0$code) {modified_smc_0[i, ] <-
    pref_smc_plans_0[which(pref_0$code == pref_2$code[i]), ]}
  else {modified_smc_0[i, ] <- pref_smc_plans_0[which(pref_0$code == key[i]), ]}
}

# map 1-split plans to 2-split plans
modified_smc_1 <- matrix(0, nrow = dim(pref_smc_plans_2)[1],
                         ncol = dim(pref_smc_plans_1)[2])

for (i in 1:dim(pref_smc_plans_2)[1]) {
  if (pref_2$code[i] %in% pref_1$code) {modified_smc_1[i, ] <-
    pref_smc_plans_1[which(pref_1$code == pref_2$code[i]), ]}
  else {modified_smc_1[i, ] <- pref_smc_plans_1[which(pref_1$code == key[i]), ]}
}

overlap_smc_0 <- vector(length = dim(pref_smc_plans_0)[2])
overlap_smc_1 <- vector(length = dim(pref_smc_plans_1)[2])
overlap_smc_2 <- vector(length = dim(pref_smc_plans_2)[2])

for (i in 1:length(overlap_smc_0)){
  overlap_smc_0[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_smc_0[, i], pref_2$pop,
                                                       weighting = "s", index_only = TRUE)
}
for (i in 1:length(overlap_smc_1)){
  overlap_smc_1[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_smc_1[, i], pref_2$pop,
                                                       weighting = "s", index_only = TRUE)
}
for (i in 1:length(overlap_smc_2)){
  overlap_smc_2[i] <- redist::redist.prec.pop.overlap(status_quo$ku, pref_smc_plans_2[, i], pref_2$pop,
                                                       weighting = "s", index_only = TRUE)
}

wgt_orig <- simulation_weight_disparity_table(redist::redist_plans(plans = matrix(status_quo$ku, ncol = 1), map = pref_map_2, algorithm = "smc"))

# set parameters

improved_plans <- as.data.frame(
  cbind(rbind(wgt_smc_1 %>% dplyr::filter(LH < wgt_orig$LH),
              wgt_smc_0 %>% dplyr::filter(LH < wgt_orig$LH)
  ),

  c(overlap_smc_1[which(wgt_smc_1$LH < wgt_orig$LH)],
    overlap_smc_0[which(wgt_smc_0$LH < wgt_orig$LH)]
  ),

  as.character(count_splits(modified_smc_1[, which(wgt_smc_1$LH < wgt_orig$LH)], key),
               count_splits(modified_smc_0[, which(wgt_smc_0$LH < wgt_orig$LH)], key)
  )))

names(improved_plans) <- c(names(wgt_smc_0), "Dissimilarity", "Splits")

plot_smc <- ggplot(improved_plans, aes(Dissimilarity, LH, colour = Splits)) +
  geom_point(size = 1, alpha = 0.3)
ggMarginal(plot_smc, groupColour = TRUE, groupFill = TRUE)


