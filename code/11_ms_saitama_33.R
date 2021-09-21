############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)
#remotes::install_github("alarm-redist/redist@dev")
#install.packages("redist")
library(redist)
# pull functions from jcdf
# set working directory to the function folder
setwd("./R")
files.sources <-  list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- information set-up -----------#
# prefectural information
sim_type <- "ms"
nsims <- 500000
pref_code <- 11
pref_name <- "saitama"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 16
ndists_old <- 15
sq_maxmin <- 1.444
#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
# Iruma and , 11326, 11327
merge_gun_exception <- c(11324)  # enter `c()` if not applicable

######### Download and Clean Census ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code) #first download data

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")

# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

#clean pref_raw
pref <- pref_raw %>%
  clean_jcdf()
pref <- pref %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
#Estimate 2020 pop.
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

###############Data cleaning (Freeze small municipalities)###################
#---------0 split data----------------#
#check 0 split data (match with 2020 census data)
pref_0 <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)
#treat gun as one municipality
pref_0 <- merge_gun(pref_0,
                    exception = merge_gun_exception)
ranking <- pref_0 %>%
  dplyr::arrange(desc(pop))
#select municipalities whose population is less than 33% of target pop -> keep intact
pref_small <- pref_0 %>%
  dplyr::filter(pop < 0.33 * sum(pref_0$pop)/ndists_new)
pref_small$subcode <- "0000"
#select municipalities whose population is more than 33% of target pop -> separate
large_codes <- pref_0$code[which(pref_0$code %in% pref_small$code == FALSE)]
pref_large <- pref %>%
  dplyr::filter(code %in% large_codes)
#bind together
pref_33 <- dplyr::bind_rows(pref_small, pref_large)

############Simulation Prep########################
#adjacency list
prefadj <- redist::redist.adjacency(pref_33)

neighbor <- geomander::suggest_neighbors(shp = pref_33,
                                         adjacency = prefadj)
if(nrow(neighbor) > 0){
  prefadj <- geomander::add_edge(prefadj,
                                 neighbor$x,
                                 neighbor$y,
                                 zero = TRUE)
}

pref_map <- redist::redist_map(pref_33,
                               ndists = round(sum(pref_33$pop)/(sum(pref_33$pop)/ndists_new)),
                               pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                               total_pop = pop,
                               adj = prefadj)

pref_ms <- redist::redist_mergesplit(
  map = pref_map,
  nsims = nsims,
  counties = pref_33$code,
  warmup = 0,
  constraints = list(multissplits = list(strength = 30),
                     splits = list(strength = 14))
)

i <- 33
# save it
saveRDS(pref_ms, paste("simulation/",
                       sprintf("%02d", pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(sim_type),
                       "_",
                       as.character(nsims),
                       "_",
                       i,
                       "percent",
                       "_",
                       "pref_ms.Rds",
                       sep = ""))


# get disparity data
weight_pref <- simulation_weight_disparity_table(pref_ms)
plans_pref <- redist::get_plans_matrix(pref_ms)

# get splits
pref_ms_splits <- count_splits(plans_pref, pref_map$code)
pref_ms_countiessplit <- redist::redist.splits(plans_pref, pref_map$code)

pref_ms_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref)))
pref_ms_results$max_to_min <- weight_pref$max_to_min
pref_ms_results$splits <- pref_ms_splits
pref_ms_results$counties_split <- pref_ms_countiessplit
pref_ms_results$draw <- weight_pref$draw

pref_ms_results <- pref_ms_results %>%
  dplyr::group_by(max_to_min, splits, counties_split) %>%
  dplyr::summarise(draw = first(draw)) %>%
  dplyr::arrange(splits)

min(pref_ms_results$max_to_min[which(pref_ms_results$splits == pref_ms_results$counties_split)])

satisfying_plan <- pref_ms_results %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == counties_split) %>%
  dplyr::arrange(max_to_min)

###### Draw Map#########
pref_boundaries <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
# (481298) -> 1.20 max_min
optimal_matrix_plan <- redist::get_plans_matrix(pref_ms %>%
                                                  filter(draw == satisfying_plan$draw[1]))
colnames(optimal_matrix_plan) <- "district"
optimal_boundary <- cbind(pref_33, as_tibble(optimal_matrix_plan))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary, aes(fill = factor(district))) +
  geom_sf(data = pref_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")+
  scale_fill_manual(values=as.vector(pals::polychrome(ndists_new)))+
  ggtitle(paste("Mergesplit #", satisfying_plan$draw[1]," max/min=", satisfying_plan$max_to_min[1], "splits=", satisfying_plan$splits[1]))

########### Shortburst ###########
pref_sb <- redist::redist_shortburst(
  map = pref_map,
  score_fn = redist::scorer_multisplits(map = pref_map, counties = pref_map$code) +
    redist::scorer_pop_dev(pref_map) * redist::scorer_splits(pref_map, pref_map$code),
  maximize = FALSE,
  burst_size = 1000,
  max_bursts = 10000,
  counties = pref_33$code,
  init_plan = "sample"
  )

# get disparity data
weight_pref_sb <- simulation_weight_disparity_table(pref_sb)
plans_pref_sb <- redist::get_plans_matrix(pref_sb)

# get splits
pref_sb_splits <- count_splits(plans_pref_sb, pref_map$code)
pref_sb_countiessplit <- redist::redist.splits(plans_pref_sb, pref_map$code)

pref_sb_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref_sb)))
pref_sb_results$max_to_min <- weight_pref_sb$max_to_min
pref_sb_results$splits <- pref_sb_splits
pref_sb_results$counties_split <- pref_sb_countiessplit
pref_sb_results$draw <- weight_pref_sb$draw

pref_sb_results <- pref_sb_results %>%
  dplyr::group_by(max_to_min, splits, counties_split) %>%
  dplyr::summarise(draw = first(draw)) %>%
  dplyr::arrange(splits)

min(pref_sb_results$max_to_min[which(pref_sb_results$splits == pref_sb_results$counties_split)])

satisfying_plan_sb <- pref_sb_results %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == counties_split) %>%
  dplyr::arrange(max_to_min)

###### Draw Map#########
# (694) -> 1.22 max_min
optimal_matrix_plan_sb <- redist::get_plans_matrix(pref_sb %>%
                                                  filter(draw == satisfying_plan_sb$draw[1]))
colnames(optimal_matrix_plan_sb) <- "district"
optimal_boundary_sb <- cbind(pref_33, as_tibble(optimal_matrix_plan_sb))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary_sb, aes(fill = factor(district))) +
  geom_sf(data = pref_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")+
  scale_fill_manual(values=as.vector(pals::polychrome(ndists_new)))+
  ggtitle(paste("Shortburst #", satisfying_plan_sb$draw[1]," max/min=", satisfying_plan_sb$max_to_min[1], "splits=", satisfying_plan_sb$splits[1]))
