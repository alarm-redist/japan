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
nsims <- 250000
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

######### Set Population data frame in the smaller level ###########
#clean data and estimate population
pref_2020 <- pref_raw %>%
  # clean data frame
  clean_jcdf() %>%
  # calculate Japanese nationality
  calc_kokumin(dem_pops) %>%
  # estimate small area population
  estimate_2020_pop(census2020) %>%
  dplyr::select(code, pop_estimate, geometry) %>%
  dplyr::rename(pop = pop_estimate)


############## Set County Level Data frame ###################
# Group by municipalities (city and gun)
pref_county <- pref_2020 %>%
  merge_gun(., exception = merge_gun_exception) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Iruma-gun (11326 and 11327) except Miyoshi-cho (11324) based on current plan
iruma <- pref_county %>%
  dplyr::filter(code == 11326 |
                  code == 11327) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Chichibu City (11207) and Chichibu-gun (11360) based on Teiju-jiritsuken
chichibu <- pref_county %>%
  dplyr::filter(code == 11207 |
                  code == 11360) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Honjo City (11211) and Kodama-gun (11380) based on Teiju-jiritsuken
honjo <- pref_county %>%
  dplyr::filter(code == 11211 |
                  code == 11380) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Kasukabe City (11214) and KitaKatsushika-gun (11460)
kasukabe <- pref_county %>%
  dplyr::filter(code == 11214 |
                  code == 11460) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Higashi-matsuyama City (11212) and Hiki-gun (11340)
higashimatsuyama <- pref_county %>%
  dplyr::filter(code == 11212 |
                  code == 11340) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

pref_county_manually_edited <- pref_county %>%
  dplyr::filter(!code %in% c(11326,
                            11327,
                            11207,
                            11360,
                            11211,
                            11380,
                            11214,
                            11460,
                            11212,
                            11340)) %>%
  dplyr::bind_rows(.,
                   iruma,
                   chichibu,
                   honjo,
                   kasukabe,
                   higashimatsuyama) %>%
  dplyr::select(code, geometry)

########## Add `county` column to the `pref` data frame ###########
county <- geomander::geo_match(from = pref_2020,
                             to = pref_county_manually_edited,
                             method = "center",
                             tiebreaker = TRUE)

pref <- pref_2020 %>%
  dplyr::mutate(county = county)

pref_boundaries <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
############Simulation Prep########################
#adjacency list
prefadj <- redist::redist.adjacency(pref)

neighbor <- geomander::suggest_neighbors(shp = pref,
                                         adjacency = prefadj)
if(nrow(neighbor) > 0){
  prefadj <- geomander::add_edge(prefadj,
                                 neighbor$x,
                                 neighbor$y,
                                 zero = TRUE)
}

pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                               total_pop = pop,
                               adj = prefadj)




####### Mergesplit Simulation ######
sim_type <- "ms"

pref_ms <- redist::redist_mergesplit(
  map = pref_map,
  nsims = nsims,
  counties = pref$county,
  warmup = 0,
  constraints = list(multissplits = list(strength = 140),
                     splits = list(strength = 10))
)

# save it
saveRDS(pref_ms, paste("simulation/",
                       sprintf("%02d", pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(sim_type),
                       "_",
                       as.character(nsims),
                       ".Rds",
                       sep = ""))

# get disparity data
weight_pref_ms <- simulation_weight_disparity_table(pref_ms)
plans_pref_ms <- redist::get_plans_matrix(pref_ms)
redist_plans <- redist::redist_plans(plans = plans_pref_ms,
                                     map = pref_map,
                                     algorithm = "ms")

# get splits
pref_ms_splits <- count_splits(plans_pref_ms, pref_map$code)
pref_ms_codesplit <- redist::redist.splits(plans_pref_ms,
                                           pref_map$code)
pref_ms_countiessplit <- redist::redist.splits(plans_pref_ms,
                                               pref_map$county)

pref_ms_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref_ms)))
pref_ms_results$max_to_min <- weight_pref_ms$max_to_min
pref_ms_results$splits <- pref_ms_splits
pref_ms_results$code_split <- pref_ms_codesplit
pref_ms_results$counties_split <- pref_ms_countiessplit
pref_ms_results$draw <- weight_pref_ms$draw

pref_ms_results <- pref_ms_results %>%
  dplyr::group_by(max_to_min, splits, code_split) %>%
  dplyr::summarise(draw = first(draw)) %>%
  dplyr::arrange(splits)

min(pref_ms_results$max_to_min[which(pref_ms_results$splits == pref_ms_results$code_split)])

satisfying_plan_ms <- pref_ms_results %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == code_split) %>%
  dplyr::arrange(max_to_min)

# Draw Map

optimal_matrix_plan_ms <- redist::get_plans_matrix(pref_ms %>%
                                                     filter(draw == satisfying_plan_ms$draw[1]))
colnames(optimal_matrix_plan_ms) <- "district"
optimal_boundary_ms <- cbind(pref, as_tibble(optimal_matrix_plan_ms))

ggplot() +
  geom_sf(data = optimal_boundary_ms, aes(fill = factor(district))) +
  geom_sf(data = pref_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")+
  scale_fill_manual(values=as.vector(pals::polychrome(ndists_new)))+
  ggtitle(paste("mergeplit #", satisfying_plan_ms$draw[1]," max/min=", satisfying_plan_ms$max_to_min[1], "splits=", satisfying_plan_ms$splits[1]))


### SMC Simulation #####

sim_type <- "smc"

pref_smc <- redist::redist_smc(pref_map,
                              nsims = nsims,
                              counties = county
                              #,
                             # constraints = list(multisplits = list(strength = 1000)
                                                # ,splits = list(strength = 20)
                              #                  )
                              #pop_temper = 0.05
                              )

# save it
saveRDS(pref_smc, paste("simulation/",
                       sprintf("%02d", pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(sim_type),
                       "_",
                       as.character(nsims),
                       ".Rds",
                       sep = ""))

pref_smc <- readRDS(paste("simulation/",
                          sprintf("%02d", pref_code),
                          "_",
                          as.character(pref_name),
                          "_",
                          as.character(sim_type),
                          "_",
                          as.character(nsims),
                          ".Rds",
                          sep = ""))

# get disparity data
weight_pref_smc <- simulation_weight_disparity_table(pref_smc)
plans_pref_smc <- redist::get_plans_matrix(pref_smc)
redist_plans <- redist::redist_plans(plans = plans_pref_smc,
                                     map = pref_map,
                                     algorithm = "smc")

# get splits
pref_smc_splits <- count_splits(plans_pref_smc, pref_map$code)
pref_smc_codesplit <- redist::redist.splits(plans_pref_smc,
                                                pref_map$code)
pref_smc_countiessplit <- redist::redist.splits(plans_pref_smc,
                                                pref_map$county)

pref_smc_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref_smc)))
pref_smc_results$max_to_min <- weight_pref_smc$max_to_min
pref_smc_results$splits <- pref_smc_splits
pref_smc_results$code_split <- pref_smc_codesplit
pref_smc_results$counties_split <- pref_smc_countiessplit
pref_smc_results$draw <- weight_pref_smc$draw

pref_smc_results <- pref_smc_results %>%
  dplyr::group_by(max_to_min, splits, code_split) %>%
  dplyr::summarise(draw = first(draw)) %>%
  dplyr::arrange(splits)

min(pref_smc_results$max_to_min[which(pref_smc_results$splits == pref_smc_results$code_split)])

satisfying_plan_smc <- pref_smc_results %>%
 # dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == code_split) %>%
  dplyr::arrange(max_to_min)

###### Draw Map#########
# (694) -> 1.22 max_min
optimal_matrix_plan_smc <- redist::get_plans_matrix(pref_smc %>%
                                                     filter(draw == satisfying_plan_smc$draw[1]))
colnames(optimal_matrix_plan_smc) <- "district"
optimal_boundary_smc <- cbind(pref, as_tibble(optimal_matrix_plan_smc))

ggplot() +
  geom_sf(data = optimal_boundary_smc, aes(fill = factor(district))) +
  geom_sf(data = pref_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")+
  scale_fill_manual(values=as.vector(pals::polychrome(ndists_new)))+
  ggtitle(paste("SMC #", satisfying_plan_smc$draw[1]," max/min=", satisfying_plan_smc$max_to_min[1], "splits=", satisfying_plan_smc$splits[1]))

