############# manual entry ###############
#-------- Set-up -----------#
# prefectural information
sim_type <- "enum"
pref_code <- 25
pref_name <- "shiga"
lakes_removed <- c("琵琶湖") # enter `c()` if not applicable
merge_gun_exception <- c()  # enter `c()` if not applicable
gov_designated_city <- c() # enter `c()`if not applicable
# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4

#------- Specify municipality splits -------------#
# 0 splits
nsplits_n <- 0
split_codes_n <- c()  # enter `c()` if not applicable
intact_codes_n <- c()  # enter `c()` if not applicable

# 1 splits
nsplits_1 <- 1 # enter NULL if there is no more than this splits
split_codes_1 <- c() # enter `c()` if not applicable
intact_codes_1 <- c() # enter `c()` if not applicable

# 2 splits
nsplits_2 <- NULL # enter NULL if there is no more than this splits
split_codes_2 <- c() # enter `c()` if not applicable
intact_codes_2 <- c() # enter `c()` if not applicable

# 3 splits
nsplits_3 <- NULL # enter NULL if there is no more than this splits
split_codes_3 <- c() # enter `c()` if not applicable
intact_codes_3 <- c() # enter `c()` if not applicable

# 4 splits
nsplits_4 <- NULL # enter NULL if there is no more than this splits
split_codes_4 <- c() # enter `c()` if not applicable
intact_codes_4 <- c() # enter `c()` if not applicable

# 5 splits
nsplits_5 <- NULL # enter NULL if there is no more than this splits
split_codes_5 <- c() # enter `c()` if not applicable
intact_codes_5 <- c() # enter `c()` if not applicable

################ automated process ##################
# the following is the uniformed process to automate

############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)
#remotes::install_github("alarm-redist/redist@dev")

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#------------- get and clean census data --------
# download census shp
pref_raw <- download_shp(pref_code)
# clean shp
pref <- clean_jcdf(pref_raw = pref_raw)

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# combining those two data
pref <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

# remove lake
if(is_null(lakes_removed)){
  pref <- pref
} else {
  pref <- remove_lake(pref, lakes_removed)
}

# check map
pref %>%
  ggplot() +
  geom_sf(fill = "red")

# download historical boundary data
old_pref <- download_old_shp(pref_code)

# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

#---------------- split and merge --------------
# merge small
if(is_null(split_codes_n)){
  pref_n <- pref
} else {
  dem_pops <- download_pop_demographics(pref_code) #first download data
  pref_n <- pref %>%
    calc_kokumin(pref = ., dem_pops) %>%
    merge_small(pref = .,
                split_codes = split_codes_n,
                intact_codes = intact_codes_n) %>%
    estimate_2020_pop(., census2020)
}

# merge gun
if(is_null(merge_gun_exception)){
  pref_n <- merge_gun(pref = pref_n)
} else {
  pref_n <- merge_gun(pref = pref_n,
            exception = merge_gun_exception)
}

#------------- set up map ----------------
# simulation parameters
prefadj_n <- redist::redist.adjacency(shp = pref_n) # Adjacency list

# add ferries
# ignore errors if there is no ferry
ferries_n <- add_ferries(pref_n)
prefadj_n <- geomander::add_edge(prefadj_n, ferries_n[, 1], ferries_n[, 2], zero = TRUE)
# check contiguity
suggest_n <-  geomander::suggest_component_connection(shp = pref_n, adj = prefadj_n)
prefadj_n <- geomander::add_edge(prefadj_n,
                                 suggest_n$x,
                                 suggest_n$y,
                                 zero = TRUE)

# define map
pref_map_n <- redist::redist_map(pref_n,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_n)

############# simulation with enumeration ############

# mechanical set up
makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

# set inital path
if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

enum_plans_n <- redist::redist.enumpart(adj = prefadj_n,
                             unordered_path = here::here('data/unord'),
                             ordered_path = here::here('data/ord'),
                             out_path = here::here('data/enum'),
                             ndists = ndists_new,
                             all = TRUE,
                             total_pop = pref_map_n[[attr(pref_map_n, 'pop_col')]])

good_plans_n <- enum_plans_n[[1]][, enum_plans_n[[2]] < redist::get_pop_tol(pref_map_n)]

plans_n <- redist::redist_plans(good_plans_n,
                                pref_map_n,
                                algorithm = 'enumpart')

# get disparity data
wgt_enum_n <- simulation_maxmin_LH_table(plans_n)

# get plans
pref_enum_plans_n <- redist::get_plans_matrix(sim_smc_pref_n)

# --------- One-Split ----------------#

sim_type_1 <- "smc"
nsplits_1 <- 1
nsims_1 <- 25000

# initialize pref_1 object
pref_1 <- pref

# merge guns, exception of 北松浦郡
pref_1 <- merge_gun(pref_1, exception = 42383)

# reflect old boundaries
pref_1 <- reflect_old_boundaries(pref_1, old_pref, pop_by_old_boundary, old_42201, 42201)

# estimation of old-boundary level national populations
nat_2020_42201 <- (census2020 %>% dplyr::filter(code == 42201, ))$pop_national
pop_2015_42201 <- sum(pref_1[which(pref_1$code %in% old_42201), ]$pop)

for (i in 1:length(old_42201)) {

  pref_1[which(pref_1$code == old_42201[i]), ]$pop <-
    round(pref_1[which(pref_1$code == old_42201[i]), ]$pop / pop_2015_42201 * nat_2020_42201)

}

# make geometry valid
pref_1 <- sf::st_make_valid(pref_1)

row.names(pref_1) <- NULL

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

makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

enum_plans_1 <- redist::redist.enumpart(adj = prefadj_1,
                                        unordered_path = here::here('simulation/unord_nagasaki_1'),
                                        ordered_path = here::here('simulation/ord_nagasaki_1'),
                                        out_path = here::here('simulation/enum_nagasaki_1'),
                                        ndists = ndists_new, all = TRUE,
                                        total_pop = pref_map_1[[attr(pref_map_1, 'pop_col')]])

good_plans_1 <- enum_plans_1[[1]][, enum_plans_1[[2]] < redist::get_pop_tol(pref_map_1)]

plans_1 <- redist::redist_plans(good_plans_1, pref_map_1, algorithm = 'enumpart')

# get disparity data
wgt_enum_1 <- simulation_weight_disparity_table(plans_1)

# --------- Two-Split ----------------#

sim_type_2 <- "smc"
nsplits_2 <- 2
nsims_2 <- 25000

# initialize pref_2 object
pref_2 <- pref

pref_2 <- merge_gun(pref_2, exception = 42383) # allowing 北松浦郡 split

# reflect old boundaries
pref_2 <- reflect_old_boundaries(pref_2, old_pref, pop_by_old_boundary, old_42201, 42201)
pref_2 <- reflect_old_boundaries(pref_2, old_pref, pop_by_old_boundary, old_42202, 42202)

# estimation of old-boundary level national populations
nat_2020_42201 <- (census2020 %>% dplyr::filter(code == 42201, ))$pop_national
pop_2015_42201 <- sum(pref_2[which(pref_2$code %in% old_42201), ]$pop)

nat_2020_42202 <- (census2020 %>% dplyr::filter(code == 42202, ))$pop_national
pop_2015_42202 <- sum(pref_2[which(pref_2$code %in% old_42202), ]$pop)

for (i in 1:length(old_42201)) {

  pref_2[which(pref_2$code == old_42201[i]), ]$pop <-
    round(pref_2[which(pref_2$code == old_42201[i]), ]$pop / pop_2015_42201 * nat_2020_42201)

}

for (i in 1:length(old_42202)) {

  pref_2[which(pref_2$code == old_42202[i]), ]$pop <-
    round(pref_2[which(pref_2$code == old_42202[i]), ]$pop / pop_2015_42202 * nat_2020_42202)

}

# make geometry valid
pref_2 <- sf::st_make_valid(pref_2)

row.names(pref_2) <- NULL

# simulation parameters
prefadj_2 <- redist::redist.adjacency(shp = pref_2) # Adjacency list

# add ferries
ferries_2 <- add_ferries(pref_2)
prefadj_2 <- geomander::add_edge(prefadj_2, ferries_2[, 1], ferries_2[, 2], zero = TRUE)

# check contiguity
suggest_2 <- geomander::suggest_component_connection(shp = pref_2, adj = prefadj_2)

prefadj_2 <- geomander::add_edge(prefadj_2, suggest_2$x,
                                 suggest_2$y, zero = TRUE) # Fixing 壱岐市、対島市 isolation

pref_map_2 <- redist::redist_map(pref_2,
                                 ndists = ndists_new,
                                 pop_tol = 0.20,
                                 total_pop = pop,
                                 adj = prefadj_2)

makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

enum_plans_2 <- redist::redist.enumpart(adj = prefadj_2,
                                        unordered_path = here::here('simulation/unord_nagasaki_2'),
                                        ordered_path = here::here('simulation/ord_nagasaki_2'),
                                        out_path = here::here('simulation/enum_nagasaki_2'),
                                        ndists = ndists_new, all = TRUE,
                                        total_pop = pref_map_2[[attr(pref_map_2, 'pop_col')]])

good_plans_2 <- enum_plans_2[[1]][, enum_plans_2[[2]] < redist::get_pop_tol(pref_map_2)]

plans_2 <- redist::redist_plans(good_plans_2, pref_map_2, algorithm = 'enumpart')

# ------ Analysis ------ -#

sim_smc_pref_0 <- readRDS("simulation/42_nagasaki_smc_5000_0.Rds")
pref_ms_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)

sim_smc_pref_1 <- readRDS("simulation/42_nagasaki_smc_25000_1.Rds")
pref_ms_plans_1 <- redist::get_plans_matrix(sim_smc_pref_1)

sim_smc_pref_2 <- readRDS("simulation/42_nagasaki_smc_25000_2.Rds")
pref_ms_plans_2 <- redist::get_plans_matrix(sim_smc_pref_2)

wgt_tbl <- simulation_weight_disparity_table(sim_smc_pref_0)
wgt_tbl_1 <- simulation_weight_disparity_table(sim_smc_pref_1)
wgt_tbl_2 <- simulation_weight_disparity_table(sim_smc_pref_2)

# Cooccurence analysis

status_quo <- status_quo_match(pref_2)

overlap_2 <- vector(length = nsims_2)

for (i in 1:nsims_2){
  overlap_2[i] <- redist::redist.prec.pop.overlap(status_quo$ku, pref_ms_plans_2[, i], pref_2$pop,
                                                  weighting = "s", index_only = TRUE)
}

plot(overlap_2, wgt_tbl_2$LH, xlab = "Dissimilarity", ylab = "Loosemore-Hanby", pch=18)

good_plans <- which(overlap_2 < 0.009 & wgt_tbl_2$max_to_min < 1.03, )

redist::redist.plot.plans(sim_smc_pref_2,
                          draws = good_plans,
                          geom = pref_map_2)
