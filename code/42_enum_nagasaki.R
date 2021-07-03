# ----------- set up -------------#
library(tidyverse)
library(ggExtra)
library(ggplot2)

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
sim_type_0 <- "enum"
nsims_0 <- 5000
nsplits_0 <- 0

# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4

pref_raw <- download_shp(pref_code)

pref <- clean_jcdf(pref_raw = pref_raw)

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

# 2020 census
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")

# clean it
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# download historical boundary data
old_pref <- download_old_shp(pref_code)

# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

# split Nagasaki-shi
old_42201 <- pop_by_old_boundary %>% dplyr::filter(X == 42201 & X.3 == 2000 & X.1 == 9, )
old_42201 <- old_42201[, 7]
old_42201 <- 1000 * pref_code + readr::parse_number(old_42201)

# split Sasebo-shi
old_42202 <- pop_by_old_boundary %>% dplyr::filter(X == 42202 & X.3 == 2000 & X.1 == 9, )
old_42202 <- old_42202[, 7]
old_42202 <- 1000 * pref_code + readr::parse_number(old_42202)


# -------- Zero-Split ------------#

pref_0 <- pref

# merge guns, exception of 北松浦郡
pref_0 <- merge_gun(pref_0, exception = 42383)

row.names(pref_0) <- NULL

# simulation parameters
prefadj_0 <- redist::redist.adjacency(shp = pref_0) # Adjacency list

# add ferries
ferries_0 <- add_ferries(pref_0)
prefadj_0 <- geomander::add_edge(prefadj_0, ferries_0[, 1], ferries_0[, 2], zero = TRUE)

# check contiguity
suggest_0 <- geomander::suggest_component_connection(shp = pref_0, adj = prefadj_0)

prefadj_0 <- geomander::add_edge(prefadj_0, suggest_0$x,
                               suggest_0$y, zero = TRUE) # Fixing 壱岐市、対島市 isolation

pref_map_0 <- redist::redist_map(pref_0,
                               ndists = ndists_new,
                               pop_tol= 0.20,
                               total_pop = pop,
                               adj = prefadj_0)

makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

# small map -- enumerate, don't sample!

if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

enum_plans_0 <- redist::redist.enumpart(adj = prefadj_0,
                             unordered_path = here::here('simulation/unord_nagasaki_0'),
                             ordered_path = here::here('simulation/ord_nagasaki_0'),
                             out_path = here::here('simulation/enum_nagasaki_0'),
                             ndists = ndists_new, all = TRUE,
                             total_pop = pref_map_0[[attr(pref_map_0, 'pop_col')]])

plans_0 <- redist::redist_plans(enum_plans_0$plans, pref_map_0, algorithm = 'enumpart')

# get disparity data
wgt_enum_0 <- simulation_weight_disparity_table(plans_0)


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

plans_1 <- redist::redist_plans(enum_plans_1$plans, pref_map_1, algorithm = 'enumpart')

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

prefadj_2 <- lapply(prefadj_2, unique)

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

plans_2 <- redist::redist_plans(enum_plans_2$plans, pref_map_2, algorithm = 'enumpart')

wgt_enum_2 <- simulation_weight_disparity_table(plans_2)

# ------ Analysis ------- #

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
modified_enum_0 <- matrix(0, nrow = dim(enum_plans_2$plans)[1], ncol = dim(enum_plans_0$plans)[2])

for (i in 1:dim(enum_plans_2$plans)[1]) {
  if (pref_2$code[i] %in% pref_0$code) {modified_enum_0[i, ] <-
    enum_plans_0$plans[which(pref_0$code == pref_2$code[i]), ]}
  else {modified_enum_0[i, ] <- enum_plans_0$plans[which(pref_0$code == key[i]), ]}
}

# map 1-split plans to 2-split plans
modified_enum_1 <- matrix(0, nrow = dim(enum_plans_2$plans)[1], ncol = dim(enum_plans_1$plans)[2])

for (i in 1:dim(enum_plans_2$plans)[1]) {
  if (pref_2$code[i] %in% pref_1$code) {modified_enum_1[i, ] <-
    enum_plans_1$plans[which(pref_1$code == pref_2$code[i]), ]}
  else {modified_enum_1[i, ] <- enum_plans_1$plans[which(pref_1$code == key[i]), ]}
}

overlap_enum_0 <- vector(length = dim(enum_plans_0$plans)[2])
overlap_enum_1 <- vector(length = dim(enum_plans_1$plans)[2])
overlap_enum_2 <- vector(length = dim(enum_plans_2$plans)[2])

for (i in 1:length(overlap_enum_0)){
  overlap_enum_0[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_enum_0[, i], pref_2$pop,
                                                       weighting = "s", index_only = TRUE)
}
for (i in 1:length(overlap_enum_1)){
  overlap_enum_1[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_enum_1[, i], pref_2$pop,
                                                       weighting = "s", index_only = TRUE)
}
for (i in 1:length(overlap_enum_2)){
  overlap_enum_2[i] <- redist::redist.prec.pop.overlap(status_quo$ku, enum_plans_2$plans[, i], pref_2$pop,
                                                  weighting = "s", index_only = TRUE)
}

wgt_orig <- simulation_weight_disparity_table(redist::redist_plans(plans = matrix(status_quo$ku, ncol = 1), map = pref_map_2, algorithm = "smc"))

# set parameters
improved_plans <- as.data.frame(
  cbind(rbind(wgt_enum_1 %>% dplyr::filter(LH < wgt_orig$LH),
              wgt_enum_0 %>% dplyr::filter(LH < wgt_orig$LH)
            ),

      c(overlap_enum_1[which(wgt_enum_1$LH < wgt_orig$LH)],
        overlap_enum_0[which(wgt_enum_0$LH < wgt_orig$LH)]
            ),

      c(rep("1", length(overlap_enum_1[which(wgt_enum_1$LH < wgt_orig$LH)])),
            rep("0", length(overlap_enum_0[which(wgt_enum_0$LH < wgt_orig$LH)]))
        )))


names(improved_plans) <- c(names(wgt_enum_0), "Dissimilarity", "Splits")

plot_enum <- ggplot(improved_plans, aes(Dissimilarity, LH, colour = Splits)) +
  geom_point(size = 1) +
  scale_alpha_manual(guide = 'none', values = list(a = 0.2, point = 1))

ggMarginal(plot_enum, groupColour = TRUE, groupFill = TRUE)

