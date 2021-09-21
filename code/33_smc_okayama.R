############# set up ###############
#-------------- functions set up ---------#
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

#-------- information set-up -----------#
# prefectural information
sim_type <- "smc"
nsims <- 25000
pref_code <- 33
pref_name <- "okayama"
# set number of district (check external information)
ndists_new <- 4
ndists_old <- 5


######### Download and Clean Census ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code) #first download data

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

# -------- Choose which city to split ------------#
census2020 %>%
  filter(code > 33000 & code < 34000) %>% #excluding 33100 Okayamashi
  arrange(desc(pop_national))
#33202 倉敷
#33101 岡山市北区
#33104 岡山市南区
#33102 岡山市中区-> no way to split; skip
#33203 津山
#33103 岡山市東区

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

old_33202 <- find_old_codes(33202, pop_by_old_boundary)
#倉敷市: 33202 33441 33503
old_33203 <- find_old_codes(33203, pop_by_old_boundary)
#津山市: 33203 33601 33605 33624 33664

##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#Ferries
#ferries_0 <- add_ferries(pref_0) #No ferry routes

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list

#save(list=ls(all=TRUE), file="32_smc_okayama_data_0split.Rdata")

pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.25,
                                 total_pop = pop,
                                 adj = prefadj_0)

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_0 <- redist::redist_smc(pref_map_0,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_0, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_0",
                              ".Rds",
                              sep = ""))

###############1 split##################
pref_1 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge gun (0 exceptions) ------------#
pref_1 <- merge_gun(pref_1)

# -------- Old boundary ------------#
pref_1 <- reflect_old_boundaries(pref_1,
                                 old_boundary = old_boundary,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_code = old_33202,
                                 #codes of municipalities that now belong to Kurashikishi
                                 new_code = 33202) #code of merged municipality (Kurashikishi)

#Estimate 2020 pop based on old boundary
pref_1 <- estimate_old_boundary_pop(old_33202, 33202, pref_1, census2020)

#No ferries

# -------- set up for simulation ------------#
# simulation parameters
prefadj_1 <- redist::redist.adjacency(pref_1) # Adjacency list

pref_map_1 <- redist::redist_map(pref_1,
                                 ndists = ndists_new,
                                 pop_tol= 0.25,
                                 total_pop = pop,
                                 adj = prefadj_1)

###save(list=ls(all=TRUE), file="33_smc_okayama_data_1split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_1 <- redist::redist_smc(pref_map_1,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_1, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_1",
                              ".Rds",
                              sep = ""))

###############2 splits#############
pref_2 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge gun (0 exceptions) ------------#
pref_2 <- merge_gun(pref_2)

# -------- Old boundary ------------#
pref_2 <- reflect_old_boundaries(pref_2,
                                 old_boundary = old_boundary,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_code = old_33202,
                                 #codes of municipalities that now belong to Kurashikishi
                                 new_code = 33202)
pref_2 <- estimate_old_boundary_pop(old_33202, 33202, pref_2, census2020)

##-------No standardized way to split 岡山市北区(33101)------------##
sf::st_crs(old_boundary) <- sf::st_crs(pref_2)
#area unaffected by split
post_gappei_except_for_designated_city_2 <- pref_2 %>%
  dplyr::filter(code != 33101) %>%
  dplyr::select(code, pop, geometry)

#extract the area that was later added to 岡山市北区 ("岡山市北区新地域")
added_municipalities_2 <- old_boundary %>%
  dplyr::filter(N03_007 %in% c(33301, 33303)) %>%   #301 御津町、303 建部町
  dplyr::select(N03_004, N03_007, geometry)
names(added_municipalities_2) <- c("municipality", "code", "geometry")
added_municipalities_2 <- sf::st_make_valid(added_municipalities_2)
added_municipalities_2 <- added_municipalities_2 %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry =  sf::st_union(geometry))

#"岡山市北区旧地域"
sq_kita <- pref_2 %>%
  dplyr::filter(code == 33101) %>%
  dplyr::select(geometry)
old_okayama <- old_boundary %>%
  dplyr::filter(N03_007 == 33201) %>%   #old okayama
  dplyr::select(geometry)
sq_kita <- sf::st_make_valid(sq_kita)
old_okayama <- sf::st_make_valid(old_okayama)
minus_added_municipalities_2 <- sf::st_intersection(sq_kita, old_okayama)

#clean 2015 pop data
cleaned_pop_by_old_boundary <- pop_by_old_boundary %>%
  slice(-c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
  select(X, X.1, X.4, X.5)
pop_data_2015 <- cleaned_pop_by_old_boundary %>%
  filter(X %in% 33100) %>%
  filter(X.1 %in% 9) %>%#"9" corresponds to the old municipalities (cf.shikibetsu code)
  select(X, X.4, X.5)
names(pop_data_2015) <- c("null_code", "municipality", "pop")
#obtain the last three digits of the old municipality codes
pop_data_2015 <- pop_data_2015 %>%
  separate(municipality, into = c("a", "old_code", "b"), sep = " ") %>%
  select(old_code, pop)
#generate functioning municipality codes
#(i.e. add the first two digits)
pref_10<- pref$code[1] %/% 1000 #prefecture_code
a <- lapply(pref_10,  paste0, pop_data_2015$old_code)
b <- as.data.frame(a, col.names = "code")
#final version of 2015(2020) population based on old boundaries
pop_data_2015 <- bind_cols(b, pop_data_2015)

#Obtain 2015 pop data in 301御津町、303建部
pop_data_2015_2 <- pop_data_2015 %>%
  dplyr::filter(code %in% c(33301, 33303)) %>%
  dplyr::select(code, pop)

#estimate population of Japanese nationals in 301御津町、303建部  for 2020
kita_total_2015 <- cleaned_pop_by_old_boundary %>%
  filter(X == 33101) %>%
  filter(X.1 == 0) %>%
  select(X.5)
kita_total_2015 <- as.numeric(kita_total_2015)
kita_nat_2020 <- (census2020 %>% dplyr::filter(code == 33101, ))$pop_national
#estimated 2020 pop of Japanese nationals in 301御津町、303建部
pop_data_2015_2$pop[which(pop_data_2015_2$code == 33301)] <-
   round(as.numeric(pop_data_2015_2$pop[which(pop_data_2015_2$code == 33301)] )*  kita_nat_2020 / kita_total_2015)
pop_data_2015_2$pop[which(pop_data_2015_2$code == 33303)] <-
  round(as.numeric(pop_data_2015_2$pop[which(pop_data_2015_2$code == 33303)] )*  kita_nat_2020 / kita_total_2015)

#merge pop data with geometry data
added_municipalities_2 <- merge(added_municipalities_2, pop_data_2015_2, by = "code")
added_municipalities_2$code <- as.numeric(added_municipalities_2$code)
added_municipalities_2$pop <- as.numeric(added_municipalities_2$pop)
#estimated 2020 pop of Japanese nationals in 岡山市北区新地域
minus_added_municipalities_2$pop <- kita_nat_2020 - sum(as.numeric(pop_data_2015_2$pop))
minus_added_municipalities_2$code <- 33101

#merge back together
pref_2 <- dplyr::bind_rows(post_gappei_except_for_designated_city_2, added_municipalities_2, minus_added_municipalities_2)

#No Ferries

# -------- set up for simulation ------------#
# simulation parameters
prefadj_2 <- redist::redist.adjacency(pref_2) # Adjacency list

pref_map_2 <- redist::redist_map(pref_2,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_2)

####save(list=ls(all=TRUE), file="33_smc_okayama_data_2splits.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_2 <- redist::redist_smc(pref_map_2,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_2, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_2",
                              ".Rds",
                              sep = ""))


###############3 splits#############
pref_3 <- pref_2

##-------No standardized way to split 岡山市南区(33104)------------##
sf::st_crs(old_boundary) <- sf::st_crs(pref_3)
#area unaffected by split
post_gappei_except_for_designated_city_3 <- pref_3 %>%
  dplyr::filter(code != 33104) %>%
  dplyr::select(code, pop, geometry)

#extract the area that was later added to 岡山市南区 ("岡山市南区新地域")
added_municipalities_3 <- old_boundary %>%
  dplyr::filter(N03_007 %in% 33401) %>%   #401 灘崎町
  dplyr::select(N03_004, N03_007, geometry)
names(added_municipalities_3) <- c("municipality", "code", "geometry")
added_municipalities_3 <- sf::st_make_valid(added_municipalities_3)
added_municipalities_3 <- added_municipalities_3 %>%
  dplyr::select(geometry, code)

#"岡山市南区旧地域"
sq_minami <- pref_3 %>%
  dplyr::filter(code == 33104) %>%
  dplyr::select(code, geometry) #南区 under the current plan
sq_minami <- sf::st_make_valid(sq_minami)
old_okayama$code <- 33104
old_okayama <- sf::st_make_valid(old_okayama) #岡山市 as of 2000
minus_added_municipalities_3 <- sf::st_intersection(sq_minami, old_okayama)

#Obtain 2015 pop data in 401 灘崎町
pop_data_2015_3 <- pop_data_2015 %>%
  dplyr::filter(code %in% 33401) %>%
  dplyr::select(code, pop)

#estimate population of Japanese nationals in 401 灘崎町  for 2020
minami_total_2015 <- cleaned_pop_by_old_boundary %>%
  filter(X == 33104) %>%
  filter(X.1 == 0) %>%
  select(X.5)
minami_total_2015 <- as.numeric(minami_total_2015)
minami_nat_2020 <- (census2020 %>% dplyr::filter(code == 33104, ))$pop_national
#estimated 2020 pop of Japanese nationals in 401 灘崎町
pop_data_2015_3$pop[which(pop_data_2015_3$code == 33401)] <-
  round(as.numeric(pop_data_2015_3$pop[which(pop_data_2015_3$code == 33401)] )*  minami_nat_2020 / minami_total_2015)

#merge pop data with geometry data
added_municipalities_3$pop <- as.numeric(pop_data_2015_3$pop)
added_municipalities_3$code <- as.numeric(added_municipalities_3$code)
#estimated 2020 pop of Japanese nationals in 岡山市南区旧地域
minus_added_municipalities_3$pop <- as.numeric(minami_nat_2020 - sum(as.numeric(pop_data_2015_3$pop)))
minus_added_municipalities_3 <- minus_added_municipalities_3 %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry), pop = pop)
minus_added_municipalities_3 <- minus_added_municipalities_3[!duplicated(minus_added_municipalities_3$code), ]

#merge back together
pref_3 <- dplyr::bind_rows(post_gappei_except_for_designated_city_3, added_municipalities_3, minus_added_municipalities_3) %>%
  dplyr::select(code, pop, geometry) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry), pop = pop)

#No Ferries

# -------- set up for simulation ------------#
# simulation parameters
prefadj_3 <- redist::redist.adjacency(pref_3) # Adjacency list

pref_map_3 <- redist::redist_map(pref_3,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_3)


####save(list=ls(all=TRUE), file="33_smc_okayama_data_3splits.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_3 <- redist::redist_smc(pref_map_3,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_3, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_3",
                              ".Rds",
                              sep = ""))


###############4 splits#############
pref_4 <- pref_3

# -------- Old boundary ------------#
pref_4 <- reflect_old_boundaries(pref_4,
                                 old_boundary = old_boundary,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_code = old_33203,
                                 #codes of municipalities that now belong to Tsuyamashi
                                 new_code = 33203)
pref_4 <- estimate_old_boundary_pop(old_33203, 33203, pref_4, census2020)


#No ferries

# -------- set up for simulation ------------#
# simulation parameters
prefadj_4 <- redist::redist.adjacency(pref_4) # Adjacency list

pref_map_4 <- redist::redist_map(pref_4,
                                 ndists = ndists_new,
                                 pop_tol= 0.15,
                                 total_pop = pop,
                                 adj = prefadj_4)

###save(list=ls(all=TRUE), file="34_smc_hiroshima_data_4splits.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_4 <- redist::redist_smc(pref_map_4,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_4, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_4",
                              ".Rds",
                              sep = ""))

###############5 splits#############
pref_5 <- pref_4

##-------No standardized way to split 岡山市東区(33103)------------##
sf::st_crs(old_boundary) <- sf::st_crs(pref_5)
#area unaffected by split
post_gappei_except_for_designated_city_5 <- pref_5 %>%
  dplyr::filter(code != 33103) %>%
  dplyr::select(code, pop, geometry)

#extract the area that was later added to 岡山市東区 ("岡山市東区新地域")
added_municipalities_5 <- old_boundary %>%
  dplyr::filter(N03_007 == 33321) %>%   #321 瀬戸町
  dplyr::select(N03_004, N03_007, geometry)
names(added_municipalities_5) <- c("municipality", "code", "geometry")
added_municipalities_5 <- sf::st_make_valid(added_municipalities_5)
added_municipalities_5 <- added_municipalities_5 %>%
  dplyr::select(code, geometry)

#"岡山市東区旧地域"
sq_higashi <- pref_5 %>%
  dplyr::filter(code == 33103) %>%
  dplyr::select(geometry) #東区 status quo
old_okayama <- old_boundary %>%
  dplyr::filter(N03_007 == 33201) %>%   #旧岡山市
  dplyr::select(geometry)
sq_higashi <- sf::st_make_valid(sq_higashi)
old_okayama <- sf::st_make_valid(old_okayama)
minus_added_municipalities_5 <- sf::st_intersection(sq_higashi, old_okayama)

#Obtain 2015 pop data in 321 瀬戸町
pop_data_2015_5 <- pop_data_2015 %>%
  dplyr::filter(code ==33321) %>%
  dplyr::select(code, pop)

#estimate population of Japanese nationals in 321 瀬戸町  for 2020
higashi_total_2015 <- cleaned_pop_by_old_boundary %>%
  filter(X == 33103) %>%
  filter(X.1 == 0) %>%
  select(X.5)
higashi_total_2015 <- as.numeric(higashi_total_2015)
higashi_nat_2020 <- (census2020 %>% dplyr::filter(code == 33103, ))$pop_national
#estimated 2020 pop of Japanese nationals in  321 瀬戸町
pop_data_2015_5$pop[which(pop_data_2015_5$code %in% c(33321))] <-
  round(as.numeric(pop_data_2015_5$pop[which(pop_data_2015_5$code == 33321)] )*  higashi_nat_2020 / higashi_total_2015)

#merge pop data with geometry data
added_municipalities_5 <- merge(added_municipalities_5, pop_data_2015_5, by = "code")
added_municipalities_5$code <- as.numeric(added_municipalities_5$code)
added_municipalities_5$pop <- as.numeric(added_municipalities_5$pop)
#estimated 2020 pop of Japanese nationals in 岡山市東区旧地域
minus_added_municipalities_5$pop <- higashi_nat_2020 - sum(as.numeric(pop_data_2015_5$pop))
minus_added_municipalities_5$code <- 33103
minus_added_municipalities_5 <- minus_added_municipalities_5 %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry), pop = pop)
minus_added_municipalities_5 <- minus_added_municipalities_5[!duplicated(minus_added_municipalities_5$code), ]

#merge back together
pref_5 <- dplyr::bind_rows(post_gappei_except_for_designated_city_5, added_municipalities_5, minus_added_municipalities_5)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_5 <- redist::redist.adjacency(pref_5) # Adjacencies

pref_map_5 <- redist::redist_map(pref_5,
                                 ndists = ndists_new,
                                 pop_tol= 0.15,
                                 total_pop = pop,
                                 adj = prefadj_5)

###save(list=ls(all=TRUE), file="33_smc_okayama_data_5splits.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_5 <- redist::redist_smc(pref_map_5,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_5, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_5",
                              ".Rds",
                              sep = ""))

########Analysis#####################
# extract plans
pref_smc_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)
pref_smc_plans_1 <- redist::get_plans_matrix(sim_smc_pref_1)
pref_smc_plans_2 <- redist::get_plans_matrix(sim_smc_pref_2)
pref_smc_plans_3 <- redist::get_plans_matrix(sim_smc_pref_3)
pref_smc_plans_4 <- redist::get_plans_matrix(sim_smc_pref_4)
pref_smc_plans_5 <- redist::get_plans_matrix(sim_smc_pref_5)

# get disparity table data
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_smc_0 <- cbind(n, wgt_smc_0)
#wgt_smc_0$n[which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))]
#Maxmin 1.0225 #12581
#redist::redist.plot.plans(sim_smc_pref_0, draws = 12581, geom = pref_map_0)

okayama_33_optimalmap_0 <- redist::redist.plot.map(shp = pref_0,
                                                  plan = pref_smc_plans_0[, which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))[1]],
                                                  boundaries = FALSE,
                                                  title = "Okayama Optimal Plan (0-split)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_0$max_to_min), 3), sep = ""), hjust = 0.5)

wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1)
#wgt_smc_1 <- cbind(n, wgt_smc_1)
#wgt_smc_1$n[which(wgt_smc_1$max_to_min == min(wgt_smc_1$max_to_min))]
#Maxmin 1.0299 #6754
#redist::redist.plot.plans(sim_smc_pref_1, draws = 6754, geom = pref_map_1)

okayama_33_optimalmap_1 <- redist::redist.plot.map(shp = pref_1,
                                                     plan = pref_smc_plans_1[, which(wgt_smc_1$max_to_min == min(wgt_smc_1$max_to_min))[1]],
                                                     boundaries = FALSE,
                                                     title = "Okayama Optimal Plan (1-split)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_1$max_to_min), 3), sep = ""), hjust = 0.5)


wgt_smc_2 <- simulation_weight_disparity_table(sim_smc_pref_2)
#wgt_smc_2 <- cbind(n, wgt_smc_2)
#wgt_smc_2$n[which(wgt_smc_2$max_to_min == min(wgt_smc_2$max_to_min))]
#Maxmin 1.0198 #22745
#redist::redist.plot.plans(sim_smc_pref_2, draws = 22745, geom = pref_map_2)

okayama_33_optimalmap_2 <- redist::redist.plot.map(shp = pref_2,
                                                     plan = pref_smc_plans_2[, which(wgt_smc_2$max_to_min == min(wgt_smc_2$max_to_min))[1]],
                                                     boundaries = FALSE,
                                                     title = "Okayama Optimal Plan (2-splits)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_2$max_to_min), 3), sep = ""), hjust = 0.5)


wgt_smc_3 <- simulation_weight_disparity_table(sim_smc_pref_3)
#wgt_smc_3 <- cbind(n, wgt_smc_3)
#wgt_smc_3$n[which(wgt_smc_3$max_to_min == min(wgt_smc_3$max_to_min))]
#Maxmin  1.0192 #1242  1303  4483  7098 ....
#redist::redist.plot.plans(sim_smc_pref_3, draws = 1242, geom = pref_map_3)

okayama_33_optimalmap_3 <- redist::redist.plot.map(shp = pref_3,
                                                     plan = pref_smc_plans_3[, which(wgt_smc_3$max_to_min == min(wgt_smc_3$max_to_min))[1]],
                                                     boundaries = FALSE,
                                                     title = "Okayama Optimal Plan (3-splits)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_3$max_to_min), 3), sep = ""), hjust = 0.5)

wgt_smc_4 <- simulation_weight_disparity_table(sim_smc_pref_4)
#wgt_smc_4 <- cbind(n, wgt_smc_4)
#wgt_smc_4$n[which(wgt_smc_4$max_to_min == min(wgt_smc_4$max_to_min))]
#Maxmin  1.019 #11481 13685 15912 16991 17110
#redist::redist.plot.plans(sim_smc_pref_4, draws = 11481, geom = pref_map_4)

okayama_33_optimalmap_4 <- redist::redist.plot.map(shp = pref_4,
                                                     plan = pref_smc_plans_4[, which(wgt_smc_4$max_to_min == min(wgt_smc_4$max_to_min))[1]],
                                                     boundaries = FALSE,
                                                     title = "Okayama Optimal Plan (4-splits)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_4$max_to_min), 3), sep = ""), hjust = 0.5)


wgt_smc_5 <- simulation_weight_disparity_table(sim_smc_pref_5)
#wgt_smc_5 <- cbind(n, wgt_smc_5)
#wgt_smc_5$n[which(wgt_smc_5$max_to_min == min(wgt_smc_5$max_to_min))]
#Maxmin  #1.008 #7545 10888 11775 16942 20091
#redist::redist.plot.plans(sim_smc_pref_5, draws = 7545, geom = pref_map_5)

okayama_33_optimalmap_5 <- redist::redist.plot.map(shp = pref_5,
                                                     plan = pref_smc_plans_5[, which(wgt_smc_5$max_to_min == min(wgt_smc_5$max_to_min))[1]],
                                                     boundaries = FALSE,
                                                     title = "Okayama Optimal Plan (5-splits)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_5$max_to_min), 3), sep = ""), hjust = 0.5)


#save(list=ls(all=TRUE), file="32_smc_okayama_data_0to5splits.Rdata")

#######Compactness###################
#------status quo---------#
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
prefadj <- redist::redist.adjacency(pref)
ferries <- add_ferries(pref)
prefadj <- geomander::add_edge(prefadj, ferries$V1, ferries$V2)

nedge <- as.numeric(length(unlist(prefadj)))

pref_cd <- status_quo_match(pref)
n_rem_orig <- redist::redist.compactness(shp = pref,
                                         plans = pref_cd$ku,
                                         measure = c("EdgesRemoved"),
                                         adj = prefadj)[1, ]$EdgesRemoved
ecc_orig <- n_rem_orig/nedge

#------simulated plans---------#
sim_smc_pref_0_plans <- redist::get_plans_matrix(sim_smc_pref_0)
nedge_0 <- as.numeric(length(unlist(prefadj_0)))
n_rem_0 <- (redist::redist.compactness(shp = pref_0,
                                       plans = sim_smc_pref_0_plans,
                                       measure = c("EdgesRemoved"),
                                       adj = prefadj_0) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_0 <- n_rem_0/nedge_0

sim_smc_pref_1_plans <- redist::get_plans_matrix(sim_smc_pref_1)
nedge_1 <- as.numeric(length(unlist(prefadj_1)))
n_rem_1 <- (redist::redist.compactness(shp = pref_1,
                                       plans = sim_smc_pref_1_plans,
                                       measure = c("EdgesRemoved"),
                                       adj = prefadj_1) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_1 <- n_rem_1/nedge_1

sim_smc_pref_2_plans <- redist::get_plans_matrix(sim_smc_pref_2)
nedge_2 <- as.numeric(length(unlist(prefadj_2)))
n_rem_2 <- (redist::redist.compactness(shp = pref_2,
                                       plans = sim_smc_pref_2_plans,
                                       measure = c("EdgesRemoved"),
                                       adj = prefadj_2) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_2 <- n_rem_2/nedge_2

sim_smc_pref_3_plans <- redist::get_plans_matrix(sim_smc_pref_3)
nedge_3 <- as.numeric(length(unlist(prefadj_3)))
n_rem_3 <- (redist::redist.compactness(shp = pref_3,
                                       plans = sim_smc_pref_3_plans,
                                       measure = c("EdgesRemoved"),
                                       adj = prefadj_3) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_3 <- n_rem_3/nedge_3

sim_smc_pref_4_plans <- redist::get_plans_matrix(sim_smc_pref_4)
nedge_4 <- as.numeric(length(unlist(prefadj_4)))
n_rem_4 <- (redist::redist.compactness(shp = pref_4,
                                       plans = sim_smc_pref_4_plans,
                                       measure = c("EdgesRemoved"),
                                       adj = prefadj_4) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_4 <- n_rem_4/nedge_4

sim_smc_pref_5_plans <- redist::get_plans_matrix(sim_smc_pref_5)
nedge_5 <- as.numeric(length(unlist(prefadj_5)))
n_rem_5 <- (redist::redist.compactness(shp = pref_5,
                                       plans = sim_smc_pref_5_plans,
                                       measure = c("EdgesRemoved"),
                                       adj = prefadj_5) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_5 <- n_rem_5/nedge_5

##############Co-occcurrence###################
#load packages
library(cluster)
library(viridis)
library(network)
library(ggnetwork)

#get plans that have a low max:min ratio (Top 10%)
good_num_0 <-  wgt_smc_0 %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(nsims*0.1)) %>%
  select(n)
good_num_0 <- as.vector(t(good_num_0))
sim_smc_pref_0_good <- sim_smc_pref_0 %>%
  filter(draw %in% good_num_0)
#obtain co-occurrence matrix
m_co_0 = prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

###calculate the centroids of each municipality/gun to plot population size
pref_map_0$CENTROID <- sf::st_centroid(pref_map_0$geometry)
pref_map_pop_centroid_0 <- pref_map_0 %>%
  as_tibble() %>%
  dplyr::select(code, CENTROID, pop) %>%
  separate(CENTROID, into = c("long", "lat"), sep = c(" "))
pref_map_pop_centroid_0$long <- str_remove_all(pref_map_pop_centroid_0$long, "[c(,]")
pref_map_pop_centroid_0$lat <- str_remove_all(pref_map_pop_centroid_0$lat, "[)]")
pref_map_pop_centroid_0$long <- as.numeric(pref_map_pop_centroid_0$long)
pref_map_pop_centroid_0$lat <- as.numeric(pref_map_pop_centroid_0$lat)

#prepare to bind together with network dataframe
lat <- pref_map_pop_centroid_0$lat
names(lat) <- as.character(pref_map_pop_centroid_0$code)
long <- pref_map_pop_centroid_0$long
names(long) <- as.character(pref_map_pop_centroid_0$code)

###Draw lines between municipalities that tend to be in the same district
m_co_sig_0 <- m_co_0
#extract co-occurrence > 90%
rownames(m_co_sig_0) <- pref_map_0$code
colnames(m_co_sig_0) <- pref_map_0$code
m_co_sig_0 <- as_tibble(as.data.frame(as.table(m_co_sig_0)))
m_co_sig_0$Freq <- as.numeric(m_co_sig_0$Freq)

#Clean up dataframe
m_co_sig_0 <- m_co_sig_0 %>%
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>%
  filter(Var1 != Var2, Freq > 0.9)
#Only the municipalities that are in the same district more than 90% of the time are included

#Creat 0 x 3 tibble
m_co_sig_0_adj <- m_co_sig_0
m_co_sig_0_adj <- m_co_sig_0_adj[ !(m_co_sig_0_adj$Var1 %in% m_co_sig_0$Var1), ]

#filter out the co-occurrence between adjacent municipalities
for(i in 1:length(pref_0$code)){
  p <- m_co_sig_0 %>%
    filter(Var1 == pref_0$code[i]) %>%
    filter(Var2 %in% c(as.character(pref_0$code[prefadj_0[[i]]+1])))
  m_co_sig_0_adj <- dplyr::bind_rows(p, m_co_sig_0_adj)
}

#use network package to obtain network
network_0_adj <- network(m_co_sig_0_adj, directed = FALSE, multiple = TRUE)

#Prepare geometry/edges for plotting
geometry_0_adj <- cbind(long[ network.vertex.names(network_0_adj) ],
                        lat[ network.vertex.names(network_0_adj) ])
edges_0_adj <- ggnetwork(network_0_adj, layout = geometry_0_adj, scale = FALSE)

### Color municipalities that tend to be in the same district
#cluster
cl_co_0 = cluster::agnes(m_co_0)
plot(as.dendrogram(cl_co_0)) # pick a number of clusters from the dendrogram.
prec_clusters_0 = cutree(cl_co_0, 4) # change 6 to the number of clusters you want

#convert to tibble
pref_membership_0 <- as_tibble(as.data.frame(prec_clusters_0))
pref_membership_0 <- bind_cols(pref_map_0$code, pref_membership_0)
names(pref_membership_0) <- c("code", "membership")
pref_membership_0$membership <- as.factor(pref_membership_0$membership)

#match membership data with pref_map_0
pref_map_0 <- merge(pref_map_0, pref_membership_0, by = "code")

###plot
pref_map_0 %>%
  ggplot() +
  geom_sf(aes(fill = membership.y), show.legend = FALSE) +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow",
                              "4" = "green")) +
  #size of the circles corresponds to population size in the municipality/gun
  geom_point(data = pref_map_pop_centroid_0, aes(long, lat, size = 10*pop/100000),
             color = "grey") +
  #color of the edges corresponds to the strength of the co-occurrence
  geom_edges(data = edges_0_adj, mapping = aes(x, y, xend = xend, yend = yend, color = Freq),
             size = 0.8) +
  scale_color_gradient(low = "white", high = "navy") +
  labs(size = "Population (10,000)",
       color = "Co-occurrence",
       title = "Co-occurrence Analysis: Plans with Top 10% Max-min Ratio",
       caption = "Lines represent co-occurrence between adjacent municipalities.") +
  theme(legend.box = "vertical",
        legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())
