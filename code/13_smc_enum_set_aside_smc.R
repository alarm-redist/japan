############# set up ###############
library(tidyverse)
set.seed(12345)

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- information set-up -----------#
sim_type <- "smc"
nsims <- 25000
pref_code <- 13
pref_name <- "tokyo"
ndists_new <- 30
ndists_old <- 25

######### Download and Clean Data ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code)
# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

#########Clean data: pref################
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
#Estimate 2020 pop.
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#######Merge Gun#########
#merge nishitama
nishitama <- pref %>%
  dplyr::filter(code %in% c("13303", "13305", "13307", "13308"))
nishitama$code <- 13300
nishitama <- nishitama %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
nishitama$subcode <- "0000"

#merge oshima
oshima <- pref %>%
  dplyr::filter(code %in% c("13361", "13362", "13363", "13364"))
oshima$code <- 13360
oshima <- oshima %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
oshima$subcode <- "0000"

#merge miyake
miyake <- pref %>%
  dplyr::filter(code %in% c("13381", "13382"))
miyake$code <- 13380
miyake <- miyake %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
miyake$subcode <- "0000"

#merge hachijyo
hachijyo <- pref %>%
  dplyr::filter(code %in% c("13401", "13402"))
hachijyo$code <- 13400
hachijyo <- hachijyo %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
hachijyo$subcode <- "0000"

#merge ogasawara
ogasawara <- pref %>%
  dplyr::filter(code == "13421")
ogasawara$code <- 13420
ogasawara <- ogasawara %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
ogasawara$subcode <- "0000"

#merge back together
pref_non_gun <- pref %>%
  dplyr::filter(code %in% c("13303", "13305", "13307", "13308",
                            "13361", "13362", "13363", "13364",
                            "13381", "13382",
                            "13401", "13402",
                            "13421") == FALSE)
#now pref is an object with 0 gun splits
pref <- dplyr::bind_rows(pref_non_gun, nishitama, oshima,
                         miyake, hachijyo, ogasawara)

#######Separat Tama from 23ku###############
rural <-  pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115",
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420") == FALSE)

urban <- pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115", #with Setagaya
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420")) #Islands are considered connected to Minato-ku

#save(list=ls(all=TRUE), file="13_smc_tokyo_basic_data_by_region.Rdata")

##########Group by municipalities############
rural_by_mun <- rural %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

urban_by_mun <- urban %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

#######maps set-up##########
urban_by_mun_edge <- add_ferries(urban_by_mun)

# simulation parameters
urban_by_mun_edge_adj <- redist::redist.adjacency(urban_by_mun) # Adjacency list
#add ferries
urban_by_mun_edge_adj <- geomander::add_edge(urban_by_mun_edge_adj,
                                             urban_by_mun_edge$V1,
                                             urban_by_mun_edge$V2)

urban_group_map <- redist::redist_map(urban_by_mun,
                                      ndists = 2,
                                      pop_tol= 0.08,
                                      total_pop = pop,
                                      adj = urban_by_mun_edge_adj)

#########enumeration set-up##############
makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

# simulation
if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

########enumerate########################
urban_enum_plans <- redist::redist.enumpart(adj = urban_by_mun_edge_adj,
                                       unordered_path = here::here('simulation/unord_tokyo_urban'),
                                       ordered_path = here::here('simulation/ord_tokyo_urban'),
                                       out_path = here::here('simulation/enum_tokyo_urban'),
                                       ndists = 2,
                                       all = TRUE,
                                       total_pop = urban_group_map[[attr(urban_group_map, 'pop_col')]])

save(urban_enum_plans, file = "simulation/13_enum_tokyo_urban_muns.Rdata")

#check result
urban_enum_plans_result <- redist::redist.read.enumpart(out_path = 'simulation/enum_tokyo_urban')
urban_enum_plans_result_matrix <- redist::redist_plans(urban_enum_plans_result,
                                                       urban_group_map,
                                                       algorithm = 'enumpart')

#multiple of target pop.
target <- round(sum(urban_by_mun$pop)/21)
#Score: 1.*target   2.round(*target) - *target   3. take absolute value
urban_enum_plans_result_matrix$score <- abs(urban_enum_plans_result_matrix$total_pop/target -
  round(urban_enum_plans_result_matrix$total_pop/target))

#filter out best plan
best_split <-
urban_enum_plans_result_matrix %>%
  filter(draw == urban_enum_plans_result_matrix$draw[which(urban_enum_plans_result_matrix$score ==
                                                             min(urban_enum_plans_result_matrix$score))])
#visualization
urban_by_mun$block <- best_split

#get data on optimal plan
matrix_best_split <- redist::get_plans_matrix(best_split)
colnames(matrix_best_split) <- "block"
optimal_boundary <- cbind(head(urban_by_mun, 23), head(as_tibble(matrix_best_split), 23))


#map with block data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary, aes(fill = factor(block))) +
  scale_fill_manual(values = c("red", "yellow")) +
  geom_sf(data = head(urban_by_mun, 23), fill = NA, color = "black", lwd = 1.5) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

