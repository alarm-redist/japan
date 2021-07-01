##############load packages###################
library(tidyverse)
library(sf)
library(geomander)
library(redist)
library(ggthemes)
library(ggrepel)
library(scales)
library(rgdal)
library(plotly)
library(glue)
library(patchwork)
library(ggtext)
library(RSpectra)
library(readr)

############pref data (regardless of split)###############
# ----------- set up -------------#
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

# ---------- Set Up Prefectures ----------#
pref_num <- 34
pref_name <- as.character("hiroshima")
ndists_new <- 6
ndists_old <- 7
nsims <- 25000
sim_type <- as.character("smc")

#-------- Clean data (2015 Census)-----------#
# Clean data
pref_raw <- download_shp(pref_num) #34:Hiroshima
pref <- clean_jcdf(pref_raw = pref_raw)


#-------- Download 2020 census-----------#
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)


##############First try with 0 splits#######################
#-------- Use 2020 census data at the municipality level (0 splits this time)-----------#
pref0 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref0 <- merge_gun(pref0)

#Ferries
edge0 <- add_ferries(pref0)

# -------- set up for simulation ------------#
# simulation parameters
pref0adj <- redist::redist.adjacency(pref0) # Adjacency list
#add edge
pref0adj <- geomander::add_edge(pref0adj, edge0$V1, edge0$V2)

pref0_map <- redist::redist_map(pref0,
                               ndists = ndists_new,
                               pop_tol= 0.08,
                               total_pop = pop,
                               adj = pref0adj)

#save(list=ls(all=TRUE), file="34_smc_hiroshima_data1.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref0 <- redist::redist_smc(pref0_map,
                                   nsims = nsims)

# save it
saveRDS(sim_smc_pref0, paste("simulation/",
                            as.character(pref_num),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

# test with map
test_map_pref0 <- redist::redist.plot.plans(sim_smc_pref0,
                          draws = 1:6,
                          geom = pref0_map) +
  labs(caption = "SMC")


###############Skip(0 split analysis)#############
simulation_weight_disparity_table(sim_smc_pref)
#probably around 1.10

# -------- enumeration ------------#
# simulation
sim_enumerate_pref <- redist::redist.enumerate(prefadj,
                                               ndists = ndists_new,
                                               popvec = pref$pop_national,
                                               nconstraintlow = NULL,
                                               nconstrainthigh = NULL,
                                               popcons = NULL,
                                               contiguitymap = "rooks")
# test with map
redist::redist.plot.plans(sim_enumerate_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "Enumeration")



###############Workflow for 1 split##################
# -------- Choose which city to split ------------#
census2020 %>%
  filter(code > 34200 & code < 35000) %>% #excluding 34100 Hiroshima City
  arrange(desc(pop_national))
#Excluding Hiroshima City, Top 4 Muns
#34207: 福山 (currently NOT split; pre-gappei: 207 福山 481 内海 482沼隈 501神辺 524新市)
#34202: 呉 (currently NOT split; pre-gappei: 202呉 311音戸 312倉橋 313下蒲刈 324蒲刈 423安浦 424川尻 425豊浜 426豊)
#34212: 東広島 (currently split; pre-gappei: 212東広島 402黒瀬 405福富 406豊栄 408河内 422安芸津)
#34205: 尾道(currently split; pre-gappei: 205尾道 206因島 430瀬戸田 441御調 444向島)
#Since 4 municipalities (excluding Hiroshima City, a seireishiteitoshi) are currently split,
#the 1st ~ 4th largest municipalities will be split.
#江田島市 and 三原市, which are currently split, will no longer be split.

pref1 <- pref

# -------- 2015 小地域 data ------------#
#First have to obtain the number of Japanese nationals per each 小地域 as of 2015
#JINKO in pref includes foreigners too -> calculate Japanese population
dem_pops <- download_pop_demographics(pref_num) #first download data
pref1 <- calc_kokumin(pref1, dem_pops)

# -------- Estimate 2020 小地域 data ------------#
#Estimate 2020 Pop at the 小地域-level (pref is currently based on 2015 Census)
#*Note: when splitting the 4 municipalities based on the pre-gappei boundaries,
#*the population data will be based on the 2015 Census
#*this inconsistency will be resolved once we get access to the 2020 Census
pref1 <- estimate_2020_pop(pref1, census2020)

# -------- Make naming consistent ------------#
pref1 <- pref1 %>%
  dplyr::rename(pop = pop_estimate) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge municipalities (with exceptions) ------------#
pref1 <- merge_small(pref1, split_codes = 34207, intact_codes = c(34101, 34102, 34103, 34104, 34105,
                                           34106, 34107, 34108))
#split Fukuyamashi (34207)
#made sure to group together the 8 Wards of Hiroshima City together
#wards are treated as single municipalities;
#splitting Hiroshima City doesn't count as a municipality split

# -------- Merge gun (0 exceptions) ------------#
pref1 <- merge_gun(pref1, exception = 34207)
#make sure to set Fukuyamashi as an exception; or else Fukuyamashi will be grouped together

# -------- Old boundary ------------#
#first download data
old_boundary <- download_old_shp(pref_code = pref_num)
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_num)

pref1 <- reflect_old_boundaries(pref1,
                                old_boundary = old_boundary,
                                pop_by_old_boundary = pop_by_old_boundary,
                                old_code = c(34207, 34481, 34482, 34501, 34524),
                                #codes of municipalities that now belong to Fukuyamashi
                                 new_code = 34207) #code of merged municipality (Fukuyamashi)

# -------- Ferries ------------#
edge1 <- add_ferries(pref1)

# -------- set up for simulation ------------#
# simulation parameters
pref1adj <- redist::redist.adjacency(pref1) # Adjacency list
#add edge
pref1adj <- geomander::add_edge(pref1adj, edge1$V1, edge1$V2)

###For this case, I need to add one adjacency manually because there is an island (福山市内海町)
###that is not connected by a ferry to mainland 福山市. 内海町 is connected to 福山市 by a bridge.
###This would not a problem when 福山市 is not split, but now that
###福山市 is divided based on the old administrative districts, 内海町 is treated as a single municipality
###and thus needs to be connected to its neighboring municipality.
pref1adj <- geomander::add_edge(pref1adj, 1, 2)
#1 corresponds to 福山市 and 2 corresponds to 内海町

pref1_map <- redist::redist_map(pref1,
                                ndists = ndists_new,
                                pop_tol= 0.08,
                                total_pop = pop,
                                adj = pref1adj)

#save(list=ls(all=TRUE), file="34_smc_hiroshima_data1.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref1 <- redist::redist_smc(pref1_map,
                                    nsims = nsims)

# save it
saveRDS(sim_smc_pref1, paste("simulation/",
                             as.character(pref_num),
                             "_",
                             as.character(pref_name),
                             "_",
                             as.character(sim_type),
                             "_",
                             as.character(nsims),
                             "_",
                             "one split (Fukuyama)",
                             ".Rds",
                             sep = ""))

# test with map
test_map_pref1 <- redist::redist.plot.plans(sim_smc_pref1,
                                            draws = 1:6,
                                            geom = pref1_map) +
  labs(caption = "SMC")

