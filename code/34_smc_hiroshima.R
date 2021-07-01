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
sim_smc_pref <- redist::redist_smc(pref0_map,
                                   nsims = 25000,
                                   pop_temper = 0.05)
# test with map
redist::redist.plot.plans(sim_smc_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "SMC")

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

# -------- 2015 小地域 data ------------#
#First have to obtain the number of Japanese nationals per each 小地域 as of 2015
#JINKO in pref includes foreigners too -> calculate Japanese population
age_pops <- download_pop_demographics(pref_num) #first download data on pop by age group
pref <- calc_kokumin(pref, age_pops)

# -------- Estimate 2020 小地域 data ------------#
#Estimate 2020 Pop at the 小地域-level (pref is currently based on 2015 Census)
#*Note: when splitting the 4 municipalities based on the pre-gappei boundaries,
#*the population data will be based on the 2015 Census
#*this inconsistency will be resolved once we get access to the 2020 Census
pref <- estimate_2020_pop(pref, census2020)
#The column "pop" in "pref" now indicates the "estimated number of Japanese nationals"

# -------- Make naming consistent ------------#
dplyr::select(code, geometry, pop)

# -------- Merge municipalities (with exceptions) ------------#
pref <- merge_small(pref, split_codes = c(), intact_codes = c(34101, 34102, 34103, 34104, 34105,
                                           34106, 34107, 34108))
#made sure to group together the 8 Wards of Hiroshima City together
#wards are treated as single municipalities; splitting Hiroshima City doesn't count as a municipality split

# -------- Old boundary ------------#
pref <- reflect_old_boundaries(34)
