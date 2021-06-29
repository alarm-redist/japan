# ----------- set up -------------#
library(tidyverse)
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")
#-------- Clean data -----------#
# shapefile
pref_raw <- download_shp(25)
pref <- clean_jcdf(pref_raw = pref_raw)
# remove lake
pref <- remove_lake(pref, "琵琶湖")
# 2020 census
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# clean it
census2020 <- clean_2020_census(total = total, foreigner = foreigner)
# combining two data
pref <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code'))
# check map
pref %>%
  ggplot() +
  geom_sf(fill = "red")
# get municodes
municodes <- pref$code
# -------- set up for simulation ------------#
# simulation parameters
prefadj <- redist::redist.adjacency(pref) # Adjacency list
# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.08,
                               total_pop = pop_national,
                               adj = prefadj)

# --------- Merge Split simulation ----------------#
sim_ms_pref <- redist::redist_mergesplit(map = pref_map,
                                         nsims = 25000,
                                         warmup = 1,
                                         compactness = 1.4)

redist::redist.plot.plans(sim_ms_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "Merge Split")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref <- redist::redist_smc(pref_map,
                                   nsims = 25000,
                                   pop_temper = 0.05)
# test with map
redist::redist.plot.plans(sim_smc_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "SMC")

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
