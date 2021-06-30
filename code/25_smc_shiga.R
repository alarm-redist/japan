# ----------- set up functions-------------#
library(tidyverse)
set.seed(12345)
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

# ---------- Set Up Prefectures ----------#
pref_num <- 25
pref_name <- as.character("shiga")
ndists_new <- 3
ndists_old <- 4
lakes_removed <- c("琵琶湖")
nsims <- 25000
sim_type <- as.character("smc")

#-------- Clean data -----------#
# shapefile
pref_raw <- download_shp(pref_num)
pref <- clean_jcdf(pref_raw = pref_raw)
# remove lake
pref <- remove_lake(pref, lakes_removed)
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
# merge gun
pref <- merge_gun(pref)
# Add Ferries
edge <- add_ferries(pref)

# check map
pref %>%
  ggplot() +
  geom_sf(fill = "red")

# -------- set up for simulation ------------#
# simulation parameters
prefadj <- redist::redist.adjacency(pref) # Adjacency list
#add edge
prefadj <- geomander::add_edge(prefadj, edge$V1, edge$V2)


# set map
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.3,
                               total_pop = pop_national,
                               adj = prefadj)

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref <- redist::redist_smc(pref_map,
                                   nsims = nsims)

# save it
saveRDS(sim_smc_pref, paste("simulation/",
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
redist::redist.plot.plans(sim_smc_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "SMC")

weight_disparity_smc_pref <- simulation_weight_disparity_table(sim_smc_pref)
