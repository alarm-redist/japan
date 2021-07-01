# ----------- set up -------------#
library(tidyverse)

remotes::install_github("alarm-redist/redist@dev")

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- Clean data -----------#
# shapefile
pref_code <- 42
pref_name <- "nagasaki"
sim_type <- "smc"
nsims <- 5000

# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4

pref_raw <- download_shp(pref_code)
pref <- clean_jcdf(pref_raw = pref_raw)

# remove lake, not necessarily for Nagasaki
# pref <- remove_lake(pref, "琵琶湖")

# 2020 census
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")

# clean it
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# combining two data
pref <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

# exception of 北松浦郡
pref <- merge_gun(pref, exception = 42383)
pref <- sf::st_as_sf(pref)

# check map
pref %>%
  ggplot() +
  geom_sf(fill = "red")

# -------- set up for simulation ------------#
# simulation parameters
prefadj <- redist::redist.adjacency(shp = pref) # Adjacency list

# add ferries
ferries <- add_ferries(pref)
prefadj <- geomander::add_edge(prefadj, ferries[, 1], ferries[, 2], zero = TRUE)

# check contiguity
contiguity <- geomander::check_contiguity(prefadj)
# suggest <- geomander::suggest_component_connection()

prefadj <- geomander::add_edge(prefadj, which(pref$code == 42209),
                               which(pref$code == 42207), zero = TRUE) # Fixing 壱岐市、対島市 isolation

pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.20,
                               total_pop = pop,
                               adj = prefadj)

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref <- redist::redist_smc(pref_map,
                                   nsims = nsims,
                                   pop_temper = 0.05)

# get disparity data
wgt_tbl <- simulation_weight_disparity_table(sim_smc_pref)

# get plans
pref_ms_plans <- redist::get_plans_matrix(sim_smc_pref)

saveRDS(sim_smc_pref, paste("simulation/",
                            as.character(pref_code),
                            "_",
                            as.character(pref_name),
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

# --------- SMC Analysis ----------------#



