############# set up ###############
#-------------- functions set up ---------------
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
# prefectural information
sim_type <- "smc"
nsims <- 25000
pref_code <- 38
pref_name <- "ehime"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4

#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
nsplit <- 1
merge_gun_exception <- c()  # enter `c()` if not applicable

######### Download and Clean Data ############
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

# remove lake
ifelse(is.null(lakes_removed),
       pref <- pref,
       pref <- remove_lake(pref, lakes_removed))

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

# the code of split municipalities
split_codes <- pref[order(-pref$pop), ]$code[0:nsplit]
intact_codes <- c()

##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#Ferries　ferries_0 <- add_ferries(pref_0) No need to add ferries in this case

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list

#manually add adjacency
prefadj_0 <- geomander::add_edge(prefadj_0, 12, 1)
prefadj_0 <- geomander::add_edge(prefadj_0, 12, 13)
prefadj_0 <- geomander::add_edge(prefadj_0, 12, 15)
prefadj_0 <- geomander::add_edge(prefadj_0, 12, 8)
#connect [12]38340 伊予郡松前町&砥部町 to [1]38201松山市
#connect [12]38340 伊予郡松前町&砥部町 to [13]38386久方高原町
#connect [12]38340 伊予郡松前町&砥部町 to [15]38422内子町
#connect [12]38340 伊予郡松前町&砥部町 to [8] 38210伊予市

pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
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





##################
redist.plot.map(shp = minus_added_municipalities_5) + theme_map()

redist.plot.plans(sim_smc_pref_0, draws = 1,
                  geom = pref_map_0)
