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

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)


##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#ferries
ferries_0 <- add_ferries(pref_0)
#connect [12]38340 上島町 to [2]38202今治市

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list

#add edge(#connect [12]38340 上島町 to [2]38202今治市)
prefadj_0 <- geomander::add_edge(prefadj_0, 2, 12)


pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_0)

#save(list=ls(all=TRUE), file="38_smc_ehime_data_0split.Rdata")

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
pref_1 <- pref

# -------- Merge gun (0 exceptions) ------------#
pref_1 <- merge_gun(pref_1)

# -------- Old boundary ------------#
old_38201 <- find_old_codes(38201, pop_by_old_boundary)
#松山市: 38201(旧松山市) 38211（旧北条市） 38363（旧中島町）
pref_1 <- reflect_old_boundaries(pref_1, old_boundary, pop_by_old_boundary, old_38201, 38201)

#Estimate 2020 pop based on old boundary
pref_1 <- estimate_old_boundary_pop(old_38201, 38201, pref_1, census2020)

#Ferries
ferries_1 <- add_ferries(pref_1)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_1 <- redist::redist.adjacency(pref_1) # Adjacency list
#add edge
prefadj_1 <- geomander::add_edge(prefadj_1, 1, 3)  #connect [3]338363 旧中島町 to [1]38201旧松山市
prefadj_1 <- geomander::add_edge(prefadj_1, 4, 14) #connect [14]38340 上島町 to [4]38202今治市

pref_map_1 <- redist::redist_map(pref_1,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_1)

###save(list=ls(all=TRUE), file="38_smc_ehime_data_1split.Rdata")

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

##################
redist.plot.map(shp = pref_1) + theme_map()

redist.plot.plans(sim_smc_pref_1, draws = 24500,
                  geom = pref_map_1)
