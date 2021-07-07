############# set up ###############
#-------------- functions set up ---------------
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
pref_code <- 25
pref_name <- "shiga"
lakes_removed <- c("琵琶湖") # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4
#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
nsplit <- 1
# the code of split municipaliti
split_codes <- c(25201)
intact_codes <- c()
merge_gun_exception <- c()  # enter `c()` if not applicable

######### Download and Clean Census ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code) #first download data
# clean shp
cleaned_census_shp <- pref_raw %>%
  clean_jcdf() %>%
  calc_kokumin(age_pops = dem_pops)

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

pref <- cleaned_census_shp %>%
  dplyr::rename(pop = JINKO)

# remove lake
ifelse(is.null(lakes_removed),
       pref <- pref,
       pref <- remove_lake(pref,lakes_removed))

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

####### Simulation by number of splits#######

for(i in 0:nsplit){
  pref_n <- split_pref(pref = pref,
                       census2020 = census2020,
                       old_boundary = old_boundary,
                       pop_by_old_boundary = pop_by_old_boundary,
                       nsplit = i,
                       split_codes = split_codes,
                       intact_codes = intact_codes,
                       merge_gun_exception = merge_gun_exception)

  #------------- set up map ----------------
  # simulation parameters
  prefadj <- redist::redist.adjacency(shp = pref_n) # Adjacency list

  # add ferry if applicable
  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref_n)
    prefadj <- geomander::add_edge(prefadj,
                                   ferries[, 1],
                                   ferries[, 2],
                                   zero = TRUE)
    # check contiguity
    suggest <-  geomander::suggest_component_connection(shp = pref_n,
                                                        adj = prefadj)
    prefadj <- geomander::add_edge(prefadj,
                                   suggest$x,
                                   suggest$y,
                                   zero = TRUE)
  }

  # define map
  pref_map <- redist::redist_map(pref_n,
                                 ndists = ndists_new,
                                 pop_tol= 0.40,
                                 total_pop = pop,
                                 adj = prefadj)

  ###### simulation ######
  sim_smc_pref <- redist::redist_smc(pref_map,
                                     nsims = nsims)
  # save it
  saveRDS(sim_smc_pref, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_",
                              as.character(i),
                              "split.Rds",
                              sep = ""))

  # get plans
  smc_plans_pref <- redist::get_plans_matrix(sim_smc_pref)

  # get disparity data
  smc_weight_pref <- simulation_weight_disparity_table(sim_smc_pref)

  # rename elements to be used
  assign(paste(pref_name, pref_code, i, sep = "_"),
         pref_n)
  assign(paste(pref_name, pref_code, "adj", i, sep = "_"),
         prefadj)
  assign(paste(pref_name, pref_code, "map", i, sep = "_"),
         pref_map)
  assign(paste(pref_name, pref_code, "sim_smc", i, sep = "_"),
         sim_smc_pref)
  assign(paste(pref_name, pref_code, "smc_plans", i, sep = "_"),
         smc_plans_pref)
  assign(paste(pref_name, pref_code, "smc_weight", i, sep = "_"),
         smc_weight_pref)

  rm(list= ls()[(ls() %in% c("pref_n",
                             "prefadj",
                             "pref_map",
                             "sim_smc_pref",
                             "smc_plans_pref",
                             "smc_weight_pref",
                             "ferries",
                             "suggest",
                             "port_data",
                             "route_data")
  )])

}

maxmin_LH <- ggplot(data = shiga_25_smc_weight_0,
                    mapping = aes(x = LH,
                                  y = max_to_min))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

maxmin_LH

ggsave(filename = paste("plots/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_",
                        as.character(sim_type),
                        "_",
                        as.character(nsims),
                        "_",
                        as.character(nsplit),
                        "maxmin_LH_split.png",
                        sep = ""),
       plot = maxmin_LH)

LH_HH <- ggplot(data = shiga_25_smc_weight_0,
                    mapping = aes(x = LH,
                                  y = HH))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

LH_HH

ggsave(filename = paste("plots/",
                        as.character(pref_code),
                        "_",
                        as.character(pref_name),
                        "_",
                        as.character(sim_type),
                        "_",
                        as.character(nsims),
                        "_",
                        as.character(nsplit),
                        "LH_HH_split.png",
                        sep = ""),
       plot = LH_HH)
