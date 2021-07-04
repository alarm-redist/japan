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

########### Split one municipality ###############

#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
nsplit <- 1
# the code of split municipaliti
split_codes <- c(25201)
intact_codes <- c()
old_code <- data.frame(
  a = c(25201, 25301),
  b = NA,
  c = NA,
  d = NA,
  e = NA
)
merge_gun_exception <- c()  # enter `c()` if not applicable

############## Prepare data #################
# clean and get data for simulation

for(i in 0:nsplit){
  pref_n <- split_pref(pref_code = pref_code,
                       lakes_removed = lakes_removed,
                       nsplit = i,
                       split_codes = split_codes,
                       intact_codes = intact_codes,
                       old_code = old_code,
                       merge_gun_exception = merge_gun_exception)

  #------------- set up map ----------------
  # simulation parameters
  prefadj <- redist::redist.adjacency(shp = pref_n) # Adjacency list

  # add ferries
  # ignore errors if there is no ferry
  #ferries <- add_ferries(pref_n)
  #prefadj <- geomander::add_edge(prefadj,
  #                               ferries[, 1],
  #                               ferries[, 2],
  #                               zero = TRUE)
  # check contiguity
  #suggest <-  geomander::suggest_component_connection(shp = pref_n,
  #                                                    adj = prefadj)
  #prefadj <- geomander::add_edge(prefadj,
  #                               suggest$x,
  #                               suggest$y,
  #                               zero = TRUE)

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
  assign(paste("pref_", i, sep = ""),
         pref_n)
  assign(paste("prefadj_", i, sep = ""),
         prefadj)
  assign(paste("pref_map_", i, sep = ""),
         pref_map)
  assign(paste("sim_smc_pref_", i, sep = ""),
         sim_smc_pref)
  assign(paste("smc_plans_pref_", i, sep = ""),
         smc_plans_pref)
  assign(paste("smc_weight_pref_", i, sep = ""),
         smc_weight_pref)

  rm(list= ls()[(ls() %in% c("pref_n",
                             "prefadj",
                             "pref_map",
                             "sim_smc_pref",
                             "smc_plans_pref",
                             "smc_weight_pref")
  )])

}
