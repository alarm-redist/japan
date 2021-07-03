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
sim_type <- "enum"
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
pref_n <- split_pref(pref_code = pref_code,
                     nsplit = nsplit,
                     split_codes = split_codes,
                     intact_codes = intact_codes,
                     old_code = old_code,
                     merge_gun_exception = merge_gun_exception)

################ Simulation with enumuration################
#------------- set up map ----------------
# simulation parameters
prefadj <- redist::redist.adjacency(shp = pref_n) # Adjacency list

# add ferries
# ignore errors if there is no ferry
ferries <- add_ferries(pref_n)
prefadj <- geomander::add_edge(prefadj, ferries[, 1], ferries[, 2], zero = TRUE)
# check contiguity
suggest <-  geomander::suggest_component_connection(shp = pref_n,
                                                    adj = prefadj)
prefadj <- geomander::add_edge(prefadj,
                               suggest$x,
                               suggest$y,
                               zero = TRUE)

# define map
pref_map <- redist::redist_map(pref_n,
                               ndists = ndists_new,
                               pop_tol= 0.20,
                               total_pop = pop,
                               adj = prefadj)

# ---------------- enumuration ---------------------
# mechanical set up
makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

# set inital path
if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

enum_plans <- redist::redist.enumpart(adj = prefadj,
                                      unordered_path = here::here('data/unord'),
                                      ordered_path = here::here('data/ord'),
                                      out_path = here::here('data/enum'),
                                      ndists = ndists_new,
                                      all = TRUE,
                                      total_pop = pref_map[[attr(pref_map, 'pop_col')]])

good_plans <- enum_plans[[1]][, enum_plans[[2]] < redist::get_pop_tol(pref_map)]

plans <- redist::redist_plans(good_plans,
                              pref_map,
                              algorithm = 'enumpart')

# get disparity data
wgt_enum <- simulation_weight_disparity_table(plans)

