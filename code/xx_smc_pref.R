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

for(i in 0:nsplit){
  pref_n <- split_pref(pref_code = pref_code,
                       lakes_removed = lakes_removed,
                       nsplit = i,
                       split_codes = split_codes,
                       intact_codes = intact_codes,
                       old_code = old_code,
                       merge_gun_exception = merge_gun_exception)
  assign(paste("pref_", i, sep = ""),
         pref_n)
}

# -------- set up for simulation ------------#
# simulation parameters
pref0adj <- redist::redist.adjacency(pref0) # Adjacency list
#add edge
pref0adj <- geomander::add_edge(pref0adj, edge0$V1, edge0$V2)

pref0_map <- redist::redist_map(pref0,
                                ndists = ndists_new,
                                pop_tol= pop_tol,
                                total_pop = pop,
                                adj = pref0adj)


# --------- SMC simulation ----------------#
# simulation
sim_smc_pref0 <- redist::redist_smc(pref0_map,
                                    nsims = nsims)

# save it
saveRDS(sim_smc_pref0, paste("simulation/",
                             as.character(pref_code),
                             "_",
                             as.character(pref_name),
                             "_",
                             as.character(sim_type),
                             "_",
                             as.character(nsims),
                             "_",
                             as.character(nsplit),
                             "split.Rds",
                             sep = ""))

# test with map
redist::redist.plot.plans(sim_smc_pref0,
                          draws = 1:6,
                          geom = pref0_map) +
  labs(caption = sim_type)

# -------- Evaluating Redistricting Plan (0 split)------------#
# get plans
pref0_smc_plans <- redist::get_plans_matrix(sim_smc_pref0)

# get disparity data
wgt_tbl0 <- simulation_weight_disparity_table(sim_smc_pref0)

#best ippyo no kakusa
min(wgt_tbl0$max_to_min)

# find optimal
n <- c(1:nsims)
wgt_tbl0 <- cbind(as.data.frame(n), wgt_tbl0)

optimal <- wgt_tbl0$n[which(wgt_tbl0$max_to_min == min(wgt_tbl0$max_to_min))]

#print optimal plan
optimal_map <- redist::redist.plot.plans(sim_smc_pref0,
                                         draws = optimal[1],
                                         geom = pref0_map) +
  labs(title = paste(pref_name,
                     "_",
                     sim_type,
                     "_",
                     nsims,
                     "_",
                     nsplit,
                     "split_optimal plan",
                     sep = ""),
       subtitle = paste("Max to Min Ratio =",
                        round(min(wgt_tbl0$max_to_min), 4),
                        "\n Loosemore-Hanby index =",
                        round(min(wgt_tbl0$LH), 4),
                        sep = " "),
       caption = paste("#",
                       optimal[1],
                       sep = ""))
optimal_map

# save it
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
                        "split.png",
                        sep = ""),
       plot = optimal_map)

saveRDS(optimal_map, paste("plots/",
                           as.character(pref_code),
                           "_",
                           as.character(pref_name),
                           "_",
                           as.character(sim_type),
                           "_",
                           as.character(nsims),
                           "_",
                           as.character(nsplit),
                           "split.Rds",
                           sep = ""))
