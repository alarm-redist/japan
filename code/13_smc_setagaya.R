#set up
pref_code = 13
pref_name = "tokyo"
sim_type = "smc"
nsims = 25000

#load packages
library(tidyverse)
library(redist)

#load
load("~/Desktop/ALARM Project/Tokyo Results/by_region/setagaya/13_enum_setagaya.RData")

#adjacency list
setagaya_adj <- redist::redist.adjacency(setagaya)

#set up map
setagaya_map <- redist::redist_map(setagaya,
                                   ndists = 2,
                                   pop_tol= 0.03,
                                   total_pop = pop,
                                   adj = setagaya_adj)

#smc simulation
setagaya_smc <- redist::redist_smc(setagaya_map,
                                   nsims = nsims,
                                   pop_temper = 0.05)

#save
saveRDS(setagaya_smc, paste("simulation/",
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

setagaya_smc <- readRDS("~/Desktop/ALARM Project/Tokyo Results/by_region/setagaya/13_setagaya_smc_25000.Rds")

#######obtain optimal plan#################
# get disparity table data
setagaya_smc_wgt <- simulation_weight_disparity_table(setagaya_smc)
n <- c(1:25000)
setagaya_smc_wgt <- cbind(n, setagaya_smc_wgt)
setagaya_smc_wgt$n[which(setagaya_smc_wgt$max_to_min == min(setagaya_smc_wgt$max_to_min))]
#Maxmin 1.000026 # 3310 14212
redist::redist.plot.plans(setagaya_smc, draws = 3310, geom = setagaya_map)




