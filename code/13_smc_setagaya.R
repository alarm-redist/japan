#set up
pref_code = 13
pref_name = "tokyo"
sim_type = "smc"
nsims = 25000

#load packages
library(tidyverse)
library(redist)

#load
load("13_enum_setagaya.RData")

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



