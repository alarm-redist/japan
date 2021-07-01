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
merge_gun_exception <- c(0)
nsims <- 25000
sim_type <- as.character("smc")
nsplit <- 0

#-------- Clean data (2015 Census)-----------#
# Clean data
pref_raw <- download_shp(pref_num)
pref <- clean_jcdf(pref_raw = pref_raw)
# remove lake
pref <- remove_lake(pref, lakes_removed)

#-------- Download 2020 census-----------#
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)


##############First try with 0 splits#######################
#-------- Use 2020 census data at the municipality level (0 splits this time)-----------#
pref0 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref0 <- merge_gun(pref = pref0,
                   exception = merge_gun_exception)

#Ferries
edge0 <- add_ferries(pref0)
# check map

pref %>%
  ggplot() +
  geom_sf(fill = "red")

# -------- set up for simulation ------------#
# simulation parameters
pref0adj <- redist::redist.adjacency(pref0) # Adjacency list
#add edge
pref0adj <- geomander::add_edge(pref0adj, edge0$V1, edge0$V2)

pref0_map <- redist::redist_map(pref0,
                                ndists = ndists_new,
                                pop_tol= 0.3,
                                total_pop = pop,
                                adj = pref0adj)


# --------- SMC simulation ----------------#
# simulation
sim_smc_pref0 <- redist::redist_smc(pref0_map,
                                    nsims = nsims)

# save it
saveRDS(sim_smc_pref0, paste("simulation/",
                             as.character(pref_num),
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
                        as.character(pref_num),
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
                           as.character(pref_num),
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
