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
pref_code <- 25
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
pref_raw <- download_shp(pref_code)
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

###############Workflow for 1 split##################
# additional set up
nsplit <- 1
seirei_shitei <- c(0)
intact_codes <- c(0)
merge_gun_exception <- c(0)
old_code <- c(25201, 25301)
# -------- Choose which city to split ------------#
# rank without seirei shitei (government designated) city
# choose the city to split
split_codes <- census2020 %>%
  dplyr::filter(code > pref_code*1000 &
                  code < (pref_code + 1)*1000 &
                  code != seirei_shitei) %>%
  dplyr::arrange(desc(pop_national)) %>%
  dplyr::select(code) %>%
  dplyr::slice(nsplit) %>%
  tibble::deframe()

# -------- 2015 小地域 data ------------#
#First have to obtain the number of Japanese nationals per each 小地域 as of 2015
#JINKO in pref includes foreigners too -> calculate Japanese population
pref1 <- pref
dem_pops <- download_pop_demographics(pref_code) #first download data
pref1 <- calc_kokumin(pref1, dem_pops)

# -------- Estimate 2020 小地域 data ------------#
#Estimate 2020 Pop at the 小地域-level (pref is currently based on 2015 Census)
#*Note: when splitting the 4 municipalities based on the pre-gappei boundaries,
#*the population data will be based on the 2015 Census
#*this inconsistency will be resolved once we get access to the 2020 Census
pref1 <- estimate_2020_pop(pref1, census2020)

# -------- Make naming consistent ------------#
pref1 <- pref1 %>%
  dplyr::rename(pop = pop_estimate) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge municipalities (with exceptions) ------------#
pref1 <- merge_small(pref = pref1,
                     split_codes = split_codes,
                     intact_codes = intact_codes)

# -------- Merge gun (0 exceptions) ------------#
pref1 <- merge_gun(pref = pref1,
                   exception = merge_gun_exception)

# -------- Old boundary ------------#
#first download data
old_boundary <- download_old_shp(pref_code = pref_code)
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

pref1 <- reflect_old_boundaries(pref1,
                                old_boundary = old_boundary,
                                pop_by_old_boundary = pop_by_old_boundary,
                                old_code = old_code,
                                new_code = split_codes)

# -------- Ferries ------------#
edge1 <- add_ferries(pref1)

# -------- set up for simulation ------------#
# simulation parameters
pref1adj <- redist::redist.adjacency(pref1) # Adjacency list
#add edge
pref1adj <- geomander::add_edge(pref1adj, edge1$V1, edge1$V2)

# create map
pref1_map <- redist::redist_map(pref1,
                                ndists = ndists_new,
                                pop_tol= 0.35,
                                total_pop = pop,
                                adj = pref1adj)

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref1 <- redist::redist_smc(pref1_map,
                                    nsims = nsims)

# save it
saveRDS(sim_smc_pref1, paste("simulation/",
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
redist::redist.plot.plans(sim_smc_pref1,
                          draws = 1:6,
                          geom = pref1_map)
# -------- Evaluating Redistricting Plan (1 split)------------#
# get plans
pref1_smc_plans <- redist::get_plans_matrix(sim_smc_pref1)

# get disparity data
wgt_tbl1 <- simulation_weight_disparity_table(sim_smc_pref1)

#best ippyo no kakusa
min(wgt_tbl1$max_to_min)

# find optimal
n <- c(1:nsims)
wgt_tbl1 <- cbind(as.data.frame(n), wgt_tbl1)

optimal <- wgt_tbl1$n[which(wgt_tbl1$max_to_min == min(wgt_tbl1$max_to_min))]

#print optimal plan
optimal_map <- redist::redist.plot.plans(sim_smc_pref1,
                                         draws = optimal[1],
                                         geom = pref1_map) +
  labs(title = paste(pref_name,
                     "_",
                     sim_type,
                     "_",
                     nsims,
                     "_",
                     nsplit,
                     "split_optimal plan",
                     sep = ""),
       subtitle = paste("Max to Min Ratio",
                        round(min(wgt_tbl1$max_to_min), 4),
                        sep = "="),
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
