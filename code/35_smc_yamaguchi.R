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
pref_code <- 35
pref_name <- "yamaguchi"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4
#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
nsplit <- 2
merge_gun_exception <- c()  # enter `c()` if not applicable

######### Download and Clean Census ############
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
split_codes <- pref[order(-pref$pop), ]$code[1:nsplit]
intact_codes <- c()

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
  # add ferry if applicable
  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref_n)

    if(nrow(ferries) > 0) {
      prefadj <- geomander::add_edge(prefadj,
                                     ferries[, 1],
                                     ferries[, 2],
                                     zero = TRUE)
    }

    #suggest <-  geomander::suggest_component_connection(shp = pref_n,
    #                                                    adjacency = prefadj)
    #prefadj <- geomander::add_edge(prefadj,
    #                               suggest$x,
    #                               suggest$y,
    #                               zero = TRUE)


    # add bridge between Oshima-Gun and Yanai City
    if(i == 0){
      prefadj <- geomander::add_edge(adjacency = prefadj,
                                     v1 = 10,
                                     v2 = 14)
    } else if(i == 1){
      prefadj <- geomander::add_edge(adjacency = prefadj,
                                     v1 = 14,
                                     v2 = 18)
    } else if(i == 2){
      prefadj <- geomander::add_edge(adjacency = prefadj,
                                     v1 = 19,
                                     v2 = 23)
    }

  }

  # define map
  pref_map <- redist::redist_map(pref_n,
                                 ndists = ndists_new,
                                 pop_tol= 0.15,
                                 total_pop = pop,
                                 adj = prefadj)

  ###### simulation ######
  sim_smc_pref <- redist::redist_smc(pref_map,
                                     nsims = nsims)
  # save it
  saveRDS(sim_smc_pref, paste("simulation/",
                              sprintf("%02d", pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_",
                              as.character(i),
                              ".Rds",
                              sep = ""))

  # get plans
  smc_plans_pref <- redist::get_plans_matrix(sim_smc_pref)

  # get disparity data
  smc_weight_pref <- simulation_weight_disparity_table(sim_smc_pref)

  # make least max_min map

  n <- c(1:nsims)
  smc_weight_pref <- cbind(as.data.frame(n), smc_weight_pref)

  optimal <- smc_weight_pref$n[which(smc_weight_pref$max_to_min == min(smc_weight_pref$max_to_min))]

  #print optimal plan
  optimal_map <- redist::redist.plot.plans(plans = sim_smc_pref,
                                           draws = optimal[1],
                                           geom = pref_map) +
    labs(title = paste(pref_name,
                       "_",
                       sim_type,
                       "_",
                       nsims,
                       "_",
                       i,
                       "split_least_max_to_min_plan",
                       sep = ""),
         subtitle = paste("Max to Min Ratio =",
                          round(min(smc_weight_pref$max_to_min), 4),
                          "\n Loosemore-Hanby index =",
                          round(min(smc_weight_pref$LH), 4),
                          sep = " "),
         caption = paste("#",
                         optimal[1],
                         sep = ""))

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
  assign(paste(pref_name, pref_code,"smc_weight", i, sep = "_"),
         smc_weight_pref)
  assign(paste(pref_name, pref_code,"least_max_min_plan", i, sep = "_"),
         optimal_map)

  rm(list= ls()[(ls() %in% c("pref_n",
                             "prefadj",
                             "pref_map",
                             "sim_smc_pref",
                             "smc_plans_pref",
                             "smc_weight_pref",
                             "ferries",
                             "suggest",
                             "port_data",
                             "route_data",
                             "optimal",
                             "optimal_map")
  )])

}

split0 <- ggplot(yamaguchi_35_0, fill = code)+
  geom_sf()
split1 <- ggplot(yamaguchi_35_1, fill = code)+
  geom_sf()
split2 <- ggplot(yamaguchi_35_2, fill = code)+
  geom_sf()

cowplot::plot_grid(split0, split1, split2)

cowplot::plot_grid(yamaguchi_35_least_max_min_plan_0,
                   yamaguchi_35_least_max_min_plan_1,
                   yamaguchi_35_least_max_min_plan_2)
