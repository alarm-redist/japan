############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)
remotes::install_github("alarm-redist/redist@dev")

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
pref_code <- 42
pref_name <- "nagasaki"
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
  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref_n)
    prefadj <- geomander::add_edge(prefadj,
                                   ferries[, 1],
                                   ferries[, 2],
                                   zero = TRUE)

  }

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
                              ".Rds",
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
  assign(paste(pref_name, pref_code,"smc_weight", i, sep = "_"),
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

# ------ Analysis ------- #

#Import Statements
# import as files, if necessary
nagasaki_42_sim_smc_0 <- readRDS("simulation/42_nagasaki_smc_25000_0.Rds")
nagasaki_42_sim_smc_1 <- readRDS("simulation/42_nagasaki_smc_25000_1.Rds")
nagasaki_42_sim_smc_2 <- readRDS("simulation/42_nagasaki_smc_25000_2.Rds")

# extract plans
nagasaki_42_smc_plans_0 <- redist::get_plans_matrix(nagasaki_42_sim_smc_0)
nagasaki_42_smc_plans_1 <- redist::get_plans_matrix(nagasaki_42_sim_smc_1)
nagasaki_42_smc_plans_2 <- redist::get_plans_matrix(nagasaki_42_sim_smc_2)

# get disparity table data
nagasaki_42_smc_weight_0 <- simulation_weight_disparity_table(nagasaki_42_sim_smc_0)
nagasaki_42_smc_weight_1 <- simulation_weight_disparity_table(nagasaki_42_sim_smc_1)
nagasaki_42_smc_weight_2 <- simulation_weight_disparity_table(nagasaki_42_sim_smc_2)

# Cooccurence analysis
status_quo <- status_quo_match(nagasaki_42_2)

# establish keys to map 0-split, 1-split plans to 2-split plans
key_0 <- vector(length = length(nagasaki_42_2$code))

old_codes <-
  list(find_old_codes(split_codes[1], pop_by_old_boundary),
     find_old_codes(split_codes[2], pop_by_old_boundary))

for (i in 1:length(nagasaki_42_2$code)) {
  if (nagasaki_42_2$code[i] %in% old_codes[[1]]) {key_0[i] <- split_codes[1]}
  else if (nagasaki_42_2$code[i] %in% old_codes[[2]]) {key_0[i] <- split_codes[2]}
  else {key_0[i] <- nagasaki_42_2$code[i]}
}


# map 0-split plans to 2-split plans
modified_smc_0 <- matrix(0, nrow = dim(nagasaki_42_smc_plans_2)[1],
                         ncol = dim(nagasaki_42_smc_plans_0)[2])

for (i in 1:dim(nagasaki_42_smc_plans_2)[1]) {
  if (nagasaki_42_2$code[i] %in% nagasaki_42_0$code) {modified_smc_0[i, ] <-
    nagasaki_42_smc_plans_0[which(nagasaki_42_0$code == nagasaki_42_2$code[i]), ]}
  else {modified_smc_0[i, ] <- nagasaki_42_smc_plans_0[which(nagasaki_42_0$code == key_0[i]), ]}
}

# map 1-split plans to 2-split plans
modified_smc_1 <- matrix(0, nrow = dim(nagasaki_42_smc_plans_2)[1],
                         ncol = dim(nagasaki_42_smc_plans_1)[2])

for (i in 1:dim(nagasaki_42_smc_plans_2)[1]) {
  if (nagasaki_42_2$code[i] %in% nagasaki_42_1$code) {modified_smc_1[i, ] <-
    nagasaki_42_smc_plans_1[which(nagasaki_42_1$code == nagasaki_42_2$code[i]), ]}
  else {modified_smc_1[i, ] <- nagasaki_42_smc_plans_1[which(nagasaki_42_1$code == key_0[i]), ]}
}

unique_weights <- dplyr::bind_rows(nagasaki_42_smc_weight_0, nagasaki_42_smc_weight_1, nagasaki_42_smc_weight_2)
unique_weights$n <- rep(1:(nsims*(nsplit+1)))
unique_weights$splits <- c(rep(0, nsims), count_splits(modified_smc_1, key_0), count_splits(nagasaki_42_smc_plans_2, key_0))
unique_weights <- unique_weights %>%
  dplyr::group_by(max_to_min, Gini, LH, HH, splits) %>% dplyr::summarize(n = first(n))

overlap_smc <- vector(length = nrow(unique_weights))

for (i in 1:length(overlap_smc)){
  if (unique_weights$n[i] <= nsims) {overlap_smc[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_smc_0[, i %% nsims], nagasaki_42_2$pop,
                                                                     weighting = "s", index_only = TRUE)}
  else if (unique_weights$n[i] > nsims & unique_weights$n[i] <= 2*nsims) {overlap_smc[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_smc_1[, i %% nsims], nagasaki_42_2$pop,
                                                                                         weighting = "s", index_only = TRUE)}
  else {overlap_smc[i] <- redist::redist.prec.pop.overlap(status_quo$ku, nagasaki_42_smc_plans_2[, i %% nsims], nagasaki_42_2$pop,
                                                          weighting = "s", index_only = TRUE)}
}


nagasaki_orig_weight <- simulation_weight_disparity_table(redist::redist_plans(plans = matrix(status_quo$ku, ncol = 1), map = nagasaki_42_map_2, algorithm = "smc"))

# set parameters

improved_plans <- as.data.frame(
  cbind(unique_weights[which(unique_weights$max_to_min < nagasaki_orig_weight$max_to_min), ],

  overlap_smc[which(unique_weights$max_to_min < nagasaki_orig_weight$max_to_min)])
)

names(improved_plans) <- c(names(unique_weights), "Dissimilarity")

improved_plans$splits <- as.character(improved_plans$splits)

plot_smc <- ggplot(improved_plans, aes(Dissimilarity, max_to_min, colour = splits)) +
  geom_point(size = 1, alpha = 0.3) + ggplot2::ggtitle("Nagasaki Dissimilarity vs Max-Min")
ggExtra::ggMarginal(plot_smc, groupColour = TRUE, groupFill = TRUE)

unique_weights %>% dplyr::group_by(splits) %>% dplyr::summarize(min(max_to_min))

redist::redist.plot.map(
shp = nagasaki_42_2, plan = modified_smc_0[, which(nagasaki_42_smc_weight_0$max_to_min == min(nagasaki_42_smc_weight_0$max_to_min))[1]]
)
