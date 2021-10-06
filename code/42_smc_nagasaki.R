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
getOption('timeout')
options(timeout = 240)
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
                                 pop_tol= 0.15,
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
shp = nagasaki_42_2, plan = modified_smc_0[, which(nagasaki_42_smc_weight_0$max_to_min == min(nagasaki_42_smc_weight_0$max_to_min))[3]]
)

#load packages
library(cluster)
library(viridis)
library(network)
library(ggnetwork)

part_parallel_plans_pref <- cbind(modified_smc_0, modified_smc_1, nagasaki_42_smc_plans_2)
part_parallel_weight_pref <- dplyr::bind_rows(nagasaki_42_smc_weight_0, nagasaki_42_smc_weight_1, nagasaki_42_smc_weight_2)

#add column "n" as an indicator of the plans
n <- c(1:ncol(part_parallel_plans_pref))
n <- as.data.frame(n)
wgt_smc_0 <- cbind(n, part_parallel_weight_pref)

#get plans that have a low max:min ratio (Top 10%)
good_num_0 <-  wgt_smc_0 %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(floor(nrow(wgt_smc_0)*0.1))) %>%
  select(n)

good_num_0 <- as.vector(t(good_num_0))

sim_smc_pref_0_good <-
  dplyr::bind_rows(
    redist::redist_plans(part_parallel_plans_pref, nagasaki_42_map_2, algorithm = "smc") %>% filter(draw %in% good_num_0)
  )

#obtain co-occurrence matrix
m_co_0 <- redist::prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

###calculate the centroids of each municipality/gun to plot population size
pref_map_0 <- nagasaki_42_2
pref_map_0$CENTROID <- sf::st_centroid(nagasaki_42_2$geometry)

###Draw lines between municipalities that tend to be in the same district
m_co_sig_0 <- m_co_0
#extract co-occurrence > 90%
rownames(m_co_sig_0) <- pref_map_0$code
colnames(m_co_sig_0) <- pref_map_0$code
m_co_sig_0 <- as_tibble(as.data.frame(as.table(m_co_sig_0)))
m_co_sig_0$Freq <- as.numeric(m_co_sig_0$Freq)

#Clean up dataframe
m_co_sig_0 <- m_co_sig_0 %>%
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>%
  filter(Var1 != Var2, Freq > 0.9)
#Only the municipalities that are in the same district more than 90% of the time are included

#Creat 0 x 3 tibble
m_co_sig_0_adj <- m_co_sig_0
m_co_sig_0_adj <- m_co_sig_0_adj[ !(m_co_sig_0_adj$Var1 %in% m_co_sig_0$Var1), ]

#filter out the co-occurrence between adjacent municipalities
for(i in 1:length(nagasaki_42_2$code)){
  p <- m_co_sig_0 %>%
    filter(Var1 == nagasaki_42_2$code[i]) %>%
    filter(Var2 %in% c(as.character(nagasaki_42_2$code[nagasaki_42_adj_2[[i]]+1])))
  m_co_sig_0_adj <- dplyr::bind_rows(p, m_co_sig_0_adj)
}

#use network package to obtain network
network_0_adj <- network(m_co_sig_0_adj, directed = FALSE, multiple = TRUE)

### Color municipalities that tend to be in the same district
#cluster
cl_co_0 = cluster::agnes(m_co_0)
plot(as.dendrogram(cl_co_0)) # pick a number of clusters from the dendrogram.
prec_clusters_0 = cutree(cl_co_0, ndists_new) # change 6 to the number of clusters you want

relcomp <- function(a, b) {

  comp <- vector()

  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }

  return(comp)
}

pref_part <- nagasaki_42_2
part_adj <- nagasaki_42_adj_2

cooc_ratio <- vector(length = length(pref_part$code))

for (i in 1:length(pref_part$code))
{
  cooc_ratio[i] <- 1 - sum(pref_part$pop[relcomp(part_adj[[i]]+1, which(prec_clusters_0 == prec_clusters_0[i]))] * m_co_0[i, relcomp(part_adj[[i]]+1, which(prec_clusters_0 == prec_clusters_0[i]))])/
    sum(pref_part$pop[part_adj[[i]]+1] * m_co_0[i, part_adj[[i]]+1])
}

pref_block <- merge_gun(pref_part)

pref_part$cluster = prec_clusters_0
pref_part$strength = cooc_ratio

redist::redist.plot.map(pref_part, plan = pref_part$cluster, fill = pref_part$strength) +
  geom_sf(data = pref_block, fill = NA, color = "black", lwd = 0.5)

pref_part %>%
  ggplot() +
  geom_sf(aes(color = cluster, alpha = strength), show.legend = FALSE) +
  geom_sf(data = pref_block, fill = NA, color = "black", lwd = 0.5) +
  theme(legend.box = "vertical",
        legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())


redist::redist.plot.map(pref_part, plan = prec_clusters_0, fill = cooc_ratio) +
  geom_sf(data = pref_block, fill = NA, color = "black", lwd = 0.5)

#convert to tibble
pref_membership_0 <- as_tibble(as.data.frame(prec_clusters_0))
pref_membership_0 <- bind_cols(pref_map_0$code, pref_membership_0)
names(pref_membership_0) <- c("code", "membership")
pref_membership_0$membership <- as.factor(pref_membership_0$membership)

#match membership data with pref_map_0
pref_map_0 <- cbind(pref_map_0, pref_membership_0$membership) %>%
  dplyr::rename(membership = pref_membership_0.membership)

counts <- as.data.frame(unique(cbind(pref_map_0$code, pref_map_0$membership)))

split <- pref_map_0 %>% dplyr::filter(code == 14113)
together <- pref_map_0 %>% dplyr::filter(code != 14113) %>%
  dplyr::group_by(code, membership) %>%
  dplyr::summarize(geometry = sf::st_union(geometry), pop = sum(pop), subcode = "0000")
together$CENTROID <- sf::st_centroid(together$geometry)

pref_map_0 <- rbind(split, together)

pref_map_pop_centroid_0 <- pref_map_0 %>%
  as_tibble() %>%
  dplyr::select(code, CENTROID, pop, geometry) %>%
  separate(CENTROID, into = c("long", "lat"), sep = c(" "))
pref_map_pop_centroid_0$long <- str_remove_all(pref_map_pop_centroid_0$long, "[c(,]")
pref_map_pop_centroid_0$lat <- str_remove_all(pref_map_pop_centroid_0$lat, "[)]")
pref_map_pop_centroid_0$long <- as.numeric(pref_map_pop_centroid_0$long)
pref_map_pop_centroid_0$lat <- as.numeric(pref_map_pop_centroid_0$lat)

#prepare to bind together with network dataframe
lat <- pref_map_pop_centroid_0$lat
names(lat) <- as.character(pref_map_pop_centroid_0$code)
long <- pref_map_pop_centroid_0$long
names(long) <- as.character(pref_map_pop_centroid_0$code)

#Prepare geometry/edges for plotting
geometry_0_adj <- cbind(long[ network.vertex.names(network_0_adj) ],
                        lat[ network.vertex.names(network_0_adj) ])
edges_0_adj <- ggnetwork(network_0_adj, layout = geometry_0_adj, scale = FALSE)

###plot
pref_map_0 %>%
  ggplot() +
  geom_sf(aes(fill = membership), show.legend = FALSE) +
  #size of the circles corresponds to population size in the municipality/gun
  #color of the edges corresponds to the strength of the co-occurrence
  geom_edges(data = edges_0_adj, mapping = aes(x, y, xend = xend, yend = yend, color = Freq),
             size = 0.2) +
  scale_color_gradient(low = "navy", high = "navy") +
  labs(size = "Population (10,000)",
       color = "Co-occurrence",
       title = "Co-occurrence Analysis: Plans with Top 10% Max-min Ratio and Less Splits than SQ",
       caption = "Lines represent co-occurrence between adjacent municipalities.") +
  theme(legend.box = "vertical",
        legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

redist::redist.plot.map(nagasaki_42_2, plan = status_quo$ku)
