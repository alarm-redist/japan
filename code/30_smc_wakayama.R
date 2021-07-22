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
pref_code <- 30
pref_name <- "wakayama"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 2
ndists_old <- 3

#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
nsplit <- 0
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

  pref_n <- avoid_enclave(pref_n, c(30203, 30340))
  pref_n <- avoid_enclave(pref_n, c(30420, 30207))

  #------------- set up map ----------------
  # simulation parameters
  prefadj <- redist::redist.adjacency(shp = pref_n) # Adjacency list

  # add ferry if applicable
  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref_n)
  }

  if(nrow(ferries) > 0) {
    prefadj <- geomander::add_edge(prefadj,
                                 ferries[, 1],
                                 ferries[, 2],
                                 zero = TRUE)

    suggest <-  geomander::suggest_component_connection(shp = pref_n,
                                                        adj = prefadj)
    prefadj <- geomander::add_edge(prefadj,
                                   suggest$x,
                                   suggest$y,
                                   zero = TRUE)

  }

  # define map
  pref_map <- redist::redist_map(pref_n,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj)

  ###### simulation ######
  sim_smc_pref <- redist::redist_smc(pref_map,
                                     nsims = nsims,
                                     pop_temper = 0.05)
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

wakayama_30_optimalmap <- redist::redist.plot.map(shp = wakayama_30_0, plan = wakayama_30_smc_plans_0[, which(wakayama_30_smc_weight_0$max_to_min == min(wakayama_30_smc_weight_0$max_to_min))[1]], boundaries = FALSE, title = "Wakayama Optimal Plan (0-split)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red")) + ggplot2::labs(caption = paste("Max-min Ratio: ", round(min(wakayama_30_smc_weight_0$max_to_min), 3), sep = ""), hjust = 0.5)

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
                        "_optimal.png",
                        sep = ""),
       plot = wakayama_30_optimalmap)

status_quo <- status_quo_match(wakayama_30_0)

wakayama_orig_weight <- simulation_weight_disparity_table(redist::redist_plans(plans = matrix(status_quo$ku, ncol = 1), map = wakayama_30_map_0, algorithm = "smc"))

unique_weights <- wakayama_30_smc_weight_0
unique_weights$n <- 1:nsims
unique_weights <- unique_weights %>%
  dplyr::group_by(max_to_min, Gini, LH, HH) %>% summarize(n = first(n))

wakayama_overlap_0 <- vector(length = nrow(unique_weights))
for (i in 1:length(wakayama_overlap_0)){
  wakayama_overlap_0[i] <- redist::redist.prec.pop.overlap(status_quo$ku, wakayama_30_smc_plans_0[, unique_weights$n[i]], wakayama_30_0$pop,
                                                      weighting = "s", index_only = TRUE)
}


improved_plans <- as.data.frame(
  cbind(unique_weights %>% dplyr::filter(max_to_min < wakayama_orig_weight$max_to_min),

  c(wakayama_overlap_0[which(unique_weights$max_to_min < wakayama_orig_weight$max_to_min)]
  ),

  as.character(rep("0", length(which(unique_weights$max_to_min < wakayama_orig_weight$max_to_min))))
  ))

names(improved_plans) <- c(names(unique_weights), "Dissimilarity", "Splits")


wakayama_marginal <- ggplot(improved_plans, aes(Dissimilarity, max_to_min, colour = Splits)) +
  geom_point(size = 1, alpha = 0.3) + ylim(1.00, wakayama_orig_weight$max_to_min) + ggplot2::ggtitle("Wakayama Dissimilarity vs Max-Min")
ggExtra::ggMarginal(wakayama_marginal, groupColour = TRUE, groupFill = TRUE)

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
                        "_dissim_maxmin.png",
                        sep = ""),
       plot = ggExtra::ggMarginal(wakayama_marginal, groupColour = TRUE, groupFill = TRUE))



##########Co-occurrence ############
#load packages
library(cluster)
library(viridis)
library(network)
library(ggnetwork)

pref_map_0 <- wakayama_30_map_0

#get plans that have a low max:min ratio (Top 10%)
n <- c(1:nsims)
n <- as.data.frame(n)
pref_smc_weight_0 <- cbind(n, wakayama_30_smc_weight_0)

good_num_0 <- pref_smc_weight_0 %>%
  dplyr::arrange(max_to_min) %>%
  dplyr::slice(1: as.numeric(nsims*0.1)) %>%
  dplyr::select(n)

good_num_0 <- as.vector(t(good_num_0))

sim_smc_pref_0_good <- wakayama_30_sim_smc_0 %>%
  filter(draw %in% good_num_0)

#obtain co-occurrence matrix
m_co_0 <- redist::prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

###calculate the centroids of each municipality/gun to plot population size

pref_map_0$CENTROID <- sf::st_centroid(pref_map_0$geometry)
pref_map_pop_centroid_0 <- pref_map_0 %>%
  as_tibble() %>%
  dplyr::select(code, CENTROID, pop) %>%
  separate(CENTROID, into = c("long", "lat"), sep = c(" "))

pref_map_pop_centroid_0$long <- stringr::str_remove_all(pref_map_pop_centroid_0$long, "[c(,]")
pref_map_pop_centroid_0$lat <- stringr::str_remove_all(pref_map_pop_centroid_0$lat, "[)]")
pref_map_pop_centroid_0$long <- as.numeric(pref_map_pop_centroid_0$long)
pref_map_pop_centroid_0$lat <- as.numeric(pref_map_pop_centroid_0$lat)

#prepare to bind together with network dataframe
lat <- pref_map_pop_centroid_0$lat
names(lat) <- as.character(pref_map_pop_centroid_0$code)
long <- pref_map_pop_centroid_0$long
names(long) <- as.character(pref_map_pop_centroid_0$code)

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

pref_0 <- wakayama_30_0
prefadj_0 <- wakayama_30_adj_0
#filter out the co-occurrence between adjacent municipalities
for(i in 1:length(pref_0$code)){
  p <- m_co_sig_0 %>%
    filter(Var1 == pref_0$code[i]) %>%
    filter(Var2 %in% c(as.character(pref_0$code[prefadj_0[[i]]+1])))
  m_co_sig_0_adj <- dplyr::bind_rows(p, m_co_sig_0_adj)
}

#use network package to obtain network
network_0_adj <- network::network(m_co_sig_0_adj, directed = FALSE, multiple = TRUE)
#Prepare geometry/edges for plotting
geometry_0_adj <- cbind(long[ network::network.vertex.names(network_0_adj) ],
                        lat[ network::network.vertex.names(network_0_adj) ])
edges_0_adj <- ggnetwork::ggnetwork(network_0_adj, layout = geometry_0_adj, scale = FALSE)

### Color municipalities that tend to be in the same district
#cluster
cl_co_0 = cluster::agnes(m_co_0)
plot(as.dendrogram(cl_co_0)) # pick a number of clusters from the dendrogram.
prec_clusters_0 = cutree(cl_co_0, 2) # change 6 to the number of clusters you want
#convert to tibble
pref_membership_0 <- as_tibble(as.data.frame(prec_clusters_0))
pref_membership_0 <- bind_cols(pref_map_0$code, pref_membership_0)
names(pref_membership_0) <- c("code", "membership")
pref_membership_0$membership <- as.factor(pref_membership_0$membership)
#match membership data with pref_map_0
pref_map_0 <- merge(pref_map_0, pref_membership_0, by = "code")

###plot
pref_map_0 %>%
  ggplot() +
  geom_sf(aes(fill = membership), show.legend = FALSE) +
  scale_fill_manual(values= c("1" = "blue", "2" = "red")) +
  #size of the circles corresponds to population size in the municipality/gun
  geom_point(data = pref_map_pop_centroid_0, aes(long, lat, size = 10*pop/100000),
             color = "grey") +
  #color of the edges corresponds to the strength of the co-occurrence
  geom_edges(data = edges_0_adj, mapping = aes(x, y, xend = xend, yend = yend, color = Freq),
             size = 0.8) +
  scale_color_gradient(low = "white", high = "navy") +
  labs(size = "Population (10,000)",
       color = "Co-occurrence",
       title = "Co-occurrence Analysis: Plans with Top 10% Max-min Ratio",
       caption = "Lines represent co-occurrence between adjacent municipalities.") +
  theme(legend.box = "vertical",
        legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

