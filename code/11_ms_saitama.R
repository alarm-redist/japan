############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)
#remotes::install_github("alarm-redist/redist@dev")
#install.packages("redist")
library(redist)
# pull functions from jcdf
# set working directory to the function folder
setwd("./R")
files.sources <-  list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- information set-up -----------#
# prefectural information
nsims <- 250000
pref_code <- 11
pref_name <- "saitama"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 16
ndists_old <- 15
sq_maxmin <- 1.444
#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
# Iruma and , 11326, 11327
merge_gun_exception <- c(11324)  # enter `c()` if not applicable

######### Download and Clean Census ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code) #first download data

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")

# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

######### Set Population data frame in the smaller level ###########
#clean data and estimate population
pref_2020 <- pref_raw %>%
  # clean data frame
  clean_jcdf() %>%
  # calculate Japanese nationality
  calc_kokumin(dem_pops) %>%
  # estimate small area population
  estimate_2020_pop(census2020) %>%
  dplyr::select(code, pop_estimate, geometry) %>%
  dplyr::rename(pop = pop_estimate)


############## Set County Level Data frame ###################
# Group by municipalities (city and gun)
pref_county <- pref_2020 %>%
  merge_gun(., exception = merge_gun_exception) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Iruma-gun (11326 and 11327) except Miyoshi-cho (11324) based on current plan
iruma <- pref_county %>%
  dplyr::filter(code == 11326 |
                  code == 11327) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

pref_county_manually_edited <- pref_county %>%
  dplyr::filter(!code %in% c(11326,
                             11327)) %>%
  dplyr::bind_rows(.,
                   iruma) %>%
  dplyr::select(code, geometry)

########## Add `county` column to the `pref` data frame ###########
county <- geomander::geo_match(from = pref_2020,
                               to = pref_county_manually_edited,
                               method = "center",
                               tiebreaker = TRUE)

pref <- pref_2020 %>%
  dplyr::mutate(county = county)

pref_boundaries <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
############Simulation Prep########################
#adjacency list
prefadj <- redist::redist.adjacency(pref)

neighbor <- geomander::suggest_neighbors(shp = pref,
                                         adjacency = prefadj)
if(nrow(neighbor) > 0){
  prefadj <- geomander::add_edge(prefadj,
                                 neighbor$x,
                                 neighbor$y,
                                 zero = TRUE)
}

pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                               total_pop = pop,
                               adj = prefadj)



save.image("clean_saitama.Rdata")

load("clean_saitama.Rdata")
####### Mergesplit Simulation ######
sim_type <- "ms"

pref_ms <- redist::redist_mergesplit(
  map = pref_map,
  nsims = nsims,
  counties = pref$county,
  warmup = 0,
  constraints = list(multissplits = list(strength = 100),
                     splits = list(strength = 10))
)

# save it
saveRDS(pref_ms, paste("simulation/",
                       sprintf("%02d", pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(sim_type),
                       "_",
                       as.character(nsims),
                       ".Rds",
                       sep = ""))

#pref_ms <- readRDS(paste("simulation/",
#                         sprintf("%02d", pref_code),
#                         "_",
#                         as.character(pref_name),
#                         "_",
#                         as.character(sim_type),
#                         "_",
#                         as.character(nsims),
#                         ".Rds",
#                         sep = ""))

# get disparity data
weight_pref_ms <- simulation_weight_disparity_table(pref_ms)
plans_pref_ms <- redist::get_plans_matrix(pref_ms)
redist_plans <- redist::redist_plans(plans = plans_pref_ms,
                                     map = pref_map,
                                     algorithm = "ms")

# get splits
pref_ms_splits <- count_splits(plans_pref_ms, pref_map$code)
pref_ms_codesplit <- redist::redist.splits(plans_pref_ms,
                                           pref_map$code)
pref_ms_countiessplit <- redist::redist.splits(plans_pref_ms,
                                               pref_map$county)

pref_ms_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref_ms)))
pref_ms_results$max_to_min <- weight_pref_ms$max_to_min
pref_ms_results$splits <- pref_ms_splits
pref_ms_results$code_split <- pref_ms_codesplit
pref_ms_results$counties_split <- pref_ms_countiessplit
pref_ms_results$draw <- weight_pref_ms$draw

pref_ms_results <- pref_ms_results %>%
  dplyr::group_by(max_to_min, splits, code_split) %>%
  dplyr::summarise(draw = first(draw)) %>%
  dplyr::arrange(splits)

min(pref_ms_results$max_to_min[which(pref_ms_results$splits == pref_ms_results$code_split)])

satisfying_plan_ms <- pref_ms_results %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == code_split) %>%
  dplyr::arrange(max_to_min)

# Draw Map

optimal_matrix_plan_ms <- redist::get_plans_matrix(pref_ms %>%
                                                     filter(draw == satisfying_plan_ms$draw[1]))
colnames(optimal_matrix_plan_ms) <- "district"
optimal_boundary_ms <- cbind(pref, as_tibble(optimal_matrix_plan_ms))

ggplot() +
  geom_sf(data = optimal_boundary_ms, aes(fill = factor(district))) +
  geom_sf(data = pref_boundaries, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), legend.position = "None")+
  scale_fill_manual(values=as.vector(pals::polychrome(ndists_new)))+
  ggtitle(paste("mergeplit #", satisfying_plan_ms$draw[1]," max/min=", satisfying_plan_ms$max_to_min[1], "splits=", satisfying_plan_ms$splits[1]))


##########Co-occurrence ############
#load packages
library(cluster)
library(viridis)
library(network)
library(ggnetwork)

#add column "n" as an indicator of the plans
all <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref_ms)))
all$max_to_min <- weight_pref_ms$max_to_min
all$splits <- pref_ms_splits
all$code_split <- pref_ms_codesplit
all$counties_split <- pref_ms_countiessplit
all$draw <- weight_pref_ms$draw

good_num_0 <- all %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == code_split) %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(floor(nrow(.)*0.1))) %>%
  select(draw)

good_num_0 <- as.vector(t(good_num_0))

sim_smc_pref_0_good <- pref_ms %>%
  filter(draw %in% good_num_0)

#obtain co-occurrence matrix
m_co_0 <- redist::prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

###calculate the centroids of each municipality/gun to plot population size
pref_map_0 <- pref
pref_map_0$CENTROID <- sf::st_centroid(pref$geometry)

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
for(i in 1:length(pref$code)){
  p <- m_co_sig_0 %>%
    filter(Var1 == pref$code[i]) %>%
    filter(Var2 %in% c(as.character(pref$code[prefadj[[i]]+1])))
  m_co_sig_0_adj <- dplyr::bind_rows(p, m_co_sig_0_adj)
}

#use network package to obtain network
network_0_adj <- network(m_co_sig_0_adj, directed = FALSE, multiple = TRUE)

### Color municipalities that tend to be in the same district
#cluster
cl_co_0 = cluster::agnes(m_co_0)
plot(as.dendrogram(cl_co_0)) # pick a number of clusters from the dendrogram.
prec_clusters_0 = cutree(cl_co_0, ndists_new) # change 6 to the number of clusters you want

#convert to tibble
pref_membership_0 <- as_tibble(as.data.frame(prec_clusters_0))
pref_membership_0 <- bind_cols(pref_map_0$code, pref_membership_0)
names(pref_membership_0) <- c("code", "membership")
pref_membership_0$membership <- as.factor(pref_membership_0$membership)

#match membership data with pref_map_0
pref_map_0 <- cbind(pref_map, pref_membership_0$membership) %>%
  dplyr::rename(membership = pref_membership_0.membership)

#counts <- as.data.frame(unique(cbind(pref_map_0$code, pref_map_0$membership)))

#split <- pref_map_0 %>% dplyr::filter(code == 14113)
#together <- pref_map_0 %>% dplyr::filter(code != 14113) %>%
#  dplyr::group_by(code, membership) %>%
#  dplyr::summarize(geometry = sf::st_union(geometry), pop = sum(pop), subcode = "0000")
#together$CENTROID <- sf::st_centroid(together$geometry)

#pref_map_0$CENTROID <- dplyr::group_by(code, membership) %>%
#  dplyr::summarize(geometry = sf::st_union(geometry), pop = sum(pop), subcode = "0000") %>%
#  sf::st_centroid(pref_map_0$geometry)

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
  geom_point(data = pref_map_pop_centroid_0, aes(long, lat, size = 10*pop/10000000),
             color = "grey") +
  #size of the circles corresponds to population size in the municipality/gun
  #color of the edges corresponds to the strength of the co-occurrence
  geom_edges(data = edges_0_adj, mapping = aes(x, y, xend = xend, yend = yend, color = Freq),
             size = 0.2) +
  scale_color_gradient(low = "grey", high = "white") +
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
