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
pref_code <- 04
pref_name <- "miyagi"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 5
ndists_old <- 6

#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
nsplit <- 2
merge_gun_exception <- c(4401, 4421)  # enter `c()` if not applicable #宮城郡 #黒加賀郡

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

# fix 富谷町 --> 富谷市 (2016 changes)
pref[which(pref$code == 4423), ]$pop <-  (census2020 %>% dplyr::filter(code == 4216, ))$pop_national
pref[which(pref$code == 4423), ]$code <- 4216

# remove lake
ifelse(is.null(lakes_removed),
       pref <- pref,
       pref <- remove_lake(pref, lakes_removed))

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

# the code of split municipalities
split_codes <- c(4202, 4215)
#cannot split 仙台市区部 -> Largest municipalities that can be split are
  #04202 石巻市 (139,097)
  #04215 大崎市 (126,551)
intact_codes <- c()

####save(list=ls(all=TRUE), file="04_smc_miyagi_data_raw.Rdata")

pref <- st_as_sf(pref)

##########0 split (0 gun split) ###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#Ferries
#ferries_0 <- add_ferries(pref_0)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list
#add edge
#prefadj_0 <- geomander::add_edge(prefadj_0, ferries_0$V1, ferries_0$V2)

pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.08,
                                 total_pop = pop,
                                 adj = prefadj_0)

#save(list=ls(all=TRUE), file="04_smc_miyagi_data_0split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_0 <- redist::redist_smc(pref_map_0,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_0, paste("simulation/",
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

###############1 split (with merge_gun exceptions)##################
pref <- st_as_sf(pref)
pref_1 <- pref

# -------- Merge gun (with exceptions) ------------#
pref_1 <- merge_gun(pref_1, c(4401, 4421))

# -------- Old boundary ------------#
old_04202 <- find_old_codes("04202", pop_by_old_boundary)
#4202 4561 4563 4564 4565 4567 4582

######=====Fix bug====##########
sf::st_crs(old_boundary) <- sf::st_crs(pref_1)

#filter out the municipalities that will not be split
#(i.e. no need to take into account old boundaries)
post_gappei_except_for_designated_city <- pref_1 %>%
  dplyr::filter(code != 4202) %>%
  dplyr::select(code, pop, geometry)

#Clean the data on old boundaries
pre_gappei_geom <- old_boundary %>%
  dplyr::filter(N03_007 %in% c("04202", "04561", "04563", "04564", "04565", "04567",  "04582")) %>%
  dplyr::select(N03_004, N03_007, geometry)
names(pre_gappei_geom) <- c("municipality", "code", "geometry")
pre_gappei_geom <- sf::st_make_valid(pre_gappei_geom)
pre_gappei_geom <- pre_gappei_geom %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry =  sf::st_union(geometry))

#get rid of unnecessary rows and columns
cleaned_pop_by_old_boundary <- pop_by_old_boundary %>%
  slice(-c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
  select(X, X.1, X.4, X.5)
#column X: municipality code; X.1:shikibetsu code(9 corresponds to the old municipalities)
#column X.4: name of old municipality; X.5: population as of 2015

#select the data we are interested in
pop_data <- cleaned_pop_by_old_boundary %>%
  filter(X %in% "04202") %>%
  filter(X.1 %in% 9) %>%
  #"9" corresponds to the old municipalities (cf.shikibetsu code)
  select(X, X.4, X.5)
names(pop_data) <- c("null_code", "municipality", "pop")
#obtain the last three digits of the old municipality codes
pop_data <- pop_data %>%
  separate(municipality, into = c("a", "old_code", "b"), sep = " ") %>%
  select(old_code, pop)

#generate functioning municipality codes
#(i.e. add the first two digits)
pref_10<- pref$code[1] %/% 1000 #prefecture_code
a <- lapply(pref_10,  paste0, pop_data$old_code)
b <- as.data.frame(a, col.names = "code")

#final version of 2015(2020) population based on old boundaries
pop_data <- bind_cols(b, pop_data)

#match with dataframes based on municipality code
pre_gappei_geom$code <- as.numeric(pre_gappei_geom$code)
old_joined <- merge(pre_gappei_geom, pop_data, by = "code")
old_joined_simp <- old_joined %>%
  select(code, pop, geometry)
old_joined_simp$code <- as.numeric(old_joined_simp$code)
old_joined_simp$pop <- as.numeric(old_joined_simp$pop)

#merge with the data that excludes the designated city
merged <- dplyr::bind_rows(old_joined_simp, post_gappei_except_for_designated_city)


########=====Continuing 1 split=======#######
pref_1 <- merged

#Estimate 2020 pop based on old boundary
pref_1 <- estimate_old_boundary_pop(old_04202,
                                    4202, pref_1, census2020)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_1 <- redist::redist.adjacency(pref_1) # Adjacency list

pref_map_1 <- redist::redist_map(pref_1,
                                 ndists = ndists_new,
                                 pop_tol= 0.06,
                                 total_pop = pop,
                                 adj = prefadj_1)

#save(list=ls(all=TRUE), file="04_smc_miyagi_data_1split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_1 <- redist::redist_smc(pref_map_1,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_1, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_1",
                              ".Rds",
                              sep = ""))

##########Two split###################
pref_2 <- pref_1

# -------- Old boundary ------------#
old_04215 <- find_old_codes("04215", pop_by_old_boundary)
#4204 4461 4462 4463 4481 4482 4502

######=====Fix bug====##########
sf::st_crs(old_boundary) <- sf::st_crs(pref_2)

#filter out the municipalities that will not be split
#(i.e. no need to take into account old boundaries)
post_gappei_except_for_designated_city <- pref_2 %>%
  dplyr::filter(code != 4215) %>%
  dplyr::select(code, pop, geometry)

#Clean the data on old boundaries
pre_gappei_geom <- old_boundary %>%
  dplyr::filter(N03_007 %in% c("04204", "04461", "04462", "04463", "04481", "04482", "04502")) %>%
  dplyr::select(N03_004, N03_007, geometry)
names(pre_gappei_geom) <- c("municipality", "code", "geometry")
pre_gappei_geom <- sf::st_make_valid(pre_gappei_geom)
pre_gappei_geom <- pre_gappei_geom %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry =  sf::st_union(geometry))

#get rid of unnecessary rows and columns
cleaned_pop_by_old_boundary <- pop_by_old_boundary %>%
  slice(-c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
  select(X, X.1, X.4, X.5)
#column X: municipality code; X.1:shikibetsu code(9 corresponds to the old municipalities)
#column X.4: name of old municipality; X.5: population as of 2015

#select the data we are interested in
pop_data <- cleaned_pop_by_old_boundary %>%
  filter(X %in% "04215") %>%
  filter(X.1 %in% 9) %>%
  #"9" corresponds to the old municipalities (cf.shikibetsu code)
  select(X, X.4, X.5)
names(pop_data) <- c("null_code", "municipality", "pop")
#obtain the last three digits of the old municipality codes
pop_data <- pop_data %>%
  separate(municipality, into = c("a", "old_code", "b"), sep = " ") %>%
  select(old_code, pop)

#generate functioning municipality codes
#(i.e. add the first two digits)
pref_10<- pref$code[1] %/% 1000 #prefecture_code
a <- lapply(pref_10,  paste0, pop_data$old_code)
b <- as.data.frame(a, col.names = "code")

#final version of 2015(2020) population based on old boundaries
pop_data <- bind_cols(b, pop_data)

#match with dataframes based on municipality code
pre_gappei_geom$code <- as.numeric(pre_gappei_geom$code)
old_joined <- merge(pre_gappei_geom, pop_data, by = "code")
old_joined_simp <- old_joined %>%
  select(code, pop, geometry)
old_joined_simp$code <- as.numeric(old_joined_simp$code)
old_joined_simp$pop <- as.numeric(old_joined_simp$pop)

#merge with the data that excludes the designated city
merged <- dplyr::bind_rows(old_joined_simp, post_gappei_except_for_designated_city)


########=====Continuing 1 split=======#######
pref_2 <- merged

#Estimate 2020 pop based on old boundary
pref_2 <- estimate_old_boundary_pop(old_04215,
                                    4215, pref_2, census2020)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_2 <- redist::redist.adjacency(pref_2) # Adjacency list

pref_map_2 <- redist::redist_map(pref_2,
                                 ndists = ndists_new,
                                 pop_tol= 0.05,
                                 total_pop = pop,
                                 adj = prefadj_2)

#save(list=ls(all=TRUE), file="04_smc_miyagi_data_2split.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_2 <- redist::redist_smc(pref_map_2,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_2, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_2",
                              ".Rds",
                              sep = ""))


########Analysis#####################
# extract plans
pref_smc_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)
pref_smc_plans_1 <- redist::get_plans_matrix(sim_smc_pref_1)
pref_smc_plans_2 <- redist::get_plans_matrix(sim_smc_pref_2)

# get disparity table data
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_smc_0 <- cbind(n, wgt_smc_0)
#wgt_smc_0$n[which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))]
#Maxmin 1.1117 #  1    6    7    8   12  ...
#redist::redist.plot.plans(sim_smc_pref_0, draws = 1, geom = pref_map_0)

miyagi_4_optimalmap_0 <- redist::redist.plot.map(shp = pref_0,
                                                 plan = pref_smc_plans_0[, which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))[1]],
                                                 boundaries = FALSE,
                                                 title = "Miyagi Optimal Plan (0 Municipality split; \n0 Gun split)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green", "5" = "orange")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_0$max_to_min), 3), sep = ""), hjust = 0.5)

wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1)
#wgt_smc_1 <- cbind(n, wgt_smc_1)
#wgt_smc_1$n[which(wgt_smc_1$max_to_min == min(wgt_smc_1$max_to_min))]
#Maxmin 1.043 #6   130   134
#redist::redist.plot.plans(sim_smc_pref_1, draws = 6, geom = pref_map_1)

miyagi_4_optimalmap_1 <- redist::redist.plot.map(shp = pref_1,
                                                 plan = pref_smc_plans_1[, which(wgt_smc_1$max_to_min == min(wgt_smc_1$max_to_min))[1]],
                                                 boundaries = FALSE,
                                                 title = "Miyagi Optimal Plan (1 Municipality split; \n2 Gun splits)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green", "5" = "orange")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_1$max_to_min), 3), sep = ""), hjust = 0.5)

wgt_smc_2 <- simulation_weight_disparity_table(sim_smc_pref_2)
#wgt_smc_2 <- cbind(n, wgt_smc_2)
#wgt_smc_2$n[which(wgt_smc_2$max_to_min == min(wgt_smc_2$max_to_min))]
#Maxmin 1.0386 #3986
#redist::redist.plot.plans(sim_smc_pref_2, draws = 3986, geom = pref_map_2)

miyagi_4_optimalmap_2 <- redist::redist.plot.map(shp = pref_2,
                                                 plan = pref_smc_plans_2[, which(wgt_smc_2$max_to_min == min(wgt_smc_2$max_to_min))[1]],
                                                 boundaries = FALSE,
                                                 title = "Miyagi Optimal Plan (2 Municipality splits; \n2 Gun splits)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow", "4" = "green", "5" = "orange")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_2$max_to_min), 3), sep = ""), hjust = 0.5)


#save(list=ls(all=TRUE), file="34_smc_hiroshima_data_0to4splits.Rdata")

##########Co-occurrence ############
#load packages
library(cluster)
library(network)
library(ggnetwork)

#get plans that have a low max:min ratio (Top 10%)
good_num_0 <-  wgt_smc_0 %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(nsims*0.1)) %>%
  select(n)
good_num_0 <- as.vector(t(good_num_0))
sim_smc_pref_0_good <- sim_smc_pref_0 %>%
  filter(draw %in% good_num_0)
#obtain co-occurrence matrix
m_co_0 = prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

###calculate the centroids of each municipality/gun to plot population size
pref_map_0$CENTROID <- sf::st_centroid(pref_map_0$geometry)
pref_map_pop_centroid_0 <- pref_map_0 %>%
  as_tibble() %>%
  dplyr::select(code, CENTROID, pop) %>%
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
for(i in 1:length(pref_0$code)){
  p <- m_co_sig_0 %>%
    filter(Var1 == pref_0$code[i]) %>%
    filter(Var2 %in% c(as.character(pref_0$code[prefadj_0[[i]]+1])))
  m_co_sig_0_adj <- dplyr::bind_rows(p, m_co_sig_0_adj)
}

#use network package to obtain network
network_0_adj <- network(m_co_sig_0_adj, directed = FALSE, multiple = TRUE)

#Prepare geometry/edges for plotting
geometry_0_adj <- cbind(long[ network.vertex.names(network_0_adj) ],
                        lat[ network.vertex.names(network_0_adj) ])
edges_0_adj <- ggnetwork(network_0_adj, layout = geometry_0_adj, scale = FALSE)

### Color municipalities that tend to be in the same district
#cluster
cl_co_0 = cluster::agnes(m_co_0)
plot(as.dendrogram(cl_co_0)) # pick a number of clusters from the dendrogram.
prec_clusters_0 = cutree(cl_co_0, 5) # change 6 to the number of clusters you want

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
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow",
                              "4" = "green", "5" = "orange")) +
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
