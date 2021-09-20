############# set up ###############
#-------------- functions set up ---------------
library(tidyverse)
set.seed(12345)

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
pref_code <- 38
pref_name <- "ehime"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4

######### Download and Clean Data ############
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

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

#########Analysis################
pref_0 %>%
  dplyr::arrange(desc(pop)) %>%
  ggplot(aes(x = reorder(as.factor(code), -pop ), y = pop)) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = sum(pref_0$pop)/ndists_new), color = "red") +
  annotate("text", x = 5, y = 490000,
           label = "Target\npop.", color = "red")+
  labs(x = NULL,
       y = "Population",
       title = "Population Distribution in Ehime") +
  coord_flip()
##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#ferries
ferries_0 <- add_ferries(pref_0)
#connect [12]38340 上島町 to [2]38202今治市

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list

#add edge(#connect [12]38340 上島町 to [2]38202今治市)
prefadj_0 <- geomander::add_edge(prefadj_0, 2, 12)


pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_0)

#save(list=ls(all=TRUE), file="38_smc_ehime_data_0split.Rdata")

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

###############1 split##################
pref_1 <- pref

# -------- Merge gun (0 exceptions) ------------#
pref_1 <- merge_gun(pref_1)

# -------- Old boundary ------------#
old_38201 <- find_old_codes(38201, pop_by_old_boundary)
#松山市: 38201(旧松山市) 38211（旧北条市） 38363（旧中島町）
pref_1 <- reflect_old_boundaries(pref_1, old_boundary, pop_by_old_boundary, old_38201, 38201)

#Estimate 2020 pop based on old boundary
pref_1 <- estimate_old_boundary_pop(old_38201, 38201, pref_1, census2020)

#Ferries
ferries_1 <- add_ferries(pref_1)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_1 <- redist::redist.adjacency(pref_1) # Adjacency list
#add edge
prefadj_1 <- geomander::add_edge(prefadj_1, 1, 3)  #connect [3]338363 旧中島町 to [1]38201旧松山市
prefadj_1 <- geomander::add_edge(prefadj_1, 4, 14) #connect [14]38340 上島町 to [4]38202今治市

pref_map_1 <- redist::redist_map(pref_1,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_1)

###save(list=ls(all=TRUE), file="38_smc_ehime_data_1split.Rdata")

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

########Analysis###############
# extract plans
pref_smc_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)
pref_smc_plans_1 <- redist::get_plans_matrix(sim_smc_pref_1)

# get disparity table data
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_smc_0 <- cbind(n, wgt_smc_0)
#wgt_smc_0$n[which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))]
#Maxmin 1.412 #Every single plan
#redist::redist.plot.plans(sim_smc_pref_0, draws = 1, geom = pref_map_0)

ehime_38_optimalmap_0 <- redist::redist.plot.map(shp = pref_0,
                                                 plan = pref_smc_plans_0[, which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))[1]],
                                                 boundaries = FALSE,
                                                 title = "Ehime Optimal Plan (0-split)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_0$max_to_min), 3), sep = ""), hjust = 0.5)

wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1)
#wgt_smc_1 <- cbind(n, wgt_smc_1)
#wgt_smc_1$n[which(wgt_smc_1$max_to_min == min(wgt_smc_1$max_to_min))]
#Maxmin 1.3388 # 2    4    5    6    7    8    9   10   etc.
#divided over whether to split 松山市 or not
#redist::redist.plot.plans(sim_smc_pref_1, draws = 2, geom = pref_map_1)

ehime_38_optimalmap_1 <- redist::redist.plot.map(shp = pref_1,
                                                plan = pref_smc_plans_1[, which(wgt_smc_1$max_to_min == min(wgt_smc_1$max_to_min))[1]],
                                                boundaries = FALSE,
                                                title = "Ehime Optimal Plan (1-split)") +
  scale_fill_manual(values= c("1" = "blue", "2" = "red", "3" = "yellow")) +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_1$max_to_min), 3), sep = ""), hjust = 0.5)


###save(list=ls(all=TRUE), file="38_smc_ehime_data_0to1split.Rdata")

############measuring compactness#################
#------status quo---------#
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
ferries <- add_ferries(pref)
prefadj <- redist::redist.adjacency(pref)
prefadj <- geomander::add_edge(prefadj, ferries$V1, ferries$V2)

nedge <- as.numeric(length(unlist(prefadj)))

pref_cd <- status_quo_match(pref)
n_rem_orig <- redist::redist.compactness(shp = pref,
                                         plans = pref_cd$ku, #####???#####
                                         measure = c("EdgesRemoved"),
                                         adj = prefadj)[1, ]$EdgesRemoved
ecc_orig <- n_rem_orig/nedge

#------simulated plans---------#
sim_smc_pref_0_plans <- redist::get_plans_matrix(sim_smc_pref_0)
nedge_0 <- as.numeric(length(unlist(prefadj_0)))
n_rem_0 <- (redist::redist.compactness(shp = pref_0,
                                        plans = sim_smc_pref_0_plans,
                                        measure = c("EdgesRemoved"),
                                        adj = prefadj_0) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_0 <- n_rem_0/nedge_0

sim_smc_pref_1_plans <- redist::get_plans_matrix(sim_smc_pref_1)
nedge_1 <- as.numeric(length(unlist(prefadj_1)))
n_rem_1 <- (redist::redist.compactness(shp = pref_1,
                                       plans = sim_smc_pref_1_plans,
                                       measure = c("EdgesRemoved"),
                                       adj = prefadj_1) %>% group_by(draw) %>% summarize(EdgesRemoved = mean(EdgesRemoved)))$EdgesRemoved
ecc_ms_1 <- n_rem_1/nedge_1


##########Co-occurrence ############
#load packages
library(cluster)
library(viridis)
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
prec_clusters_0 = cutree(cl_co_0, 3) # change 6 to the number of clusters you want

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
                              "4" = "green", "5" = "orange", "6" = "brown")) +
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
