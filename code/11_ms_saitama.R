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
koiki_code <- geomander::geo_match(from = pref_2020,
                               to = pref_county_manually_edited,
                               method = "center",
                               tiebreaker = TRUE)

pref <- pref_2020 %>%
  dplyr::mutate(koiki_code = koiki_code) %>%
  merge_gun(., exception = merge_gun_exception)

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

####### Mergesplit Simulation ######
sim_type <- "ms"

pref_ms <- redist::redist_mergesplit(
  map = pref_map,
  nsims = nsims,
  counties = pref$gun_code,
  warmup = 0,
  constraints = list(multissplits = list(strength = 100),
                     splits = list(strength = 10))
)

library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
ggplot(pref) +
  geom_sf(aes(fill = as.factor(gun_code), label = FALSE))+
  scale_fill_manual(values=as.vector(col_vector))

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

# II. Analysis
## 1. Get Optimal Plan
### 1.1 Extract Plans

index <- vector(length = nrow(pref))
for (i in 1:nrow(pref))
{
  if (pref$code[i] %in% pref$code)
  {
    index[i] = which(pref$code == pref$code[i])[1]
  }
  else
  {
    index[i] = which(pref$gun_code == pref$gun_code[i])[1]
  }
}
pref_ms_plans <- redist::get_plans_matrix(pref_ms)
pref_ms_indexed <- pref_ms_plans[index, ]
prefadj <- redist::redist.adjacency(pref)


### 1.2 Calculate Max:min ratio

wgt_ms <- simulation_weight_disparity_table(pref_ms)


orig_adj <- redist::redist.adjacency(pref)

### 1.3 Count municipality/gun/koiki renkei splits

num_gun_split <- count_splits(pref_ms_plans, pref_map$gun_code)
gun_split <- redist::redist.splits(pref_ms_plans, pref_map$gun_code)
num_koiki_split <- count_splits(pref_ms_plans, pref_map$koiki_code)
koiki_split <- redist::redist.splits(pref_ms_plans, pref_map$koiki_code)

### 1.4 Compile Results

results <- data.frame(matrix(ncol = 0, nrow = nrow(pref_ms)))
results$max_to_min <- wgt_ms$max_to_min
#number of gun splits
results$num_gun_split <- num_gun_split
results$gun_split <- gun_split
#number of koiki renkei area splits
results$num_koiki_split <- num_koiki_split
results$koiki_split <- koiki_split
results$index <- 1:nrow(wgt_ms)
results$contiguous <- 0
for (i in 1:nrow(wgt_ms))
{
  results$contiguous[i] <- max(geomander::check_contiguity(prefadj, pref_ms_indexed[, i])$component) == 1
}

### 1.5  Optimal Plan
contiguous_results <- results[which(results$contiguous == 1), ]
rownames(contiguous_results) <- 1:nrow(contiguous_results)
optimal <- contiguous_results$index[which(contiguous_results$max_to_min == min(contiguous_results$max_to_min))][1]

# 2. Visualize Optimal Plan
## 2.1 0 splits
#get data on optimal plan
pref_boundaries <- pref %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

matrix_optimal <- redist::get_plans_matrix(pref_ms %>% filter(draw == optimal))
colnames(matrix_optimal) <- "district"
optimal_boundary <- cbind(pref_map, as_tibble(matrix_optimal))
#get data on gun boundary
gun_boundary <- pref %>%
  filter(gun_code >= (pref_map$code[1]%/%1000)* 1000 + 300) %>%
  group_by(gun_code) %>%
  summarise(geometry = sf::st_union(geometry))
#get data on koiki boundary
koiki_boundary <- pref %>%
  filter(koiki_code < 10) %>%
  group_by(koiki_code) %>%
  summarise(geometry = sf::st_union(geometry))
#map with district data + municipality/gun/koiki-renkei boundary
ggplot() +
  geom_sf(data = optimal_boundary, aes(fill = factor(district))) +
  scale_fill_manual(values=as.vector(pals::polychrome(ndists_new)))+
  geom_sf(data = pref_boundaries, fill = NA, color = "black", lwd = 0.5) +
  geom_sf(data = gun_boundary, fill = NA, color = "black", lwd = 1.0) +
  geom_sf(data = koiki_boundary, fill = "plum1", alpha = 0.5, color = "plum1", lwd = 0.2) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())


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
pref_map_0 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarize(geometry = sf::st_union(geometry), pop = sum(pop), county = first(county))
pref_map_0$CENTROID <- sf::st_centroid(pref_map_0$geometry)

###Draw lines between municipalities that tend to be in the same district
m_co_sig_0 <- m_co_0
#extract co-occurrence > 90%
rownames(m_co_sig_0) <- pref$code
colnames(m_co_sig_0) <- pref$code
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
for(i in 1:length(pref_map_0$code)){
  p <- m_co_sig_0 %>%
    filter(Var1 == pref_map_0$code[i]) %>%
    filter(Var2 %in% c(as.character(pref_map_0$code[prefadj[[i]]+1])))
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
pref_membership_0 <- bind_cols(pref$code, pref_membership_0)
names(pref_membership_0) <- c("code", "membership")
pref_membership_0$membership <- as.factor(pref_membership_0$membership)

#match membership data with pref_map_0
pref_map_0 <- cbind(pref_map, pref_membership_0$membership) %>%
  dplyr::rename(membership = pref_membership_0.membership) %>%
  dplyr::group_by(code, membership) %>%
  dplyr::summarize(geometry = sf::st_union(geometry), pop = sum(pop))
pref_map_0$CENTROID <- sf::st_centroid(pref_map_0$geometry)

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


#### Cooccurence Visualisation#####
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

cooc_ratio <- vector(length = length(pref$code))

for (i in 1:length(pref$code))
{
  cooc_ratio[i] <- 1 - sum(pref$pop[relcomp(prefadj[[i]]+1, which(prec_clusters_0 == prec_clusters_0[i]))] * m_co_0[i, relcomp(prefadj[[i]]+1, which(prec_clusters_0 == prec_clusters_0[i]))])/
    sum(pref$pop[prefadj[[i]]+1] * m_co_0[i, prefadj[[i]]+1])
}

pref_block <- merge_gun(pref)

pref$cluster = prec_clusters_0
pref$strength = cooc_ratio

redist::redist.plot.map(pref, plan = pref$cluster, fill = pref$strength) +
  geom_sf(data = pref_block, fill = NA, color = "black", lwd = 0.5)

pref %>%
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

redist::redist.plot.map(pref, plan = prec_clusters_0, fill = cooc_ratio) +
  geom_sf(data = pref_block, fill = NA, color = "black", lwd = 0.5)+
  scale_fill_manual(values=as.vector(pals::polychrome(ndists_new)))
