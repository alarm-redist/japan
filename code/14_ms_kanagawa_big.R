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
pref_code <- 14
pref_name <- "kanagawa"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 20
ndists_old <- 18

#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
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
  clean_jcdf()

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

####### Simulation by number of splits#######

pref_block <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

pref_block <- merge_gun(pref_block)
pref_block$subcode = "0000"
block_adj <- redist::redist.adjacency(pref_block)

###### simulation ######
small_units <- pref %>% dplyr::select(code, KIHON1, JINKO, geometry)
small_units <- calc_kokumin(small_units, dem_pops)
small_units <- estimate_2020_pop(small_units, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#------------- set up map ----------------
unique(part_map$code) <- pref_block$code
muni_codes <- unique(small_units$code)

# Yokohama-shi, Kawasaki-shi, Sagami-hara-shi + rest
blocks <- list(unique(part_map$code)[which(unique(part_map$code) <= 14120)],
                    unique(part_map$code)[which(unique(part_map$code) >= 14120 & unique(part_map$code) <= 14140)],
                    unique(part_map$code)[which(unique(part_map$code) >= 14150)])

freeze <- intersect(pref_block$code[which(pref_block$pop <= 1/3 * sum(pref_block$pop)/ndists_new)], muni_codes)

"%ni%" <- Negate("%in%")

for(i in 1:1) {

  pref_part <- dplyr::bind_rows(small_units %>% dplyr::filter(code %ni% union(freeze, setdiff(unique(small_units$code), unique(pref_block$code)))),
                                pref_block %>% dplyr::filter(code %in% union(freeze, unique(pref_block$code)[45:50])))
  part_adj <- redist::redist.adjacency(shp = pref_part) # Adjacency list

  neighbor <- geomander::suggest_neighbors(shp = pref_part,
                                               adjacency = part_adj)
  if(nrow(neighbor) > 0) {

    part_adj <- geomander::add_edge(part_adj,
                                    neighbor$x,
                                    neighbor$y,
                                    zero = TRUE)
  }

  while(length(unique((geomander::check_contiguity(part_adj))$component)) > 1) {

    suggest <- geomander::suggest_component_connection(shp = pref_part,
                                                       adjacency = part_adj,
                                                       group = match(pref_part$code, unique(pref_part$code)))

    part_adj <- geomander::add_edge(part_adj,
                                    suggest$x,
                                    suggest$y,
                                    zero = TRUE)

  }

  part_map <- redist::redist_map(pref_part,
                                 ndists = round(sum(pref_part$pop)/(sum(pref_block$pop)/ndists_new)),
                                 pop_tol = 0.15,
                                 total_pop = pop,
                                 adj = part_adj)

  # init_smc_pref <- redist::redist_smc(
  #   map = part_map,
  #   nsims = 100,
  #   counties = pref_part$code,
  #   pop_temper = 0.05
  # )
  #
  # init_plan_vec <- redist::get_plans_matrix(init_smc_pref)
  #
  # part_parallel_pref <- redist::redist_mergesplit_parallel(
  #   map = part_map,
  #   nsims = 5000,
  #   chains = ncol(init_plan_vec),
  #   warmup = 0,
  #   init_plan = init_plan_vec,
  #   counties = pref_part$code,
  #   constraints = list(fractures = list(strength = 5), splits = list(strength = 5)),
  #   verbose = FALSE
  # )

  part_parallel_pref <- redist::redist_mergesplit(
    map = part_map,
    nsims = 250000,
    counties = pref_part$code,
    warmup = 0,
    constraints = list(fractures = list(strength = 4), splits = list(strength = 4))
  )

  # save it
  saveRDS(part_parallel_pref, paste("simulation/",
                              sprintf("%02d", pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_",
                              "block",
                              i,
                              "_",
                              "parallel.Rds",
                              sep = ""))

  # get disparity data
  part_parallel_weight_pref <- simulation_weight_disparity_table(part_parallel_pref)
  part_parallel_plans_pref <- redist::get_plans_matrix(part_parallel_pref)

  # get splits
  part_parallel_splits <- count_splits(part_parallel_plans_pref, part_map$code)
  part_parallel_countiessplit <- redist::redist.splits(part_parallel_plans_pref, part_map$code)

  parallel_results <- data.frame(matrix(ncol = 0, nrow = nrow(part_parallel_weight_pref)))
  parallel_results$max_to_min <- part_parallel_weight_pref$max_to_min
  parallel_results$splits <- part_parallel_splits
  parallel_results$counties_split <- part_parallel_countiessplit
  parallel_results$index <- 1:nrow(part_parallel_weight_pref)

  block_codes <- c(rep(1, length(which(unique(part_map$code) <= 14120))),
                   rep(2, length(which(unique(part_map$code) >= 14120 & unique(part_map$code) <= 14140))),
                   rep(3, length(which(unique(part_map$code) >= 14150 & unique(part_map$code) <= 14160))),
                   rep(4, length(which(unique(part_map$code) >= 14160))))

  block_div <- block_codes[match(part_map$code, sort(unique(part_map$code)))]

  parallel_results$cross <- count_overlap(part_parallel_plans_pref, block_div)

  parallel_results <- parallel_results %>%
    dplyr::group_by(max_to_min, splits, counties_split, cross) %>%
    dplyr::summarise(index = first(index)) %>%
    dplyr::arrange(splits)

  # rename elements to be used
  assign(paste(pref_name, pref_code, "full", i, sep = "_"),
         pref_part)
  assign(paste(pref_name, pref_code, "adj", "full", i, sep = "_"),
         part_adj)
  assign(paste(pref_name, pref_code, "map", "full", i, sep = "_"),
         part_map)
  assign(paste(pref_name, pref_code, "sim", sim_type, "full", i, sep = "_"),
         part_parallel_pref)
  assign(paste(pref_name, pref_code, sim_type, "plans", "full", i, sep = "_"),
         part_parallel_plans_pref)
  assign(paste(pref_name, pref_code, sim_type, "results", "full", i, sep = "_"),
         parallel_results)


  rm(list= ls()[(ls() %in% c("pref_part",
                             "part_adj",
                             "part_map",
                             "part_smc_pref",
                             "part_plans_pref",
                             "part_weight_pref",
                             "ferries",
                             "suggest",
                             "port_data",
                             "route_data",
                             "part_splits"
  ))])

}


##########Co-occurrence ############
#load packages
library(cluster)
library(viridis)
library(network)
library(ggnetwork)

#add column "n" as an indicator of the plans
n <- c(1:ncol(part_parallel_plans_pref))
n <- as.data.frame(n)
wgt_smc_0 <- cbind(n, part_parallel_weight_pref)

wgt_smc_0 <- wgt_smc_0[which(part_parallel_countiessplit == part_parallel_splits & part_parallel_splits <= 6), ]

#get plans that have a low max:min ratio (Top 10%)
good_num_0 <-  wgt_smc_0 %>%
  arrange(max_to_min) %>%
  slice(1: as.numeric(floor(nrow(wgt_smc_0)*0.1))) %>%
  select(n)

good_num_0 <- as.vector(t(good_num_0))

sim_smc_pref_0_good <- part_parallel_pref %>%
  filter(draw %in% good_num_0)

#obtain co-occurrence matrix
m_co_0 <- redist::prec_cooccurrence(sim_smc_pref_0_good, sampled_only=TRUE)

###calculate the centroids of each municipality/gun to plot population size
pref_map_0 <- pref_part
pref_map_0$CENTROID <- sf::st_centroid(pref_part$geometry)

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
for(i in 1:length(pref_part$code)){
  p <- m_co_sig_0 %>%
    filter(Var1 == pref_part$code[i]) %>%
    filter(Var2 %in% c(as.character(pref_part$code[part_adj[[i]]+1])))
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

# Overlap

overlap_vec <- overlap_vector(part_parallel_plans_pref[, good_num_0], block_div)
overlap_rowSums <- rowSums(overlap_vec)

redist::redist.plot.map(
  pref_part,
  fill = overlap_rowSums,
  plan = block_div
)

# Dissimilarity

mapping <- small_units %>% dplyr::select(code, subcode)
mapping$index <- -1

for(i in 1:nrow(mapping)){

  if (length(intersect(which(pref_part$code == mapping$code [i]),
                       which(pref_part$subcode == mapping$subcode [i]))) > 0) {

    mapping$index [i] <- intersect(which(pref_part$code == mapping$code [i]),
                               which(pref_part$subcode == mapping$subcode [i]))
  } else if (length(which(pref_part$code == mapping$code [i]))) {
    mapping$index [i] <- which(pref_part$code == mapping$code [i])
  } else {
    mapping$index [i] <- which(pref_part$code == 10 * floor(mapping$code [i]/10))
  }

}

mapped_plans <- redist::get_plans_matrix(sim_smc_pref_0_good)[mapping$index, ]

best_weights <- part_parallel_weight_pref[good_num_0, ]
best_weights$splits <- part_parallel_splits[good_num_0]

orig_plan <- status_quo_match(pref)



