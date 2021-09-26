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
sim_type <- "smc"
nsims <- 500000
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

# Group Chichibu City (11207) and Chichibu-gun (11360) based on Teiju-jiritsuken
chichibu <- pref_county %>%
  dplyr::filter(code == 11207 |
                  code == 11360) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Honjo City (11211) and Kodama-gun (11380) based on Teiju-jiritsuken
honjo <- pref_county %>%
  dplyr::filter(code == 11211 |
                  code == 11380) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Kasukabe City (11214) and KitaKatsushika-gun (11460)
kasukabe <- pref_county %>%
  dplyr::filter(code == 11214 |
                  code == 11460) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

# Group Higashi-matsuyama City (11212) and Hiki-gun (11340)
higashimatsuyama <- pref_county %>%
  dplyr::filter(code == 11212 |
                  code == 11340) %>%
  dplyr::summarise(code = code[1],
                   pop = sum(pop),
                   geometry = sf::st_union(geometry))

pref_county_manually_edited <- pref_county %>%
  dplyr::filter(!code %in% c(11326,
                            11327,
                            11207,
                            11360,
                            11211,
                            11380,
                            11214,
                            11460,
                            11212,
                            11340)) %>%
  dplyr::bind_rows(.,
                   iruma,
                   chichibu,
                   honjo,
                   kasukabe,
                   higashimatsuyama) %>%
  dplyr::select(code, geometry) %>%
  dplyr::rename(county = code)

########## Add `county` column to the `pref` data frame ###########
sf::st_crs(pref) <- sf::st_crs(pref_county_manually_edited)
pref <- sf::st_intersection(pref_2020, pref_county_manually_edited)

summary(lengths(sf::st_overlaps(pref)))



















############Simulation Prep########################
#adjacency list
prefadj <- redist::redist.adjacency(pref_33)

neighbor <- geomander::suggest_neighbors(shp = pref_33,
                                         adjacency = prefadj)
if(nrow(neighbor) > 0){
  prefadj <- geomander::add_edge(prefadj,
                                 neighbor$x,
                                 neighbor$y,
                                 zero = TRUE)
}

pref_map <- redist::redist_map(pref_33,
                               ndists = round(sum(pref_33$pop)/(sum(pref_33$pop)/ndists_new)),
                               pop_tol= (sq_maxmin - 1)/(1 + sq_maxmin),
                               total_pop = pop,
                               adj = prefadj)

pref %>%
  ggplot()+
  geom_sf(data = pref %>%
            dplyr::filter(code >= 11300),
          aes(fill = as.factor(code)))+
  scale_fill_brewer(palette = "Spectral")+
  geom_sf(fill = NA, color = "black", lwd = 1)

pref_smc <- redist::redist_smc(pref_map,
                              nsims = nsims,
                              counties = code,
                              constraints = list(multisplits = list(strength = 50))
                              #pop_temper = 0.05
)

# save it
saveRDS(pref_smc, paste("simulation/",
                       sprintf("%02d", pref_code),
                       "_",
                       as.character(pref_name),
                       "_",
                       as.character(sim_type),
                       "_",
                       as.character(nsims),
                       ".Rds",
                       sep = ""))


# get disparity data
weight_pref <- simulation_weight_disparity_table(pref_smc)
plans_pref <- redist::get_plans_matrix(pref_smc)

# get splits
pref_smc_splits <- count_splits(plans_pref, pref_map$code)
pref_smc_countiessplit <- redist::redist.splits(plans_pref, pref_map$code)

pref_smc_results <- data.frame(matrix(ncol = 0, nrow = nrow(weight_pref)))
pref_smc_results$max_to_min <- weight_pref$max_to_min
pref_smc_results$splits <- pref_smc_splits
pref_smc_results$counties_split <- pref_smc_countiessplit
pref_smc_results$index <- 1:nrow(weight_pref)

pref_smc_results <- pref_smc_results %>%
  dplyr::group_by(max_to_min, splits, counties_split) %>%
  dplyr::summarise(index = first(index)) %>%
  dplyr::arrange(splits)

i <- 1

# rename elements to be used
assign(paste(pref_name, pref_code, "full", i, sep = "_"),
       pref)
assign(paste(pref_name, pref_code, "adj", "full", i, sep = "_"),
       prefadj)
assign(paste(pref_name, pref_code, "map", "full", i, sep = "_"),
       pref_map)
assign(paste(pref_name, pref_code, "sim", sim_type, "full", i, sep = "_"),
       pref_smc)
assign(paste(pref_name, pref_code, sim_type, "plans", "full", i, sep = "_"),
       plans_pref)
assign(paste(pref_name, pref_code, sim_type, "results", "full", i, sep = "_"),
       pref_smc_results)

min(pref_smc_results$max_to_min[which(pref_smc_results$splits == pref_smc_results$counties_split)])

saitama_11_smc_results_full_1 %>%
  dplyr::filter(splits <= 8) %>%
  dplyr::filter(splits == counties_split) %>%
  dplyr::arrange(max_to_min)
