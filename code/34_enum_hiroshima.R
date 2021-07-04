set.seed(12345)
##############load packages###################
library(tidyverse)
library(sf)
library(geomander)
library(redist)
library(ggthemes)
library(ggrepel)
library(scales)
library(rgdal)
library(plotly)
library(glue)
library(patchwork)
library(ggtext)
library(RSpectra)
library(readr)

############pref data ###############
# ----------- set up -------------#
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

# ---------- Set Up Prefectures ----------#
pref_num <- 34
ndists_new <- 6
ndists_old <- 7

#-------- Clean data (2015 Census)-----------#
# Clean data
pref_raw <- download_shp(pref_num) #34:Hiroshima
pref <- clean_jcdf(pref_raw = pref_raw)

#-------- Download 2020 census-----------#
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)


##########0 split & remove all 飛び地###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
prefT <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

#Merge gun (No exceptions in this case; all the gun will be merged together)
prefT <- merge_gun(prefT)

#Merge 安芸区(34107) and 安芸郡(34300) to avoid 飛び地
prefT <- avoid_enclave(prefT, c(34107, 34300))

#######enumeration set-up##########
#Ferries
edgeT <- add_ferries(prefT) %>%
  filter(V1 != 3) %>%
  filter(V1 != 8 | V2 != 22)
####will remove the ferry route departing from 広島市南区(34103)
####otherwise 広島市南区 would be strangely connected to 宮島、江田島、呉
###will also remove ferry route between 呉 and 大崎上崎町 to avoid 飛び地

# simulation parameters
prefTadj <- redist::redist.adjacency(prefT) # Adjacency list
#add edge
prefTadj <- geomander::add_edge(prefTadj, edgeT$V1, edgeT$V2)

prefT_map <- redist::redist_map(prefT,
                                ndists = ndists_new,
                                pop_tol= 0.18,
                                total_pop = pop,
                                adj = prefTadj)

makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

# simulation
if(TRUE){ # change to TRUE if you need to init
  redist::redist.init.enumpart()
}

######Attempt 1#######
enum_plansT <- redist::redist.enumpart(adj = prefTadj,
                                        unordered_path = here::here('simulation/unord_hiroshimaT'),
                                        ordered_path = here::here('simulation/ord_hiroshimaT'),
                                        out_path = here::here('simulation/enum_hiroshimaT'),
                                        ndists = ndists_new,
                                        all = TRUE,
                                        total_pop = prefT_map[[attr(prefT_map, 'pop_col')]])
enum_plans <- redist.read.enumpart(out_path = 'simulation/enum_hiroshimaT')
plansT <- redist::redist_plans(enum_plans, prefT_map, algorithm = 'enumpart')
#12308514 plans


#save(enum_plans, file = "34_enumerate_hiroshima_data(enum_plans).Rdata")
save(plansT, file = "34_enumerate_hiroshima_data(plansT).Rdata")

wgt_enum_T <- simulation_weight_disparity_table(plansT)
n <- c(1:12308514)
n <- as.data.frame(n)
wgt_enum_T <- cbind(n, wgt_enum_T)

wgt_enum_T$n[which(wgt_enum_T$max_to_min == min(wgt_enum_T$max_to_min))]
#1.360954 -> could not obtain the optimal plan that SMC suggested
#most likely because I could only get 12308514 plans out of 95678942
#best plan 5388610

#print optimal plan
redist::redist.plot.plans(plansT,
                          draws = 5388610,
                          geom = prefT_map) +
  labs(caption = "Hiroshima 0 split \nEnumerate Optimal Plan;\nAvoided enclaves")


######Attempt 2#######
#imported from Microsoft Azure
#Vertex = 25; Edge = 50; Node = 1456; Solution = 95678942
enum_plansTa <- redist.read.enumpart(out_path = 'simulation/enum_hiroshimaTa')
plansTa <- redist::redist_plans(enum_plansTa, prefT_map, algorithm = 'enumpart')
###--> Error in validate_redist_plans(obj) : all(diff(max_distr) == 0) is not TRUE

