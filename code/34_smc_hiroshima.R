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

############pref data (regardless of split)###############
# ----------- set up -------------#
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

# ---------- Set Up Prefectures ----------#
pref_code <- 34
pref_name <- as.character("hiroshima")
ndists_new <- 6
ndists_old <- 7
nsims <- 25000
sim_type <- as.character("smc")

#-------- Clean data (2015 Census)-----------#
# Clean data
pref_raw <- download_shp(pref_num) #34:Hiroshima
pref <- clean_jcdf(pref_raw = pref_raw)

#-------- Download 2020 census-----------#
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# -------- Choose which city to split ------------#
census2020 %>%
  filter(code > 34200 & code < 35000) %>% #excluding 34100 Hiroshima City
  arrange(desc(pop_national))
#Excluding Hiroshima City, Top 4 Muns
#34207: 福山 (currently NOT split; pre-gappei: 207 福山 481 内海 482沼隈 501神辺 524新市)
#34202: 呉 (currently NOT split; pre-gappei: 202呉 311音戸 312倉橋 313下蒲刈 314蒲刈 423安浦 424川尻 425豊浜 426豊)
#34212: 東広島 (currently split; pre-gappei: 212東広島 402黒瀬 405福富 406豊栄 408河内 422安芸津)
#34205: 尾道(currently split; pre-gappei: 205尾道 206因島 430瀬戸田 441御調 444向島)
#Since 4 municipalities (excluding Hiroshima City, a seireishiteitoshi) are currently split,
#the 1st ~ 4th largest municipalities will be split.
#江田島市 and 三原市, which are currently split, will no longer be split.

# -------- Old boundary ------------#
old_pref <- download_old_shp(pref_code = pref_num)
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_num)
old_34207 <- find_old_codes(34207, pop_by_old_boundary)
#福山市: 34207 34481 34482 34501 34524
old_34202 <- find_old_codes(34202, pop_by_old_boundary)
#呉市: 34202 34311 34312 34313 34314 34423 34424 34425 34426
old_34212 <- find_old_codes(34212, pop_by_old_boundary)
#東広島市: 34212 34402 34405 34406 34408 34422
old_34205 <- find_old_codes(34205, pop_by_old_boundary)
#尾道市: 34205 34206 34430 34441 34444

##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#Merge 安芸区(34107) and 安芸郡(34300) to avoid 飛び地
pref_0 <- avoid_enclave(pref_0, c(34107, 34300))

#Ferries
ferries_0 <- add_ferries(pref_0) %>%
  filter(V1 != 3) %>%
  filter(V2 != 3) %>%
  filter(V1 != 8 | V2 != 22)
####will remove the ferry route departing from 広島市南区(34103)
####otherwise 広島市南区 would be strangely connected to 宮島、江田島、呉
###will also remove ferry route between 呉 and 大崎上崎町 to avoid 飛び地

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list
#add edge
prefadj_0 <- geomander::add_edge(prefadj_0, ferries_0$V1, ferries_0$V2)

pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_0)

#save(list=ls(all=TRUE), file="34_smc_hiroshima_data_0split.Rdata")

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
pref_1 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge gun (0 exceptions) ------------#
pref_1 <- merge_gun(pref_1)

# -------- Old boundary ------------#
pref_1 <- reflect_old_boundaries(pref_1,
                                old_boundary = old_pref,
                                pop_by_old_boundary = pop_by_old_boundary,
                                old_code = c(34207, 34481, 34482, 34501, 34524),
                                #codes of municipalities that now belong to Fukuyamashi
                                new_code = 34207) #code of merged municipality (Fukuyamashi)

#Estimate 2020 pop based on old boundary
pref_1 <- estimate_old_boundary_pop(old_34207, 34207, pref_1, census2020)

#Merge 安芸区(34107) and 安芸郡(34300) to avoid 飛び地
pref_1 <- avoid_enclave(pref_1, c(34107, 34300))

#Ferries
ferries_1 <- add_ferries(pref_1) %>%
  filter(V1 != 8) %>%
  filter(V2 != 8) %>%
  filter(V1 != 13 | V2 != 26)
####will remove the ferry route departing from 広島市南区(34103)
####otherwise 広島市南区 would be strangely connected to 宮島、江田島、呉
###will also remove ferry route between 呉 and 大崎上崎町 to avoid 飛び地


# -------- set up for simulation ------------#
# simulation parameters
prefadj_1 <- redist::redist.adjacency(pref_1) # Adjacency list
#add edge
prefadj_1 <- geomander::add_edge(prefadj_1, ferries_1$V1, ferries_1$V2)

#manually add adjacency
prefadj_1 <- geomander::add_edge(prefadj_1, 2, 3)
#connect 34481内海町 to 34482沼隈町

pref_map_1 <- redist::redist_map(pref_1,
                                ndists = ndists_new,
                                pop_tol= 0.20,
                                total_pop = pop,
                                adj = prefadj_1)

###save(list=ls(all=TRUE), file="34_smc_hiroshima_data_1split.Rdata")

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


###############2 splits#############
pref_2 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge gun (0 exceptions) ------------#
pref_2 <- merge_gun(pref_2)

# -------- Old boundary ------------#
pref_2 <- reflect_old_boundaries(pref_2,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34207,#codes of municipalities that now belong to Fukuyamashi
                                 new_code = 34207) #code of merged municipality (Fukuyamashi)
pref_2 <- reflect_old_boundaries(pref_2,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34202,#codes of municipalities that now belong to Fukuyamashi
                                 new_code = 34202)

#Estimate 2020 pop based on old boundary
pref_2 <- estimate_old_boundary_pop(old_34207, 34207, pref_2, census2020)
pref_2 <- estimate_old_boundary_pop(old_34202, 34202, pref_2, census2020)

#Merge 安芸区(34107) and 安芸郡(34300) to avoid 飛び地
pref_2 <- avoid_enclave(pref_2, c(34107, 34300))

#Ferries
ferries_2 <- add_ferries(pref_2) %>%
  filter(V1 != 17) %>%
  filter(V2 != 17)
####will remove the ferry route departing from 広島市南区(34103)


# -------- set up for simulation ------------#
# simulation parameters
prefadj_2 <- redist::redist.adjacency(pref_2) # Adjacency list
#add edge
prefadj_2 <- geomander::add_edge(prefadj_2, ferries_2$V1, ferries_2$V2)

#Manually add adjacencies
prefadj_2 <- geomander::add_edge(prefadj_2, 11, 12) #connect 34481内海町 -> 34482沼隈町
prefadj_2 <- geomander::add_edge(prefadj_2, 2, 1) #34311音戸町　→ 34202呉市
prefadj_2 <- geomander::add_edge(prefadj_2, 2, 32) #34311音戸町　→ 34215江田島市
prefadj_2 <- geomander::add_edge(prefadj_2, 3, 2)  #34312 倉橋町 ->  34311音戸町
prefadj_2 <- geomander::add_edge(prefadj_2, 4, 1) #34313下蒲刈町 -> 34202呉市
prefadj_2 <- geomander::add_edge(prefadj_2, 5, 4) #34314 蒲刈町 -> 34313 下蒲刈町
prefadj_2 <- geomander::add_edge(prefadj_2, 8, 9) #34425 豊浜町 -> 34426 豊町

pref_map_2 <- redist::redist_map(pref_2,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_2)

####save(list=ls(all=TRUE), file="34_smc_hiroshima_data_2splits.Rdata")

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


###############3 splits#############
pref_3 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge gun (0 exceptions) ------------#
pref_3 <- merge_gun(pref_3)

# -------- Old boundary ------------#
pref_3 <- reflect_old_boundaries(pref_3,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34207,#codes of municipalities that now belong to Fukuyamashi
                                 new_code = 34207) #code of merged municipality (Fukuyamashi)
pref_3 <- reflect_old_boundaries(pref_3,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34202,#codes of municipalities that now belong to Kureshi
                                 new_code = 34202)
pref_3 <- reflect_old_boundaries(pref_3,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34212,#codes of municipalities that now belong to Higashihiroshima
                                 new_code = 34212)


#Estimate 2020 pop based on old boundary
pref_3 <- estimate_old_boundary_pop(old_34207, 34207, pref_3, census2020)
pref_3 <- estimate_old_boundary_pop(old_34202, 34202, pref_3, census2020)
pref_3 <- estimate_old_boundary_pop(old_34212, 34212, pref_3, census2020)

#Merge 安芸区(34107) and 安芸郡(34300) to avoid 飛び地
pref_3 <- avoid_enclave(pref_3, c(34107, 34300))

#Ferries
ferries_3 <- add_ferries(pref_3) %>%
  filter(V1 != 23) %>%
  filter(V2 != 23)
####will remove the ferry route departing from 広島市南区(34103)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_3 <- redist::redist.adjacency(pref_3) # Adjacency list
#add edge
prefadj_3 <- geomander::add_edge(prefadj_3, ferries_3$V1, ferries_3$V2)

#Manually add adjacencies
prefadj_3 <- geomander::add_edge(prefadj_3, 17, 18) #connect 34481内海町 to 34482沼隈町
prefadj_3 <- geomander::add_edge(prefadj_3, 8, 7) #34311音戸町　→ 34202呉市
prefadj_3 <- geomander::add_edge(prefadj_3, 8, 37) #34311音戸町　→ 34215江田島市
prefadj_3 <- geomander::add_edge(prefadj_3, 9, 8)  #34312 倉橋町 ->  34311音戸町
prefadj_3 <- geomander::add_edge(prefadj_3, 10, 7) #34313下蒲刈町 -> 34202呉市
prefadj_3 <- geomander::add_edge(prefadj_3, 10, 11) #34314 蒲刈町 -> 34313 下蒲刈町
prefadj_3 <- geomander::add_edge(prefadj_3, 14, 15) #34425  豊浜町 -> 34426 豊町

pref_map_3 <- redist::redist_map(pref_3,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_3)

####save(list=ls(all=TRUE), file="34_smc_hiroshima_data_3splits.Rdata")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_3 <- redist::redist_smc(pref_map_3,
                                     nsims = nsims,
                                     pop_temper = 0.05)

# save it
saveRDS(sim_smc_pref_3, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_3",
                              ".Rds",
                              sep = ""))


###############4 splits#############
pref_4 <- pref %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, geometry, pop)

# -------- Merge gun (0 exceptions) ------------#
pref_4 <- merge_gun(pref_4)

# -------- Old boundary ------------#
pref_4 <- reflect_old_boundaries(pref_4,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34207,#codes of municipalities that now belong to Fukuyamashi
                                 new_code = 34207) #code of merged municipality (Fukuyamashi)
pref_4 <- reflect_old_boundaries(pref_4,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34202,#codes of municipalities that now belong to Kureshi
                                 new_code = 34202)
pref_4 <- reflect_old_boundaries(pref_4,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34212,#codes of municipalities that now belong to Higashihiroshima
                                 new_code = 34212)
pref_4 <- reflect_old_boundaries(pref_4,
                                 old_boundary = old_pref,
                                 pop_by_old_boundary = pop_by_old_boundary,
                                 old_34205,#codes of municipalities that now belong to Onomichi
                                 new_code = 34205)


#Estimate 2020 pop based on old boundary
pref_4 <- estimate_old_boundary_pop(old_34207, 34207, pref_4, census2020)
pref_4 <- estimate_old_boundary_pop(old_34202, 34202, pref_4, census2020)
pref_4 <- estimate_old_boundary_pop(old_34212, 34212, pref_4, census2020)
pref_4 <- estimate_old_boundary_pop(old_34205, 34205, pref_4, census2020)

#Merge 安芸区(34107) and 安芸郡(34300) to avoid 飛び地
pref_4 <- avoid_enclave(pref_4, c(34107, 34300))

#Ferries
ferries_4 <- add_ferries(pref_4) %>%
  filter(V2 != 28) %>%
  filter(V1 != 28)
####will remove the ferry route departing from 広島市南区(34103)

# -------- set up for simulation ------------#
# simulation parameters
prefadj_4 <- redist::redist.adjacency(pref_4) # Adjacency list
#add edge
prefadj_4 <- geomander::add_edge(prefadj_4, ferries_4$V1, ferries_4$V2)

#Manually add adjacencies
prefadj_4 <- geomander::add_edge(prefadj_4, 22, 23) #connect 34481内海町 to 34482沼隈町
prefadj_4 <- geomander::add_edge(prefadj_4, 13, 12) #34311音戸町　→ 34202呉市
prefadj_4 <- geomander::add_edge(prefadj_4, 13, 41) #34311音戸町　→ 34215江田島市
prefadj_4 <- geomander::add_edge(prefadj_4, 14, 13)  #34312 倉橋町 ->  34311音戸町
prefadj_4 <- geomander::add_edge(prefadj_4, 15, 12) #34313下蒲刈町 -> 34202呉市
prefadj_4 <- geomander::add_edge(prefadj_4, 16, 15) #34314 蒲刈町 -> 34313 下蒲刈町
prefadj_4 <- geomander::add_edge(prefadj_4, 19, 20) #34425  豊浜町 -> 34426 豊町
prefadj_4 <- geomander::add_edge(prefadj_4, 2, 5)　#34206因島 -> 34444向島


pref_map_4 <- redist::redist_map(pref_4,
                                 ndists = ndists_new,
                                 pop_tol= 0.20,
                                 total_pop = pop,
                                 adj = prefadj_4)

#save(list=ls(all=TRUE), file="34_smc_hiroshima_data_4splits.Rdata")

##pref_map_4tol <- redist::redist_map(pref_4,ndists = ndists_new,pop_tol= 0.08,total_pop = pop, adj = prefadj_4)

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref_4 <- redist::redist_smc(pref_map_4,
                                     nsims = nsims,
                                     pop_temper = 0.05)


##sim_smc_pref_4tol <- redist::redist_smc(pref_map_4tol,nsims = nsims,  pop_temper = 0.05)
##saveRDS(sim_smc_pref_4tol, paste("simulation/", as.character(pref_code), "_", as.character(pref_name),  "_", as.character(sim_type), "_", as.character(nsims), "_4lowerpoptol",".Rds", sep = ""))

# save it
saveRDS(sim_smc_pref_4, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              as.character(sim_type),
                              "_",
                              as.character(nsims),
                              "_4",
                              ".Rds",
                              sep = ""))

########Analysis#####################
# extract plans
pref_smc_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)
pref_smc_plans_1 <- redist::get_plans_matrix(sim_smc_pref_1)
pref_smc_plans_2 <- redist::get_plans_matrix(sim_smc_pref_2)
pref_smc_plans_3 <- redist::get_plans_matrix(sim_smc_pref_3)
pref_smc_plans_4 <- redist::get_plans_matrix(sim_smc_pref_4)

# get disparity table data
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_smc_0 <- cbind(n, wgt_smc_0)
#wgt_smc_0$n[which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))]
#Maxmin 1.165 #4708 13939 18240 22240 24195
#redist::redist.plot.plans(sim_smc_pref_0, draws = 4708, geom = pref_map_0)

wgt_smc_1 <- simulation_weight_disparity_table(sim_smc_pref_1)
#wgt_smc_1 <- cbind(n, wgt_smc_1)
#wgt_smc_1$n[which(wgt_smc_1$max_to_min == min(wgt_smc_1$max_to_min))]
#Maxmin 1.158 #1135  3733  4817 21074
#redist::redist.plot.plans(sim_smc_pref_1, draws = 1135, geom = pref_map_1)

wgt_smc_2 <- simulation_weight_disparity_table(sim_smc_pref_2)
#wgt_smc_2 <- cbind(n, wgt_smc_2)
#wgt_smc_2$n[which(wgt_smc_2$max_to_min == min(wgt_smc_2$max_to_min))]
#Maxmin 1.152 #19065
#redist::redist.plot.plans(sim_smc_pref_2, draws = 19065, geom = pref_map_2)

wgt_smc_3 <- simulation_weight_disparity_table(sim_smc_pref_3)
#wgt_smc_3 <- cbind(n, wgt_smc_3)
#wgt_smc_3$n[which(wgt_smc_3$max_to_min == min(wgt_smc_3$max_to_min))]
#Maxmin  1.077 #20019
#redist::redist.plot.plans(sim_smc_pref_3, draws = 20019, geom = pref_map_3)

wgt_smc_4 <- simulation_weight_disparity_table(sim_smc_pref_4)
#wgt_smc_4 <- cbind(n, wgt_smc_4)
#wgt_smc_4$n[which(wgt_smc_4$max_to_min == min(wgt_smc_4$max_to_min))]
#Maxmin  1.131 #5584 7645
#redist::redist.plot.plans(sim_smc_pref_4, draws = 5584, geom = pref_map_4)


##wgt_smc_4tol <- simulation_weight_disparity_table(sim_smc_pref_4tol)
##wgt_smc_4tol <- cbind(n, wgt_smc_4tol)
##wgt_smc_4tol$n[which(wgt_smc_4tol$max_to_min == min(wgt_smc_4tol$max_to_min))]
##Maxmin 1.064 #7156 13765 16160 16429
##redist.plot.plans(sim_smc_pref_4tol, draws = 16160, geom = pref_map_4tol)

#save(list=ls(all=TRUE), file="34_smc_hiroshima_data_0to4splits.Rdata")

############Boxplot########################
boxplot_data <- bind_cols(wgt_smc_0$max_to_min, wgt_smc_1$max_to_min, wgt_smc_2$max_to_min,
                     wgt_smc_3$max_to_min, wgt_smc_4$max_to_min)
names(boxplot_data) <- c("0_split", "1_split", "2_splits", "3_splits", "4_splits")

boxplot(boxplot_data$"0_split", boxplot_data$"1_split", boxplot_data$"2_splits",
        boxplot_data$"3_splits", boxplot_data$"4_splits",
        names = c("0 split", "1 split", "2 splits", "3 splits", "4 splits"),
        ylab = "Max: min ratio")

##boxplot_data <- bind_cols(wgt_smc_0$max_to_min, wgt_smc_1$max_to_min, wgt_smc_2$max_to_min,wgt_smc_3$max_to_min, wgt_smc_4tol$max_to_min)
##names(boxplot_data) <- c("0_split", "1_split", "2_splits", "3_splits", "4_splits")
##boxplot(boxplot_data$"0_split", boxplot_data$"1_split", boxplot_data$"2_splits", boxplot_data$"3_splits", boxplot_data$"4_splits",names = c("0 split", "1 split", "2 splits", "3 splits", "4 splits"),ylab = "Max: min ratio")


##########Different Measures of Disparity######################
maxmin_LH_0 <- ggplot(data = wgt_smc_0,
                    mapping = aes(x = LH,
                                  y = max_to_min))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)
maxmin_LH_1 <- ggplot(data = wgt_smc_1,
                      mapping = aes(x = LH,
                                    y = max_to_min))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)
maxmin_LH_2 <- ggplot(data = wgt_smc_2,
                      mapping = aes(x = LH,
                                    y = max_to_min))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)
maxmin_LH_3 <- ggplot(data = wgt_smc_3,
                      mapping = aes(x = LH,
                                    y = max_to_min))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)
maxmin_LH_4 <- ggplot(data = wgt_smc_4,
                      mapping = aes(x = LH,
                                    y = max_to_min))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

############Status quo (not successful yet)################
status_quo <- status_quo_match(pref_4)

# establish keys
key <- vector(length = length(pref_4$code))
for (i in 1:length(pref_4$code)) {
  if (pref_4$code[i] %in% old_34205) {key[i] <- 34205}
  else if (pref_4$code[i] %in% old_34212) {key[i] <- 34212}
  else if (pref_4$code[i] %in% old_34202) {key[i] <- 34202}
  else if (pref_4$code[i] %in% old_34207) {key[i] <- 34207}
  else {key[i] <- pref_4$code[i]}
}

# map 0-split plans to 4-split plans
modified_smc_0 <- matrix(0, nrow = dim(pref_smc_plans_4)[1],
                         ncol = dim(pref_smc_plans_0)[2])

for (i in 1:dim(pref_smc_plans_4)[1]) {
  if (pref_4$code[i] %in% pref_0$code) {modified_smc_0[i, ] <-
    pref_smc_plans_0[which(pref_0$code == pref_4$code[i]), ]}
  else {modified_smc_0[i, ] <- pref_smc_plans_0[which(pref_0$code == key[i]), ]}
}

# map 1-split plans to 4-split plans
modified_smc_1 <- matrix(0, nrow = dim(pref_smc_plans_4)[1],
                         ncol = dim(pref_smc_plans_1)[2])

for (i in 1:dim(pref_smc_plans_4)[1]) {
  if (pref_4$code[i] %in% pref_1$code) {modified_smc_1[i, ] <-
    pref_smc_plans_1[which(pref_1$code == pref_4$code[i]), ]}
  else {modified_smc_1[i, ] <- pref_smc_plans_1[which(pref_1$code == key[i]), ]}
}

# map 2-split plans to 4-split plans
modified_smc_2 <- matrix(0, nrow = dim(pref_smc_plans_4)[1],
                         ncol = dim(pref_smc_plans_2)[2])

for (i in 1:dim(pref_smc_plans_4)[1]) {
  if (pref_4$code[i] %in% pref_2$code) {modified_smc_2[i, ] <-
    pref_smc_plans_2[which(pref_2$code == pref_4$code[i]), ]}
  else {modified_smc_2[i, ] <- pref_smc_plans_2[which(pref_2$code == key[i]), ]}
}

# map 3-split plans to 3-split plans
modified_smc_3 <- matrix(0, nrow = dim(pref_smc_plans_4)[1],
                         ncol = dim(pref_smc_plans_3)[2])

for (i in 1:dim(pref_smc_plans_4)[1]) {
  if (pref_4$code[i] %in% pref_3$code) {modified_smc_3[i, ] <-
    pref_smc_plans_3[which(pref_3$code == pref_4$code[i]), ]}
  else {modified_smc_3[i, ] <- pref_smc_plans_3[which(pref_3$code == key[i]), ]}
}

overlap_smc_0 <- vector(length = dim(pref_smc_plans_0)[2])
overlap_smc_1 <- vector(length = dim(pref_smc_plans_1)[2])
overlap_smc_2 <- vector(length = dim(pref_smc_plans_2)[2])
overlap_smc_3 <- vector(length = dim(pref_smc_plans_3)[2])
overlap_smc_4 <- vector(length = dim(pref_smc_plans_4)[2])

#save(list=ls(all=TRUE), file="34_smc_hiroshima_data.Rdata")


for (i in 1:length(overlap_smc_0)){
  overlap_smc_0[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_smc_0[, i], pref_4$pop,
                                                      weighting = "s", index_only = TRUE)
}
for (i in 1:length(overlap_smc_1)){
  overlap_smc_1[i] <- redist::redist.prec.pop.overlap(status_quo$ku, modified_smc_1[, i], pref_4$pop,
                                                      weighting = "s", index_only = TRUE)
}
for (i in 1:length(overlap_smc_2)){
  overlap_smc_2[i] <- redist::redist.prec.pop.overlap(status_quo$ku, pref_smc_plans_2[, i], pref_4$pop,
                                                      weighting = "s", index_only = TRUE)
}

for (i in 1:length(overlap_smc_3)){
  overlap_smc_3[i] <- redist::redist.prec.pop.overlap(status_quo$ku, pref_smc_plans_3[, i], pref_4$pop,
                                                      weighting = "s", index_only = TRUE)
}

for (i in 1:length(overlap_smc_4)){
  overlap_smc_4[i] <- redist::redist.prec.pop.overlap(status_quo$ku, pref_smc_plans_4[, i], pref_4$pop,
                                                      weighting = "s", index_only = TRUE)
}

wgt_orig <- simulation_weight_disparity_table(redist::redist_plans(plans = matrix(status_quo$ku, ncol = 1), map = pref_map_4, algorithm = "smc"))

# set parameters

improved_plans <- as.data.frame(
  cbind(rbind(wgt_smc_0 %>% dplyr::filter(LH < wgt_orig$LH),
              wgt_smc_1 %>% dplyr::filter(LH < wgt_orig$LH),
              wgt_smc_2 %>% dplyr::filter(LH < wgt_orig$LH),
              wgt_smc_3 %>% dplyr::filter(LH < wgt_orig$LH),
              wgt_smc_4 %>% dplyr::filter(LH < wgt_orig$LH)
  ),

  c(overlap_smc_0[which(wgt_smc_0$LH < wgt_orig$LH)],
    overlap_smc_1[which(wgt_smc_1$LH < wgt_orig$LH)],
    overlap_smc_2[which(wgt_smc_2$LH < wgt_orig$LH)],
    overlap_smc_3[which(wgt_smc_3$LH < wgt_orig$LH)],
    overlap_smc_4[which(wgt_smc_4$LH < wgt_orig$LH)]
  ),

  as.character(count_splits(modified_smc_0[, which(wgt_smc_0$LH < wgt_orig$LH)], key),
               count_splits(modified_smc_1[, which(wgt_smc_1$LH < wgt_orig$LH)], key),
               count_splits(modified_smc_2[, which(wgt_smc_2$LH < wgt_orig$LH)], key),
               count_splits(modified_smc_3[, which(wgt_smc_3$LH < wgt_orig$LH)], key),
               count_splits(modified_smc_4[, which(wgt_smc_4$LH < wgt_orig$LH)], key)
  )))

names(improved_plans) <- c(names(wgt_smc_0), "Dissimilarity", "Splits")

plot_smc <- ggplot(improved_plans, aes(Dissimilarity, LH, colour = Splits)) +
  geom_point(size = 1, alpha = 0.3)
ggMarginal(plot_smc, groupColour = TRUE, groupFill = TRUE)




##########Co-occurrence############
library(network)
library(ggmap)
library(ggnetwork)
good_num_0 <- wgt_smc_0$n[which(wgt_smc_0$max_to_min < 1.2)]
sim_smc_pref_0_good <- sim_smc_pref_0 %>%
  filter(draw %in% good_num_0)
matrix <- prec_cooccurrence(sim_smc_pref_0_good)

rownames(matrix) <- pref_map_0$code
colnames(matrix) <- pref_map_0$code
cooccurrence_data <- as_tibble(as.data.frame(as.table(matrix)))
cooccurrence_data$Freq <- cooccurrence_data$Freq*100 #Change cooccurrence frequency to %
cooccurrence_data <- cooccurrence_data %>%
  mutate(Freq = as.integer(Freq), Var1 = as.character(Var1), Var2 = as.character(Var2)) %>%
  filter(Var1 != Var2, Freq > 50) %>%
  #Only the municipalities that are in the same district more than 50% of the time are included in the plot
  group_by(Var1)

network <- network::network(cooccurrence_data, directed = FALSE, multiple = TRUE)

pref_0$CENTROID <- sf::st_centroid(pref_0$geometry)
pref_0_longlat <- pref_0 %>%
  as_tibble() %>%
  dplyr::select(code, CENTROID, pop) %>%
  separate(CENTROID, into = c("long", "lat"), sep = c(" "))
pref_0_longlat$long <- str_remove_all(pref_0_longlat$long, "[c(,]")
pref_0_longlat$lat <- str_remove_all(pref_0_longlat$lat, "[)]")
pref_0_longlat$long <- as.numeric(pref_0_longlat$long)
pref_0_longlat$lat <- as.numeric(pref_0_longlat$lat)

lat <- pref_0_longlat$lat
names(lat) <- as.character(pref_0_longlat$code)
lon <- pref_0_longlat$long
names(lon) <- as.character(pref_0_longlat$code)

geometry <- cbind(lon[ network.vertex.names(network) ], lat[ network.vertex.names(network) ])

edges <- ggnetwork(network, layout = geometry, scale = FALSE) %>%
  rename(lon = x, lat = y)

pref_0 %>%
  ggplot() +
  geom_sf() +
  geom_point(data = pref_0_longlat, aes(x = long, y = lat, size = pop/100000), color = "grey") +
  geom_edges(data = edges, mapping = aes(color = Freq, lon, lat, xend = xend, yend = yend)) +
  scale_color_gradient(low = "white", high = "dodgerblue") +
  labs(size = "Population (100,000)", color = "Co-occurrence (%)",
       title = "Co-occurrence Analysis: Plans with Max:Min Ratio < 1.2") +
  theme(legend.box = "vertical",
        legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

##########Co-occurrence (only adjacent municipalities) ############
#Creat 0 x 3 tibble
cooccurrence_data_adj <- cooccurrence_data
cooccurrence_data_adj <- cooccurrence_data_adj[ !(cooccurrence_data_adj$Var1 %in% cooccurrence_data$Var1), ]

for(i in 1:length(pref_map_0$code)){
  p <- cooccurrence_data %>%
    filter(Var1 == pref_map_0$code[i]) %>%
    filter(Var2 %in% c(as.character(pref_map_0$code[prefadj_0[[i]]+1])))
  cooccurrence_data_adj <- dplyr::bind_rows(p, cooccurrence_data_adj)
}

network_adj <- network::network(cooccurrence_data_adj, directed = FALSE, multiple = TRUE)

geometry_adj <- cbind(lon[ network.vertex.names(network_adj) ],
                      lat[ network.vertex.names(network_adj) ])

edges_adj <- ggnetwork(network_adj, layout = geometry_adj, scale = FALSE) %>%
  rename(lon = x, lat = y)

pref_0 %>%
  ggplot() +
  geom_sf() +
  geom_point(data = pref_0_longlat, aes(x = long, y = lat, size = 10*pop/100000), color = "grey") +
  geom_edges(data = edges_adj, mapping = aes(color = Freq, lon, lat, xend = xend, yend = yend),
             size = 0.8) +
  scale_color_gradient(low = "white", high = "navy") +
  labs(size = "Population (10,000)", color = "Co-occurrence (%)",
       title = "Co-occurrence Analysis: Plans with Max:Min Ratio < 1.2",
       caption = "Plotting only co-occurrence between adjacent municipalities") +
  theme(legend.box = "vertical",
        legend.title = element_text(color = "black", size = 7),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())
#4.5*5.0
