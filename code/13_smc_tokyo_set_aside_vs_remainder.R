############# set up ###############
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
sim_type <- "smc"
nsims <- 25000
pref_code <- 13
pref_name <- "tokyo"
ndists_new <- 30
ndists_old <- 25

######### Download and Clean Data ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code)
# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

#########Clean data: pref################
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
#Estimate 2020 pop.
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#######Merge Gun#########
#merge nishitama
nishitama <- pref %>%
  dplyr::filter(code %in% c("13303", "13305", "13307", "13308"))
nishitama$code <- 13300
nishitama <- nishitama %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
nishitama$subcode <- "0000"

#merge oshima
oshima <- pref %>%
  dplyr::filter(code %in% c("13361", "13362", "13363", "13364"))
oshima$code <- 13360
oshima <- oshima %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
oshima$subcode <- "0000"

#merge miyake
miyake <- pref %>%
  dplyr::filter(code %in% c("13381", "13382"))
miyake$code <- 13380
miyake <- miyake %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
miyake$subcode <- "0000"

#merge hachijyo
hachijyo <- pref %>%
  dplyr::filter(code %in% c("13401", "13402"))
hachijyo$code <- 13400
hachijyo <- hachijyo %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
hachijyo$subcode <- "0000"

#merge ogasawara
ogasawara <- pref %>%
  dplyr::filter(code == "13421")
ogasawara$code <- 13420
ogasawara <- ogasawara %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))
ogasawara$subcode <- "0000"

#merge back together
pref_non_gun <- pref %>%
  dplyr::filter(code %in% c("13303", "13305", "13307", "13308",
                            "13361", "13362", "13363", "13364",
                            "13381", "13382",
                            "13401", "13402",
                            "13421") == FALSE)
#now pref is an object with 0 gun splits
pref <- dplyr::bind_rows(pref_non_gun, nishitama, oshima,
                         miyake, hachijyo, ogasawara)

#######Separat Tama from 23ku###############
rural <-  pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115",
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420") == FALSE)

urban <- pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115", #with Setagaya
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420")) #Islands are considered connected to Minato-ku
#save(list=ls(all=TRUE), file="13_smc_tokyo_basic_data_by_region.Rdata")

#############Urban: choose municipalities to set aside#################
urban_block <- urban %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry)) #27 administrative units

#adjacency list
urban_block_adj <- redist::redist.adjacency(urban_block)
#ferries
ferries_urban_block <- add_ferries(urban_block)
#edit adjacency list
urban_block_adj <- geomander::add_edge(urban_block_adj, ferries_urban_block$V1, ferries_urban_block$V2)

#create tibble that shows combination of municipalities
poptotal <- expand.grid(urban_block$code, urban_block$code)
poptotal <- as_tibble(as.data.frame(poptotal))
poptotal$total <- c(1:length(poptotal$Var1))

#calculate total of pop. between two municipalities
for(i in 1:length(poptotal$Var1)){
  poptotal$total[i] <- urban_block$pop[which(urban_block$code == poptotal$Var1[i])] +
                       urban_block$pop[which(urban_block$code == poptotal$Var2[i])]
}

#Creat 0 x 3 tibble
poptotal_adj <- poptotal
poptotal_adj <- poptotal_adj[ !(poptotal_adj$Var1 %in% poptotal$Var1), ]

#filter out adjacent municipalities
for(i in 1:length(urban_block$code)){
  p <- poptotal %>%
    filter(Var1 == urban_block$code[i]) %>%
    filter(Var2 %in% c(as.character(urban_block$code[urban_block_adj[[i]]+1])))
  poptotal_adj <- dplyr::bind_rows(p, poptotal_adj)
}

#choose pairs whose population is close to *2 of target pop.
target_urban <- sum(urban_block$pop)/21
poptotal_adj_multiple <- poptotal_adj %>%
  filter(total > target_urban*2*0.975 & total < target_urban*2*1.025)
#13119 13117 板橋区 北区
#13115 13114 中野区 杉並区
#13108 13109 江東区 品川区

########Setagaya 13112 #############
setagaya <- pref %>%
  dplyr::filter(code == 13112)

#adjacency list
setagaya_adj <- redist::redist.adjacency(setagaya)

#set up map
setagaya_map <- redist::redist_map(setagaya,
                                   ndists = 2,
                                   pop_tol= 0.03,
                                   total_pop = pop,
                                   adj = setagaya_adj)

#smc simulation
setagaya_smc <- redist::redist_smc(setagaya_map,
                                   nsims = nsims,
                                   pop_temper = 0.05)

setagaya <- pref %>%
  dplyr::filter(code == 13112)

#adjacency list
setagaya_adj <- redist::redist.adjacency(setagaya)

#set up map
setagaya_map <- redist::redist_map(setagaya,
                                   ndists = 2,
                                   pop_tol= 0.03,
                                   total_pop = pop,
                                   adj = setagaya_adj)

#smc simulation
setagaya_smc <- redist::redist_smc(setagaya_map,
                                   nsims = nsims,
                                   pop_temper = 0.05)

#save
saveRDS(setagaya_smc, paste("simulation/",
                            as.character(pref_code),
                            "_",
                            "setagaya",
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

setagaya_smc <- readRDS("~/Desktop/ALARM Project/jcdf/simulation/13_setagaya_smc_25000.Rds")

wgt_smc_setagaya <- simulation_weight_disparity_table(setagaya_smc)
m <- c(1:25000)
wgt_smc_setagaya <- cbind(m, wgt_smc_setagaya)
minimum_maxmin_setagaya <-
  wgt_smc_setagaya$m[which(wgt_smc_setagaya$max_to_min == min(wgt_smc_setagaya$max_to_min))][1]

redist::redist.plot.plans(setagaya_smc, draws = minimum_maxmin_setagaya, geom = setagaya_map)

setagaya_smc %>% filter(draw == minimum_maxmin_setagaya) # 462290  V.S. 462298

########Itabashi_Kita 13119 13117#########
itabashi_kita <- pref %>%
  dplyr::filter(code %in% c(13119, 13117))

#adjacency list
itabashi_kita_adj <- redist::redist.adjacency(itabashi_kita)

#set up map
itabashi_kita_map <- redist::redist_map(itabashi_kita,
                                        ndists = 2,
                                        pop_tol= 0.03,
                                        total_pop = pop,
                                        adj = itabashi_kita_adj)

#smc simulation
itabashi_kita_smc <- redist::redist_smc(itabashi_kita_map,
                                       nsims = nsims,
                                        pop_temper = 0.05)


#save
saveRDS(itabashi_kita_smc, paste("simulation/",
                            as.character(pref_code),
                            "_",
                            "itabashi_kita",
                            "_",
                            as.character(sim_type),
                            "_",
                            as.character(nsims),
                            ".Rds",
                            sep = ""))

wgt_smc_itabashi_kita <- simulation_weight_disparity_table(itabashi_kita_smc)
m <- c(1:25000)
wgt_smc_itabashi_kita <- cbind(m, wgt_smc_itabashi_kita)
minimum_maxmin_itabashi_kita <-
  wgt_smc_itabashi_kita$m[which(wgt_smc_itabashi_kita$max_to_min == min(wgt_smc_itabashi_kita$max_to_min))][1]

redist::redist.plot.plans(itabashi_kita_smc,
                          draws = minimum_maxmin_itabashi_kita,
                          geom = itabashi_kita_map)

itabashi_kita_smc %>% filter(draw == minimum_maxmin_itabashi_kita)
#445576 vs. 445577

########Nakano_Suginami 13115 13114##############
nakano_suginami <- pref %>%
  dplyr::filter(code %in% c(13115, 13114))

#adjacency list
nakano_suginami_adj <- redist::redist.adjacency(nakano_suginami)

#set up map
nakano_suginami_map <- redist::redist_map(nakano_suginami,
                                          ndists = 2,
                                          pop_tol= 0.03,
                                          total_pop = pop,
                                          adj = nakano_suginami_adj)

#smc simulation
nakano_suginami_smc <- redist::redist_smc(nakano_suginami_map,
                                          nsims = nsims,
                                          pop_temper = 0.05)

#save
saveRDS(nakano_suginami_smc, paste("simulation/",
                                 as.character(pref_code),
                                 "_",
                                 "nakano_suginami",
                                 "_",
                                 as.character(sim_type),
                                 "_",
                                 as.character(nsims),
                                 ".Rds",
                                 sep = ""))

wgt_smc_nakano_suginami <- simulation_weight_disparity_table(nakano_suginami_smc)
m <- c(1:25000)
wgt_smc_nakano_suginami <- cbind(m, wgt_smc_nakano_suginami)
minimum_maxmin_nakano_suginami <-
  wgt_smc_nakano_suginami$m[which(wgt_smc_nakano_suginami$max_to_min == min(wgt_smc_nakano_suginami$max_to_min))][1]

redist::redist.plot.plans(nakano_suginami_smc,
                          draws = minimum_maxmin_nakano_suginami,
                          geom = nakano_suginami_map)

nakano_suginami_smc %>% filter(draw == minimum_maxmin_nakano_suginami)
#451109, 451103

########Shinagawa_Koto 13108, 13109##############
shinagawa_koto <- pref %>%
  dplyr::filter(code %in% c(13108, 13109))

#adjacency list
shinagawa_koto_adj <- redist::redist.adjacency(shinagawa_koto)

#[70]13109 0250 品川区八潮
shinagawa_koto_adj <- geomander::add_edge(shinagawa_koto_adj, 70, 63) #[63]品川区東品川180
shinagawa_koto_adj <- geomander::add_edge(shinagawa_koto_adj, 70, 61) #[61]品川区東大井160
shinagawa_koto_adj <- geomander::add_edge(shinagawa_koto_adj, 70, 49) #[49]品川区勝島40

#[72]13109 0270 品川区東八潮
shinagawa_koto_adj <- geomander::add_edge(shinagawa_koto_adj, 72, 70) #[70]13109 0250 品川区八潮

#set up map
shinagawa_koto_map <- redist::redist_map(shinagawa_koto,
                                         ndists = 2,
                                         pop_tol= 0.03,
                                         total_pop = pop,
                                         adj = shinagawa_koto_adj)

#smc simulation
shinagawa_koto_smc <- redist::redist_smc(shinagawa_koto_map,
                                          nsims = nsims,
                                          pop_temper = 0.05)

#save
saveRDS(shinagawa_koto_smc, paste("simulation/",
                                   as.character(pref_code),
                                   "_",
                                   "shinagawa_koto",
                                   "_",
                                   as.character(sim_type),
                                   "_",
                                   as.character(nsims),
                                   ".Rds",
                                   sep = ""))

wgt_smc_shinagawa_koto <- simulation_weight_disparity_table(shinagawa_koto_smc)
m <- c(1:25000)
wgt_smc_shinagawa_koto <- cbind(m, wgt_smc_shinagawa_koto)
minimum_maxmin_shinagawa_koto <-
  wgt_smc_shinagawa_koto$m[which(wgt_smc_shinagawa_koto$max_to_min == min(wgt_smc_shinagawa_koto$max_to_min))][1]

redist::redist.plot.plans(shinagawa_koto_smc,
                          draws = minimum_maxmin_shinagawa_koto,
                          geom = shinagawa_koto_map)

shinagawa_koto_smc %>% filter(draw == minimum_maxmin_shinagawa_koto)

########Urban Remainder Set up###################
urban_rem <- pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", 　　　　　　　　　"13110",
                     "13111",          "13113",
                     "13116",          "13118",          "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420")) #Islands are considered part of Minato-ku

#adjacency list
urban_rem_adj <- redist::redist.adjacency(urban_rem)

#ferries
ferries_urban_rem <- add_ferries(urban_rem)

#edit adjacency list
urban_rem_adj <- geomander::add_edge(urban_rem_adj,
                                     ferries_urban_rem$V1, ferries_urban_rem$V2)
#manually edit adjacency list
#[92] 13102 0340 中央区佃
#[93] 13102 0350 中央区月島
#[94] 13102 0360 中央区勝どき
#[95] 13102 0370 中央区豊海町
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 92, 70) #[70] 13102 110 中央区新川
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 92, 66) #[66] 13102 0070 中央区明石町
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 92, 93)
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 93, 94)
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 94, 67)  #[67] 13102 0080 中央区築地
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 94, 95)

#[96] 13102 0380 中央区晴海
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 96, 93)
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 96, 94)

#[126]13103 0300 港区台場
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 126, 98) #[98]13103 0020港区海岸

#[384]13111 0580 大田区東海
#[385]13111 0590 大田区城南島
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 384, 385)
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 384, 335) #[335] 13111 0090大田区平和島

#[489]13120 0420 練馬区西大泉町
urban_rem_adj <- geomander::add_edge(urban_rem_adj, 489, 490) #connect to [490]練馬区西大泉(６丁目) 0430

urban_rem_map <- redist::redist_map(urban_rem,
                                    ndists = 13,
                                    pop_tol= 0.08,
                                    total_pop = pop,
                                    adj = urban_rem_adj)

#save(list=ls(all=TRUE), file="13_smc_tokyo_set_aside_urban_2.Rdata")

##########Urban Remainder Diagnostics#############
urban_rem_smc <- readRDS("~/Desktop/ALARM Project/Tokyo Results/Set_aside/Urban/[0.08-1000]13_urbansetaside2.Rds")

wgt_smc_urban_rem <- simulation_weight_disparity_table(urban_rem_smc)
m <- c(1:1000)
wgt_smc_urban_rem <- cbind(m, wgt_smc_urban_rem)
min(wgt_smc_urban_rem$max_to_min)

minimum_maxmin_urban <-
  wgt_smc_urban_rem$m[which(wgt_smc_urban_rem$max_to_min == min(wgt_smc_urban_rem$max_to_min))][1]

plans_pref_urban_rem <- redist::get_plans_matrix(urban_rem_smc)

#count the number of splits
splits_urban_rem <- count_splits(plans_pref_urban_rem, urban_rem_map$code)
splits_urban_rem[minimum_maxmin_urban]

#count the number of municipalities that are split
csplits_urban_rem <- redist::redist.splits(plans_pref_urban_rem, urban_rem_map$code)
csplits_urban_rem[minimum_maxmin_urban]

results_urban <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_urban_rem)))
results_urban$max_to_min <- wgt_smc_urban_rem$max_to_min
results_urban$splits <- splits_urban_rem
results_urban$counties_split <- csplits_urban_rem
results_urban$index <- 1:nrow(wgt_smc_urban_rem)
results_urban$dif <-  results_urban$splits - results_urban$counties_split
min(results_urban$dif)
View(results_urban)

#optimal plan: draw 1; maxmin 1.122205; splits 10 (0 multi-splits)

#############Urban Remainder Plot Plan################
#get data on optimal plan
matrix_plan_urban_rem <- redist::get_plans_matrix(urban_rem_smc %>% filter(draw == 1))
colnames(matrix_plan_urban_rem) <- "district"
matrix_plan_urban_rem <- head(matrix_plan_urban_rem, 731)
optimal_boundary <- cbind(head(urban_rem, 731), as_tibble(matrix_plan_urban_rem))

#get data on municipality boundary
urban_rem_boundaries <- urban_rem %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))
urban_rem_boundaries <- head(urban_rem_boundaries, 18)

#get data on district boundary
urban_rem_district_boundaries <- optimal_boundary %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = urban_rem_boundaries, fill = NA, color = "black", lwd = 1.5) +
  geom_sf(data = urban_rem_district_boundaries, fill = NA, color = "yellow", lwd = 0.15) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

#############Rural: Choose municipalities to set aside#################
rural_block <- rural %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry)) #27 administrative units

#adjacency list
rural_block_adj <- redist::redist.adjacency(rural_block)

#create tibble that shows combination of municipalities
poptotal <- expand.grid(rural_block$code, rural_block$code)
poptotal <- as_tibble(as.data.frame(poptotal))
poptotal$total <- c(1:length(poptotal$Var1))

#calculate total of pop. between two municipalities
for(i in 1:length(poptotal$Var1)){
  poptotal$total[i] <- rural_block$pop[which(rural_block$code == poptotal$Var1[i])] +
    rural_block$pop[which(rural_block$code == poptotal$Var2[i])]
}

#Creat 0 x 3 tibble
poptotal_adj <- poptotal
poptotal_adj <- poptotal_adj[ !(poptotal_adj$Var1 %in% poptotal$Var1), ]

#filter out adjacent municipalities
for(i in 1:length(rural_block$code)){
  p <- poptotal %>%
    filter(Var1 == rural_block$code[i]) %>%
    filter(Var2 %in% c(as.character(rural_block$code[rural_block_adj[[i]]+1])))
  poptotal_adj <- dplyr::bind_rows(p, poptotal_adj)
}

#filter out pairs of municipalities whose population is close to *2 of target pop.
target_rural <- sum(rural_block$pop)/9
poptotal_adj_multiple <- poptotal_adj %>%
  filter(total > target_rural*2*0.90 & total < target_rural*2*1.10)
#13209町田市 13201八王子市　= total 992341 -> 496170.5 per district

########Machida_Hachioji 13209 13201#############
machida_hachioji <- pref %>%
  dplyr::filter(code %in% c(13209, 13201))

#adjacency list
machida_hachioji_adj <- redist::redist.adjacency(machida_hachioji)

#set up map
machida_hachioji_map <- redist::redist_map(machida_hachioji,
                                          ndists = 2,
                                          pop_tol= 0.03,
                                          total_pop = pop,
                                          adj = machida_hachioji_adj)

#smc simulation
machida_hachioji_smc <- redist::redist_smc(machida_hachioji_map,
                                          nsims = nsims,
                                          pop_temper = 0.05)

#save
saveRDS(machida_hachioji_smc, paste("simulation/",
                                   as.character(pref_code),
                                   "_",
                                   "machida_hachioji",
                                   "_",
                                   as.character(sim_type),
                                   "_",
                                   as.character(nsims),
                                   ".Rds",
                                   sep = ""))

wgt_smc_machida_hachioji <- simulation_weight_disparity_table(machida_hachioji_smc)
m <- c(1:25000)
wgt_smc_machida_hachioji <- cbind(m, wgt_smc_machida_hachioji)
minimum_maxmin_machida_hachioji <-
  wgt_smc_machida_hachioji$m[which(wgt_smc_machida_hachioji$max_to_min == min(wgt_smc_machida_hachioji$max_to_min))][1]

redist::redist.plot.plans(machida_hachioji_smc,
                          draws = minimum_maxmin_machida_hachioji,
                          geom = machida_hachioji_map)

machida_hachioji_smc %>% filter(draw == minimum_maxmin_machida_hachioji)
#496171, 496170

########Rural Remainder Set up###################
rural_rem <-  pref %>%
  filter(code %in% c("13101", "13102", "13103", "13104", "13105",
                     "13106", "13107", "13108", "13109", "13110",
                     "13111", "13112", "13113", "13114", "13115",
                     "13116", "13117", "13118", "13119", "13120",
                     "13121", "13122", "13123",
                     "13360", "13380", "13400", "13420",
                     "13209", "13201") == FALSE) #set aside 八王子、町田

#adjacency list
rural_rem_adj <- redist::redist.adjacency(rural_rem)

#save(list=ls(all=TRUE), file="13_smc_tokyo_set_aside_rural.Rdata")

########Rural Remainder Simulation##########
#maps
rural_rem_map <- redist::redist_map(rural_rem,
                                    ndists = 7, #9-2(Hachioji;Machida)
                                    pop_tol= 0.08,
                                    total_pop = pop,
                                    adj = rural_rem_adj)

# --------- SMC simulation ----------------#
#run simulation
rural_rem_smc <- redist::redist_smc(rural_rem_map,
                                    nsims = 25000,
                                    counties = rural_rem_map$code,
                                    constraints = list(multisplits = list(strength = 100000000)),
                                    pop_temper = 0.05
)

#########diagnostics:rural#########
rural_smc <- readRDS("~/Desktop/ALARM Project/Tokyo Results/Set_aside/Rural/[0.08-1000]13_rural_smc_1000.Rds")

wgt_smc_rural <- simulation_weight_disparity_table(rural_smc)
m <- c(1:25000)
wgt_smc_rural <- cbind(m, wgt_smc_rural)
min(wgt_smc_rural$max_to_min)

minimum_maxmin_rural <-
  wgt_smc_rural$m[which(wgt_smc_rural$max_to_min == min(wgt_smc_rural$max_to_min))][1]

plans_pref_rural <- redist::get_plans_matrix(rural_smc)

#count the number of splits
splits_rural <- count_splits(plans_pref_rural, rural_rem_map$code)
splits_rural[minimum_maxmin_rural]

#count the number of municipalities that are split
csplits_rural<- redist::redist.splits(plans_pref_rural, rural_rem_map$code)
csplits_rural[minimum_maxmin_rural]

results_rural <- data.frame(matrix(ncol = 0, nrow = nrow(wgt_smc_rural)))
results_rural$max_to_min <- wgt_smc_rural$max_to_min
results_rural$splits <- splits_rural
results_rural$counties_split <- csplits_rural
results_rural$index <- 1:nrow(wgt_smc_rural)
results_rural$dif <-  results_rural$splits - results_rural$counties_split
min(results_rural$dif)
View(results_rural)

#New: optimal plan: draw 1; maxmin 1.141586; splits 6 (0 multi-splits)
optimal_rural <- minimum_maxmin_rural #1

#############Rural Remainder Plot Plan################
#get data on optimal plan
matrix_plan_rural_rem <- redist::get_plans_matrix(rural_smc %>% filter(draw == 1))
colnames(matrix_plan_rural_rem) <- "district"
optimal_boundary <- cbind(rural_rem, as_tibble(matrix_plan_rural_rem))

#get data on municipality boundary
rural_rem_boundaries <- rural_rem %>%
  group_by(code) %>%
  summarise(geometry = sf::st_union(geometry))

#get data on district boundary
rural_rem_district_boundaries <- optimal_boundary %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry))

#map with district data + municipality boundary
ggplot() +
  geom_sf(data = optimal_boundary, aes(fill = factor(district))) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
  geom_sf(data = rural_rem_boundaries, fill = NA, color = "black", lwd = 1) +
  geom_sf(data = rural_rem_district_boundaries, fill = NA, color = "yellow", lwd = 0.15) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())


#######Visualization of Blocks###############
tokyo_exclude_islands <- pref %>%
  dplyr::filter(code %in% c("13360", "13380", "13400", "13420") == FALSE) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(pop = sum(pop), geometry = sf::st_union(geometry))

tokyo_exclude_islands$block <- c(1,1,1,1,1,1,1,2,2,1,1,3,1,4,4,1,5,1,5,1,1,1,1,
                                 7,6,6,6,6,6,6,6,7,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                 6,6,6,6)
#get data on district boundary
block_boundary <- tokyo_exclude_islands %>%
  group_by(block) %>%
  summarise(geometry = sf::st_union(geometry))

ggplot() +
  geom_sf(data = tokyo_exclude_islands, aes(fill = factor(block))) +
  geom_sf(data = block_boundary, fill = NA, color = "black", lwd = 1) +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "None",
        panel.background = element_blank())

########Results########
total <- dplyr::bind_rows(setagaya_smc %>% filter(draw == minimum_maxmin_setagaya),
                          itabashi_kita_smc %>% filter(draw == minimum_maxmin_itabashi_kita),
                          nakano_suginami_smc %>% filter(draw == minimum_maxmin_nakano_suginami),
                          shinagawa_koto_smc %>% filter(draw == minimum_maxmin_shinagawa_koto),
                          machida_hachioji_smc %>% filter(draw == minimum_maxmin_machida_hachioji),
                          urban_rem_smc %>% filter(draw == minimum_maxmin_urban),
                          rural_smc %>%  filter(draw == optimal_rural))
total$region <- rep(c("Setagaya",
                      "Itabashi-Kita",
                      "Nakano-Suginami",
                      "Shinagawa-Koto",
                      "Machida-Hachioji",
                      "Urban(Remainder)",
                      "Rural(Remainder)"),
                    times = c(2, 2, 2, 2, 2, 13, 7))
max(total$total_pop)/min(total$total_pop)

#Number of splits
nsplits <- 1+2+2+2+2+10+6


