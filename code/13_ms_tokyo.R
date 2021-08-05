############# set up ###############
#-------------- functions set up ---------------#
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
sim_type <- "ms"
nsims <- 25000
pref_code <- 13
pref_name <- "tokyo"
lakes_removed <- c() # enter `c()` if not applicable
# set number of district (check external information)
ndists_new <- 30
ndists_old <- 25

#------- Specify municipality splits -------------#
# number of splits
nsplit <- 0
merge_gun_exception <- c()
#西多摩郡 is the only gun in Tokyo and it is not split under the status quo
#大島支庁,三宅支庁, 八丈支庁, 小笠原支庁 are treated as gun

######### Download and Clean Data ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code) #first download data

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)
# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

##########Analysis##############
pref_0 %>%
  dplyr::arrange(desc(pop)) %>%
  ggplot(aes(x = reorder(as.factor(code), -pop ), y = pop)) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = sum(pref_0$pop)/ndists_new), color = "red") +
  annotate("text", x = 50, y = 500000,
           label = "Target\npop.", color = "red")+
  labs(x = NULL,
       y = "Population",
       title = "Population Distribution in Tokyo") +
  coord_flip()

##########Simple MS#################
#clean pref_raw
pref <- pref_raw %>%
  clean_jcdf()
pref <- pref %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#ferries
ferries <- add_ferries(pref)

#98  1642 #1642 1648 #1648 1649 #98 1628 #1628 1633# 1633 1635 #1635 1636
#1636 1641 #1635 1641 #1650 1655 #1657 1658 #98 1657

# -------- set up for simulation ------------#
# simulation parameters
prefadj <- redist::redist.adjacency(pref) # Adjacency list

#add edge
prefadj <- geomander::add_edge(prefadj,
                               ferries[, 1],
                               ferries[, 2])

#manually edit adjacency list
#[92]13102 0340 中央区佃
#[93]13102 0350 中央区月島 #[94]13012 0360 中央区勝どき 　#[95]13102 0370 中央区豊海町
#[96]13102 0380 中央区晴海
prefadj <- geomander::add_edge(prefadj, 92, 70) #[70] 13102 110 中央区新川
prefadj <- geomander::add_edge(prefadj, 92, 66) #[66] 13102 0070 中央区明石町
prefadj <- geomander::add_edge(prefadj, 92, 93)
prefadj <- geomander::add_edge(prefadj, 93, 94)
prefadj <- geomander::add_edge(prefadj, 94, 67)  #[67] 13102 0080 中央区築地
prefadj <- geomander::add_edge(prefadj, 94, 95)

prefadj <- geomander::add_edge(prefadj, 96, 93)
prefadj <- geomander::add_edge(prefadj, 96, 94)
prefadj <- geomander::add_edge(prefadj, 96, 320) #[320] 13108 210 江東区　豊洲

#[369]13109 0250 品川区八潮
#[456]13111 0580 大田区東海
#[457]13111 0590 大田区城南島
prefadj <- geomander::add_edge(prefadj, 369, 362) #[362]品川区東品川180
prefadj <- geomander::add_edge(prefadj, 369, 360) #[360]品川区東大井160
prefadj <- geomander::add_edge(prefadj, 369, 348) #[348]品川区勝島40
prefadj <- geomander::add_edge(prefadj, 456, 457)
prefadj <- geomander::add_edge(prefadj, 456, 407) #[407] 13111 0090大田区平和島

#[764]13120 0420 練馬区西大泉町
prefadj <- geomander::add_edge(prefadj, 764, 765) #connect to [765]練馬区西大泉(６丁目) 0430

#[1638]13363 0040 新島村鵜渡根島
#[1639]13363 0050 新島村地内島
#[1640]13363 0060 新島村早島
#connect to mainland 新島 [1635]
prefadj <- geomander::add_edge(prefadj, 1635, 1638)
prefadj <- geomander::add_edge(prefadj, 1635, 1639)
prefadj <- geomander::add_edge(prefadj, 1635, 1640)


#[1654]13401 0060　八丈町八丈小島 #connect to mainland 八丈島 [1650]
prefadj <- geomander::add_edge(prefadj, 1650, 1654)

#[1656]13402 0020 青ヶ島村NA #connect to mainland 青ヶ島 [1655]
prefadj <- geomander::add_edge(prefadj, 1656, 1655)

#[1659]13421 0030 小笠原村聟島
prefadj <- geomander::add_edge(prefadj, 1659, 1657) #connect to 小笠原島父島[1657]

#[1660]13421 0040 小笠原村硫黄島
prefadj <- geomander::add_edge(prefadj, 1660, 1658) #connect to 小笠原島母島[1658]

#[1661]13421 0050 小笠原村沖ノ鳥島
#[1662]13421 0060 小笠原村南鳥島
prefadj <- geomander::add_edge(prefadj, 1661, 98) #[98] connect to 港区
prefadj <- geomander::add_edge(prefadj, 1662, 98) #[98] connect to 港区

pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.15,
                               total_pop = pop,
                               adj = prefadj)

# --------- MS simulation ----------------#
sim_ms_pref <- redist::redist_mergesplit(pref_map,
                                         nsims = 10,
                                         counties = pref_map$code,
                                         constraints = list(splits = list(strength = 1000)))
#constraints = list("countysplit"

# save it
saveRDS(sim_smc_pref_0, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              "smc",
                              "_",
                              as.character(nsims),
                              "_0",
                              ".Rds",
                              sep = ""))



##########MS (Only split large counties)#################
#clean pref_raw
pref <- pref_raw %>%
  clean_jcdf()
pref <- pref %>%
  dplyr::select(code, KIHON1, JINKO, geometry)
pref <- calc_kokumin(pref, dem_pops)
#Estimate 2020 pop.
pref <- estimate_2020_pop(pref, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

#check 0 split data (match with 2020 census data)
pref_0 <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)
#treat gun as one municipality
pref_0 <- merge_gun(pref_0)

ranking <- pref_0 %>%
  dplyr::arrange(desc(pop))
#select municipalities whose population is less than 50% of target pop -> keep intact
pref_small <- pref_0 %>% dplyr::filter(pop < 0.50 * sum(pref_0$pop)/ndists_new)
pref_small$subcode <- "0000"

#select municipalities whose population is more than 50% of target pop -> separate
large_codes <- pref_0$code[which(pref_0$code %in% pref_small$code == FALSE)]
pref_large <- pref %>% dplyr::filter(code %in% large_codes)

#bind together
pref_50 <- dplyr::bind_rows(pref_small, pref_large)

#ferries
ferries_50 <- add_ferries(pref_50)

# -------- set up for simulation ------------#
prefadj_50 <- redist::redist.adjacency(pref_50) # Adjacency list
#add edge
prefadj_50 <- geomander::add_edge(prefadj_50, ferries_50[, 1], ferries_50[, 2])

#manually edit adjacency list
#[270]13109 0250 品川区八潮
#[357]13111 0580 大田区東海
#[358]13111 0590 大田区城南島
prefadj_50 <- geomander::add_edge(prefadj_50, 270, 263) #[263]品川区東品川180
prefadj_50 <- geomander::add_edge(prefadj_50, 270, 261) #[261]品川区東大井160
prefadj_50 <- geomander::add_edge(prefadj_50, 270, 249) #[249]品川区勝島40
prefadj_50 <- geomander::add_edge(prefadj_50, 357, 358)
prefadj_50 <- geomander::add_edge(prefadj_50, 357, 308) #[308] 13111 0090大田区平和島

#[658]13120 0420 練馬区西大泉町
prefadj_50 <- geomander::add_edge(prefadj_50, 658, 659) #connect to [659]練馬区西大泉(６丁目) 0430


pref_map_50 <- redist::redist_map(pref_50,
                                  ndists = ndists_new,
                                  pop_tol= 0.12,
                                  total_pop = pop,
                                  adj = prefadj_50)

###save(list=ls(all=TRUE), file="13_ms_tokyo_data_50.Rdata")

# --------- MS simulation ----------------#
sim_ms_pref_50 <- redist::redist_mergesplit(pref_map_50,
                                            nsims = 100,
                                            counties = pref_map_50$code,
                                            constraints = "splits")

# save it
saveRDS(sim_ms_pref_50, paste("simulation/",
                              as.character(pref_code),
                              "_",
                              as.character(pref_name),
                              "_",
                              "smc",
                              "_",
                              as.character(nsims),
                              "_50",
                              ".Rds",
                              sep = ""))


##########MS (Only split large counties) Analysis #################
wgt_ms_50 <- simulation_weight_disparity_table(sim_ms_pref_50)
#m <- c(1:51)
#wgt_ms_50 <- cbind(m, wgt_ms_50)
#optimal <- wgt_ms_50$m[which(wgt_ms_50$max_to_min == min(wgt_ms_50$max_to_min))][1]
#Maxmin 1.6361
#redist::redist.plot.plans(sim_ms_pref_50, draws = optimal, geom = pref_map_50)

#county splits
plans_pref_50 <- redist::get_plans_matrix(sim_ms_pref_50)
# get splits
splits_50 <- count_splits(plans_pref_50, pref_map_50$code)
splits_50[optimal]

csplits_50 <- redist::redist.splits(plans_pref_50, pref_50$code)
csplits_50[optimal]

##########0 split###################
#-------- Use 2020 census data at the municipality level (0 splits)-----------#
pref_0 <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_0 <- merge_gun(pref_0)

#ferries
ferries_0 <- add_ferries(pref_0)
#[3]13103港区 -> [54]13420小笠原支庁 [52]13380三宅支庁 [51]13360大島支庁
#[52]13380三宅支庁 -> [53]13400八丈支庁

# -------- set up for simulation ------------#
# simulation parameters
prefadj_0 <- redist::redist.adjacency(pref_0) # Adjacency list

#add edge
prefadj_0 <- geomander::add_edge(prefadj_0,
                                 ferries_0[, 1],
                                 ferries_0[, 2])


pref_map_0 <- redist::redist_map(pref_0,
                                 ndists = ndists_new,
                                 pop_tol= 0.65,
                                 total_pop = pop,
                                 adj = prefadj_0)

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_0split.Rdata")

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
                              "smc",
                              "_",
                              as.character(nsims),
                              "_0",
                              ".Rds",
                              sep = ""))


########0 split Analysis###############
# extract plans
pref_smc_plans_0 <- redist::get_plans_matrix(sim_smc_pref_0)

# get disparity table data
wgt_smc_0 <- simulation_weight_disparity_table(sim_smc_pref_0)
#n <- c(1:25000)
#n <- as.data.frame(n)
#wgt_smc_0 <- cbind(n, wgt_smc_0)
#wgt_smc_0$n[which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))]
#Maxmin 7.2768 # 3    21    22    28    41...
#redist::redist.plot.plans(sim_smc_pref_0, draws = 3, geom = pref_map_0)

#Sq max 573,969 東京２２区 Sq min 482,077 東京25区 -> 1.1906

tokyo_13_optimalmap_0 <- redist::redist.plot.map(shp = pref_0,
                                                 plan = pref_smc_plans_0[, which(wgt_smc_0$max_to_min == min(wgt_smc_0$max_to_min))[1]],
                                                 boundaries = FALSE,
                                                 title = "Tokyo Optimal Plan (0-split)") +
  scale_fill_discrete() +
  labs(caption = paste("Max-min Ratio: ", round(min(wgt_smc_0$max_to_min), 3), sep = ""), hjust = 0.5)

#save(list=ls(all=TRUE), file="13_smc_tokyo_data_0to0splits.Rdata")


########Divide Tokyo into 6 Blocks###############
#clean pref_raw
pref <- pref_raw %>%
  clean_jcdf()

#define the number of blocks (divide Tokyo into 6 blocks)
n_blocks <- 6

#Further clean data
pref_block <- pref %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)
pref_block$subcode = "0000"

#Merge gun (No exceptions in this case; all the gun will be merged together)
pref_block <- merge_gun(pref_block)

#prepare for smc analysis (build adjacency list)
blockadj <- redist::redist.adjacency(shp = pref_block)

#ferries
ferries_block <- add_ferries(pref_block)
#[3]13103港区 -> [54]13420小笠原支庁 [52]13380三宅支庁 [51]13360大島支庁
#[52]13380三宅支庁 -> [53]13400八丈支庁

#add edge
blockadj <- geomander::add_edge(blockadj,
                                ferries_block[, 1],
                                ferries_block[, 2])

###save(list=ls(all=TRUE), file="13_smc_tokyo_data_block.Rdata")

#prepare for smc analysis (define map)
block_map <- redist::redist_map(pref_block,
                                ndists = n_blocks,
                                pop_tol = 0.1,
                                total_pop = pop,
                                adj = blockadj)

#smc split
smc_block <- redist::redist_smc(map = block_map,
                                nsims = 25000)

saveRDS(smc_block, paste("simulation/",
                         as.character(pref_code),
                         "_",
                         as.character(pref_name),
                         "_",
                         "smc",
                         "_",
                         as.character(nsims),
                         "_blocks",
                         ".Rds",
                         sep = ""))

#get plans
block_plans_pref <- redist::get_plans_matrix(smc_block)

#check disparity table
block_weight_pref <- simulation_weight_disparity_table(smc_block)
#n <- c(1:25000)
#n <- as.data.frame(n)
#block_weight_pref <- cbind(n, block_weight_pref)
#block_weight_pref$n[which(block_weight_pref$max_to_min == min(block_weight_pref$max_to_min))]
#Maxmin  1.0176 #49 412
#redist::redist.plot.plans(smc_block, draws = 412, geom = block_map)

#optimal plan
tokyo_blocks <- block_plans_pref[, which(block_weight_pref$max_to_min == min(block_weight_pref$max_to_min))[1]]

######Small units###########
small_units <- pref %>% dplyr::select(code, KIHON1, JINKO, geometry)
small_units <- calc_kokumin(small_units, dem_pops)
small_units <- estimate_2020_pop(small_units, census2020) %>%
  dplyr::select(code, KIHON1, pop_estimate, geometry) %>%
  dplyr::rename(subcode = KIHON1, pop = pop_estimate)

###save(list=ls(all=TRUE), file="13_tokyo_data_smallunitprep.Rdata")

######Block #1#############
#find the municipalities that belong to Block 1
part_codes_1 <- pref_block$code[which(tokyo_blocks == 1)] #Eastern Tokyo
largest_two_1 <- (pref_block[order(-pref_block$pop),] %>% dplyr::filter(code %in% part_codes_1))$code[1:1]
#13201八王子市 のみ

#filter out Block 1
pref_part_1 <- dplyr::bind_rows(small_units %>%
                                  dplyr::filter(code %in% largest_two_1), pref_block %>%
                                  dplyr::filter(!(code %in% largest_two_1) & code %in% part_codes_1))

#adjacency list for Block 1
part_adj_1 <- redist::redist.adjacency(shp = pref_part_1)

#define map
part_map_1 <- redist::redist_map(pref_part_1,
                                 ndists = ndists_new/n_blocks, #5 seats per block
                                 pop_tol = 0.27,
                                 total_pop = pop,
                                 adj = part_adj_1)
###save(list=ls(all=TRUE), file="13_tokyo_data_block1.Rdata")

#run smc
init_plan_1 <- redist::redist_smc(map = part_map_1,
                                  nsims = 1,
                                  pop_temper = 0.05)
init_plan_vec_1 <- redist::get_plans_matrix(init_plan_1)[,1]

#shortburst
part_smc_pref_1 <- redist::redist_shortburst(map = part_map_1,
                                             score_fn = 10*redist::scorer_pop_dev(part_map_1) +
                                               redist::scorer_splits(part_map_1, pref_part_1$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 2500,
                                             counties = pref_part_1$code,
                                             init_plan = init_plan_vec_1)

# save it
saveRDS(part_smc_pref_1, paste("simulation/",
                               sprintf("%02d", pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_",
                               "block_1",
                               ".Rds",
                               sep = ""))

# get plans, remove init
part_plans_pref_1 <- redist::get_plans_matrix(part_smc_pref_1)

# get disparity data
part_weight_pref_1 <- part_smc_pref_1 %>%
  dplyr::select("draw", "total_pop") %>%
  simulation_weight_disparity_table()

# get splits
part_splits_1 <- count_splits(part_plans_pref_1, part_map_1$code)

# optimal plan for Block 1
m <- c(1:2501)
m <- as.data.frame(m)
part_weight_pref_1 <- cbind(m, part_weight_pref_1)
part_weight_pref_1$m[which(part_weight_pref_1$max_to_min == min(part_weight_pref_1$max_to_min))]
#Maxmin 1.0065　#2370 2371 2372 2373 ...
redist::redist.plot.plans(part_smc_pref_1, draws = 2370, geom = part_map_1)

###save(list=ls(all=TRUE), file="13_tokyo_data_block1draws.Rdata")

######Block #2#############
#find the municipalities that belong to Block 2
part_codes_2 <- pref_block$code[which(tokyo_blocks == 2)] #世田谷, 杉並etc
largest_two_2 <- (pref_block[order(-pref_block$pop),] %>%
                    dplyr::filter(code %in% part_codes_2))$code[1:2]
#13112 世田谷区 #13115 杉並区

#filter out Block 2
pref_part_2 <- dplyr::bind_rows(small_units %>%
                                  dplyr::filter(code %in% largest_two_2), pref_block %>%
                                  dplyr::filter(!(code %in% largest_two_2) & code %in% part_codes_2))

#adjacency list for Block 2
part_adj_2 <- redist::redist.adjacency(shp = pref_part_2)

#define map
part_map_2 <- redist::redist_map(pref_part_2,
                                 ndists = ndists_new/n_blocks, #5 seats per block
                                 pop_tol = 0.27,
                                 total_pop = pop,
                                 adj = part_adj_2)
###save(list=ls(all=TRUE), file="13_tokyo_data_block2.Rdata")

#run smc
init_plan_2 <- redist::redist_smc(map = part_map_2,
                                  nsims = 1,
                                  pop_temper = 0.05)
init_plan_vec_2 <- redist::get_plans_matrix(init_plan_2)[,1]

#shortburst
part_smc_pref_2 <- redist::redist_shortburst(map = part_map_2,
                                             score_fn = 10*redist::scorer_pop_dev(part_map_2) +
                                               redist::scorer_splits(part_map_2, pref_part_2$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 2500,
                                             counties = pref_part_2$code,
                                             init_plan = init_plan_vec_2)

# save it
saveRDS(part_smc_pref_2, paste("simulation/",
                               sprintf("%02d", pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_",
                               "block_2",
                               ".Rds",
                               sep = ""))

# get plans, remove init
part_plans_pref_2 <- redist::get_plans_matrix(part_smc_pref_2)

# get disparity data
part_weight_pref_2 <- part_smc_pref_2 %>%
  dplyr::select("draw", "total_pop") %>%
  simulation_weight_disparity_table()

# get splits
part_splits_2 <- count_splits(part_plans_pref_2, part_map_2$code)

# optimal plan for Block 2
part_weight_pref_2 <- cbind(m, part_weight_pref_2)
part_weight_pref_2$m[which(part_weight_pref_2$max_to_min == min(part_weight_pref_2$max_to_min))]
#Maxmin 1.0030 #972  973  974  975  976  ...
redist::redist.plot.plans(part_smc_pref_2, draws = 972, geom = part_map_2)

###save(list=ls(all=TRUE), file="13_tokyo_data_block1draws.Rdata")

######Block #3#############
#find the municipalities that belong to Block 3
part_codes_3 <- pref_block$code[which(tokyo_blocks == 3)] #中野　練馬　府中
largest_two_3 <- (pref_block[order(-pref_block$pop),] %>%
                    dplyr::filter(code %in% part_codes_3))$code[1:4]
#13120 練馬区 #13114 中野区 #13206 府中市 (won't be split) #13229西東京市

#filter out Block 3
pref_part_3 <- dplyr::bind_rows(small_units %>%
                                  dplyr::filter(code %in% largest_two_3), pref_block %>%
                                  dplyr::filter(!(code %in% largest_two_3) & code %in% part_codes_3))

#adjacency list for Block 3
part_adj_3 <- redist::redist.adjacency(shp = pref_part_3)
#練馬区西大泉町13120 0420  is an enclave within 埼玉県新座市-> connect to 練馬区西大泉(６丁目) 0430
part_adj_3 <- geomander::add_edge(part_adj_3, 61, 62)

#define map
part_map_3 <- redist::redist_map(pref_part_3,
                                 ndists = ndists_new/n_blocks, #5 seats per block
                                 pop_tol = 0.27,
                                 total_pop = pop,
                                 adj = part_adj_3)
###save(list=ls(all=TRUE), file="13_tokyo_data_block3.Rdata")

#run smc
init_plan_3 <- redist::redist_smc(map = part_map_3,
                                  nsims = 1,
                                  pop_temper = 0.05)
init_plan_vec_3 <- redist::get_plans_matrix(init_plan_3)[,1]

#shortburst
part_smc_pref_3 <- redist::redist_shortburst(map = part_map_3,
                                             score_fn = 10*redist::scorer_pop_dev(part_map_3) +
                                               redist::scorer_splits(part_map_3, pref_part_3$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 2500,
                                             counties = pref_part_3$code,
                                             init_plan = init_plan_vec_3)

# save it
saveRDS(part_smc_pref_3, paste("simulation/",
                               sprintf("%02d", pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_",
                               "block_3",
                               ".Rds",
                               sep = ""))

# get plans, remove init
part_plans_pref_3 <- redist::get_plans_matrix(part_smc_pref_3)

# get disparity data
part_weight_pref_3 <- part_smc_pref_3 %>%
  dplyr::select("draw", "total_pop") %>%
  simulation_weight_disparity_table()

# get splits
part_splits_3 <- count_splits(part_plans_pref_3, part_map_3$code)

# optimal plan for Block 3
part_weight_pref_3 <- cbind(m, part_weight_pref_3)
part_weight_pref_3$m[which(part_weight_pref_3$max_to_min == min(part_weight_pref_3$max_to_min))]
#Maxmin 1.0161 #  2160 2161 2162 2163 2164 2165  ...
redist::redist.plot.plans(part_smc_pref_3, draws = 2160, geom = part_map_3)

###save(list=ls(all=TRUE), file="13_tokyo_data_block3draws.Rdata")

######Block #4#############
#find the municipalities that belong to Block 4
part_codes_4 <- pref_block$code[which(tokyo_blocks == 4)] #港区　新宿区　離島
largest_two_4 <- (pref_block[order(-pref_block$pop),] %>%
                    dplyr::filter(code %in% part_codes_4))$code[1:3]
#13111 大田区 #13109 品川区 #13104新宿区

#filter out Block 4
pref_part_4 <- dplyr::bind_rows(small_units %>%
                                  dplyr::filter(code %in% largest_two_4), pref_block %>%
                                  dplyr::filter(!(code %in% largest_two_4) & code %in% part_codes_4))

#adjacency list for Block 4
part_adj_4 <- redist::redist.adjacency(shp = pref_part_4)
#add ferries
ferries_4 <- add_ferries(pref_part_4)
#add edge
part_adj_4 <- geomander::add_edge(part_adj_4,
                                  ferries_4[, 1],
                                  ferries_4[, 2])
part_adj_4 <- geomander::add_edge(part_adj_4, 119, 112)
part_adj_4 <- geomander::add_edge(part_adj_4, 119, 110)
part_adj_4 <- geomander::add_edge(part_adj_4, 119, 98)
part_adj_4 <- geomander::add_edge(part_adj_4, 179, 130)
#connect [119]品川区八潮13109 250 to [112]品川区東品川180;[110]品川区東大井160 [98]品川区勝島40
#connect [179]大田区東海 13111 580to [130]東京都大田区平和島90

#define map
part_map_4 <- redist::redist_map(pref_part_4,
                                 ndists = ndists_new/n_blocks, #5 seats per block
                                 pop_tol = 0.27,
                                 total_pop = pop,
                                 adj = part_adj_4)
###save(list=ls(all=TRUE), file="13_tokyo_data_block4.Rdata")

#run smc
init_plan_4 <- redist::redist_smc(map = part_map_4,
                                  nsims = 1,
                                  pop_temper = 0.05)
init_plan_vec_4 <- redist::get_plans_matrix(init_plan_4)[,1]

#shortburst
part_smc_pref_4 <- redist::redist_shortburst(map = part_map_4,
                                             score_fn = 10*redist::scorer_pop_dev(part_map_4) +
                                               redist::scorer_splits(part_map_4, pref_part_4$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 2500,
                                             counties = pref_part_4$code,
                                             init_plan = init_plan_vec_4)

# save it
saveRDS(part_smc_pref_4, paste("simulation/",
                               sprintf("%02d", pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_",
                               "block_4",
                               ".Rds",
                               sep = ""))

# get plans, remove init
part_plans_pref_4 <- redist::get_plans_matrix(part_smc_pref_4)

# get disparity data
part_weight_pref_4 <- part_smc_pref_4 %>%
  dplyr::select("draw", "total_pop") %>%
  simulation_weight_disparity_table()

# get splits
part_splits_4 <- count_splits(part_plans_pref_4, part_map_4$code)

# optimal plan for Block 4
part_weight_pref_4 <- cbind(m, part_weight_pref_4)
part_weight_pref_4$m[which(part_weight_pref_4$max_to_min == min(part_weight_pref_4$max_to_min))]
#Maxmin 1.0038 #1693 1694 1695 1696 1697  ...
redist::redist.plot.plans(part_smc_pref_4, draws = 1693, geom = part_map_4)

###save(list=ls(all=TRUE), file="13_tokyo_data_block4draws.Rdata")

######Block #5#############
#find the municipalities that belong to Block 5
part_codes_5 <- pref_block$code[which(tokyo_blocks == 5)] #文京区　豊島区　北区
largest_two_5 <- (pref_block[order(-pref_block$pop),] %>%
                    dplyr::filter(code %in% part_codes_5))$code[1:3]
#13121 足立区 #13119 板橋区 #13117北区

#filter out Block 5
pref_part_5 <- dplyr::bind_rows(small_units %>%
                                  dplyr::filter(code %in% largest_two_5), pref_block %>%
                                  dplyr::filter(!(code %in% largest_two_5) & code %in% part_codes_5))

#adjacency list for Block 5
part_adj_5 <- redist::redist.adjacency(shp = pref_part_5)

#define map
part_map_5 <- redist::redist_map(pref_part_5,
                                 ndists = ndists_new/n_blocks, #5 seats per block
                                 pop_tol = 0.27,
                                 total_pop = pop,
                                 adj = part_adj_5)
###save(list=ls(all=TRUE), file="13_tokyo_data_block5.Rdata")

#run smc
init_plan_5 <- redist::redist_smc(map = part_map_5,
                                  nsims = 1,
                                  pop_temper = 0.05)
init_plan_vec_5 <- redist::get_plans_matrix(init_plan_5)[,1]

#shortburst
part_smc_pref_5 <- redist::redist_shortburst(map = part_map_5,
                                             score_fn = 10*redist::scorer_pop_dev(part_map_5) +
                                               redist::scorer_splits(part_map_5, pref_part_5$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 2500,
                                             counties = pref_part_5$code,
                                             init_plan = init_plan_vec_5)

# save it
saveRDS(part_smc_pref_5, paste("simulation/",
                               sprintf("%02d", pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_",
                               "block_5",
                               ".Rds",
                               sep = ""))

# get plans, remove init
part_plans_pref_5 <- redist::get_plans_matrix(part_smc_pref_5)

# get disparity data
part_weight_pref_5 <- part_smc_pref_5 %>%
  dplyr::select("draw", "total_pop") %>%
  simulation_weight_disparity_table()

# get splits
part_splits_5 <- count_splits(part_plans_pref_5, part_map_5$code)

# optimal plan for Block 5
part_weight_pref_5 <- cbind(m, part_weight_pref_5)
part_weight_pref_5$m[which(part_weight_pref_5$max_to_min == min(part_weight_pref_5$max_to_min))]
#Maxmin  1.0033 #1065 1066 1067 1068 1069  ...
redist::redist.plot.plans(part_smc_pref_5, draws = 1065, geom = part_map_5)

###save(list=ls(all=TRUE), file="13_tokyo_data_block5draws.Rdata")

######Block #6#############
#find the municipalities that belong to Block 6
part_codes_6 <- pref_block$code[which(tokyo_blocks == 6)] #千代田区中央区
largest_two_6 <- (pref_block[order(-pref_block$pop),] %>%
                    dplyr::filter(code %in% part_codes_6))$code[1:2]
#13123 江戸川区 #13108 江東区

#filter out Block 6
pref_part_6 <- dplyr::bind_rows(small_units %>%
                                  dplyr::filter(code %in% largest_two_6), pref_block %>%
                                  dplyr::filter(!(code %in% largest_two_6) & code %in% part_codes_6))

#adjacency list for Block 6
part_adj_6 <- redist::redist.adjacency(shp = pref_part_6)

#define map
part_map_6 <- redist::redist_map(pref_part_6,
                                 ndists = ndists_new/n_blocks, #5 seats per block
                                 pop_tol = 0.27,
                                 total_pop = pop,
                                 adj = part_adj_6)

#run smc
init_plan_6 <- redist::redist_smc(map = part_map_6,
                                  nsims = 1,
                                  pop_temper = 0.05)
init_plan_vec_6 <- redist::get_plans_matrix(init_plan_6)[,1]

#shortburst
part_smc_pref_6 <- redist::redist_shortburst(map = part_map_6,
                                             score_fn = 10*redist::scorer_pop_dev(part_map_6) +
                                               redist::scorer_splits(part_map_6, pref_part_6$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 2500,
                                             counties = pref_part_6$code,
                                             init_plan = init_plan_vec_6)

# save it
saveRDS(part_smc_pref_6, paste("simulation/",
                               sprintf("%02d", pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_",
                               "block_6",
                               ".Rds",
                               sep = ""))

# get plans, remove init
part_plans_pref_6 <- redist::get_plans_matrix(part_smc_pref_6)

# get disparity data
part_weight_pref_6 <- part_smc_pref_6 %>%
  dplyr::select("draw", "total_pop") %>%
  simulation_weight_disparity_table()

# get splits
part_splits_6 <- count_splits(part_plans_pref_6, part_map_6$code)

# optimal plan for Block 6
part_weight_pref_6 <- cbind(m, part_weight_pref_6)
part_weight_pref_6$m[which(part_weight_pref_6$max_to_min == min(part_weight_pref_6$max_to_min))]
#Maxmin 1.0035 #2377 2378 2379 2380 2381  ...
redist::redist.plot.plans(part_smc_pref_6, draws = 2377, geom = part_map_6)

###Save RESULTS
save(list=ls(all=TRUE), file="13_tokyo_data_block6draws.Rdata")

########Check intra-prefecture maxmin ratio################
pop_by_district <- dplyr::bind_rows(part_smc_pref_1 %>% filter(draw %in% "2500"),
                                    part_smc_pref_2 %>% filter(draw %in% "2500"),
                                    part_smc_pref_3 %>% filter(draw %in% "2500"),
                                    part_smc_pref_4 %>% filter(draw %in% "2500"),
                                    part_smc_pref_5 %>% filter(draw %in% "2500"),
                                    part_smc_pref_6 %>% filter(draw %in% "2500"))

redis_maxmin <- max(pop_by_district$total_pop)/min(pop_by_district$total_pop)
#1.0209
#sq_maxmin <- 573969 / 482077 #1.190617

#########Results##########
pref_results <- data.frame(matrix(ncol = 0, nrow = n_blocks*nrow(part_weight_pref_1)))
pref_results$block <- c(rep(1, nrow(part_weight_pref_1)),
                        rep(2, nrow(part_weight_pref_2)),
                        rep(3, nrow(part_weight_pref_3)),
                        rep(4, nrow(part_weight_pref_4)),
                        rep(5, nrow(part_weight_pref_5)),
                        rep(6, nrow(part_weight_pref_6)))
pref_results$index <- rep(1:nrow(part_weight_pref_1), n_blocks)
pref_results$max_to_min <- c(part_weight_pref_1$max_to_min,
                             part_weight_pref_2$max_to_min,
                             part_weight_pref_3$max_to_min,
                             part_weight_pref_4$max_to_min,
                             part_weight_pref_5$max_to_min,
                             part_weight_pref_6$max_to_min)
pref_results$splits <- c(part_splits_1,
                         part_splits_2,
                         part_splits_3,
                         part_splits_4,
                         part_splits_5,
                         part_splits_6)

pref_uniques <- pref_results %>%
  dplyr::group_by(block, splits) %>%
  dplyr::summarize(max_to_min = min(max_to_min))

###Save RESULTS
save(list=ls(all=TRUE), file="13_tokyo_data_results_6.Rdata")

############Loop#######################
#------------- set up map ----------------#
for (j in 1:n_blocks) {

  part_codes <- pref_block$code[which(tokyo_blocks == j)]
  largest_two <- (pref_block[order(-pref_block$pop),] %>% dplyr::filter(code %in% part_codes))$code[1:2]

  pref_part <- dplyr::bind_rows(small_units %>% dplyr::filter(code %in% largest_two), pref_block %>% dplyr::filter(!(code %in% largest_two) & code %in% part_codes))

  part_adj <- redist::redist.adjacency(shp = pref_part) # Adjacency list

  if(check_ferries(pref_code) == TRUE){
    # add ferries
    ferries <- add_ferries(pref_part)

    if(nrow(ferries) > 0) {
      part_adj <- geomander::add_edge(part_adj,
                                      ferries[, 1],
                                      ferries[, 2],
                                      zero = TRUE)
    }

  }

  neighbor <- geomander::suggest_neighbors(shp = pref_part,
                                           adjacency = part_adj)

  if(nrow(neighbor) > 0) {

    part_adj <- geomander::add_edge(part_adj,
                                    neighbor$x,
                                    neighbor$y,
                                    zero = TRUE)
  }

  if(length(unique((geomander::check_contiguity(part_adj))$component)) > 1) {

    suggest <- geomander::suggest_component_connection(shp = pref_part,
                                                       adjacency = part_adj,
                                                       group = match(pref_part$code, unique(pref_part$code)))

    part_adj <- geomander::add_edge(part_adj,
                                    suggest$x,
                                    suggest$y,
                                    zero = TRUE)

  }

  part_map <- redist::redist_map(pref_part,
                                 ndists = ndists_new/n_blocks,
                                 pop_tol = 0.30,
                                 total_pop = pop,
                                 adj = part_adj)

  init_plan <- redist::redist_smc(map = part_map,
                                  nsims = 1,
                                  pop_temper = 0.05)

  init_plan_vec <- redist::get_plans_matrix(init_plan)[,1]

  part_smc_pref <- redist::redist_shortburst(map = part_map,
                                             score_fn = 10*redist::scorer_pop_dev(part_map) + redist::scorer_splits(part_map, pref_part$code),
                                             maximize = FALSE,
                                             burst_size = 10,
                                             max_bursts = 1000,
                                             counties = pref_part$code,
                                             init_plan = init_plan_vec)

  # save it
  saveRDS(part_smc_pref, paste("simulation/",
                               sprintf("%02d", pref_code),
                               "_",
                               as.character(pref_name),
                               "_",
                               as.character(sim_type),
                               "_",
                               as.character(nsims),
                               "_",
                               "block_",
                               as.character(j),
                               ".Rds",
                               sep = ""))

  # get plans, remove init
  part_plans_pref <- redist::get_plans_matrix(part_smc_pref)

  # get disparity data
  part_weight_pref <- simulation_weight_disparity_table(part_smc_pref)

  # get splits
  part_splits <- count_splits(part_plans_pref, part_map$code)

  # rename elements to be used
  assign(paste(pref_name, pref_code, "block", j, sep = "_"),
         pref_part)
  assign(paste(pref_name, pref_code, "adj", "block", j, sep = "_"),
         part_adj)
  assign(paste(pref_name, pref_code, "map", "block", j, sep = "_"),
         part_map)
  assign(paste(pref_name, pref_code, "sim", sim_type, "block", j, sep = "_"),
         part_smc_pref)
  assign(paste(pref_name, pref_code, sim_type, "plans", "block", j, sep = "_"),
         part_plans_pref)
  assign(paste(pref_name, pref_code, sim_type, "weight", "block", j, sep = "_"),
         part_weight_pref)
  assign(paste(pref_name, pref_code, sim_type, "splits", "block", j, sep = "_"),
         part_splits)

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
  )
  )])

}
