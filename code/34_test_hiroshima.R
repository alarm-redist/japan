library(tidyverse)

# ----------- set up -------------#
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- Clean data (2015 Census)-----------#
# Clean data
pref_raw <- download_shp(34) #34:Hiroshima
pref <- clean_jcdf(pref_raw = pref_raw)
#JINKO in pref includes foreigners too -> calculate Japanese population
age_pops <- download_pop_demographics(34) #first download data on pop by age group
pref <- calc_kokumin(pref, age_pops)

#-------- Update population (Estimate based on 2020 Census)-----------#
# Download 2020 census
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

#check to see pop ranking by mun
census2020 %>%
  filter(code > 34200 & code < 35000) %>% #excluding 34100 Hiroshima City
  arrange(desc(pop_national))
#Excluding Hiroshima City, Top 4 Muns
#34207: 福山 (currently NOT split; pre-gappei: 207 福山 481 内海 482沼隈 501神辺 524新市)
#34202: 呉 (currently NOT split; pre-gappei: 202呉 311音戸 312倉橋 313下蒲刈 324蒲刈 423安浦 424川尻 425豊浜 426豊)
#34212: 東広島 (currently split; pre-gappei: 212東広島 402黒瀬 405福富 406豊栄 408河内 422安芸津)
#34205: 尾道(currently split; pre-gappei: 205尾道 206因島 430瀬戸田 441御調 444向島)
#Since 4 municipalities (excluding Hiroshima City, a seireishiteitoshi) are currently split,
#the 1st ~ 4th largest municipalities will be split.
#江田島市 and 三原市, which are currently split, will no longer be split.

#Estimate 2020 Pop at the 小地域-level (pref is currently based on 2015 Census)
#*Note: when splitting the 4 municipalities based on the pre-gappei boundaries,
#*the population data will be based on the 2015 Census
#*this inconsistency will be resolved once we get access to the 2020 Census
pop <- estimate_2020_pop(pref, census2020)
pop <- as.data.frame(pop)
pref <- bind_cols(pref, pop)
#The column "pop" in "pref" now indicates the "estimated number of Japanese nationals"

# -------- Ferries ------------#
add_ferries(pref)
#******************did not work, not sure why**********************#


##############First try with 0 splits#######################
pref0 <- merge_small(pref, intact_codes = c(34101, 34102, 34103, 34104, 34105,
                                            34106, 34107, 34108))
#made sure to group together the 8 Wards of Hiroshima City together
#wards are treated as single municipalities; splitting Hiroshima City doesn't count as a municipality split

#Will skip reflect_old_boundaries() for now because I do not split any municipalities

pref0 <- merge_gun(pref0)

# -------- set up for simulation ------------#
# simulation parameters
prefadj <- redist::redist.adjacency(pref) # Adjacency list
# set number of district (check external information)
ndists_new <- 3
ndists_old <- 4
pref_map <- redist::redist_map(pref,
                               ndists = ndists_new,
                               pop_tol= 0.08,
                               total_pop = pop_national,
                               adj = prefadj)

# --------- Merge Split simulation ----------------#
sim_ms_pref <- redist::redist_mergesplit(map = pref_map,
                                         nsims = 25000,
                                         warmup = 1,
                                         compactness = 1.4)

redist::redist.plot.plans(sim_ms_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "Merge Split")

# --------- SMC simulation ----------------#
# simulation
sim_smc_pref <- redist::redist_smc(pref_map,
                                   nsims = 25000,
                                   pop_temper = 0.05)
# test with map
redist::redist.plot.plans(sim_smc_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "SMC")

# -------- enumeration ------------#
# simulation
sim_enumerate_pref <- redist::redist.enumerate(prefadj,
                                               ndists = ndists_new,
                                               popvec = pref$pop_national,
                                               nconstraintlow = NULL,
                                               nconstrainthigh = NULL,
                                               popcons = NULL,
                                               contiguitymap = "rooks")
# test with map
redist::redist.plot.plans(sim_enumerate_pref,
                          draws = 1:6,
                          geom = pref_map) +
  labs(caption = "Enumeration")
