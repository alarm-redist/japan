library(tidyverse)

# ----------- set up -------------#
# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

#-------- Clean data -----------#
# Clean data
pref_raw <- download_shp(34) #34:Hiroshima
pref <- clean_jcdf(pref_raw = pref_raw)

#2020 census
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
#clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

#check to see pop ranking by mun
census2020 %>%
  filter(code > 34200 & code < 35000) %>% #excluding 34100 Hiroshima City
  arrange(desc(pop_national))
#Excluding Hiroshima City, Top 4 is
#34207: 福山 (currently NOT split; pre-gappei: 207 福山 481 内海 482沼隈 501神辺 524新市)
#34202: 呉 (currently NOT split; pre-gappei: 202呉 311音戸 312倉橋 313下蒲刈 324蒲刈 423安浦 424川尻 425豊浜 426豊)
#34212: 東広島 (currently split; pre-gappei: 212東広島 402黒瀬 405福富 406豊栄 408河内 422安芸津)
#34205: 尾道(currently split; pre-gappei: 205尾道 206因島 430瀬戸田 441御調 444向島)
#Since 4 municipalities (excluding Hiroshima City, a seireishiteitoshi) are currently split,
#the 1st ~ 4th largest municipalities will be split.
#江田島市 and 三原市, which are currently split, will no longer be split.

#Estimate 2020 Pop at the 小地域-level (pref is currently based on 2015 Census)
#*Note: when splitting the 4 municipalities based on the pre-gappei boundaries,
#*the poplation data will be based on the 2015 Census
#*this inconsistency will be resolved once we get access to the 2020 Census


#-------- SMC: 0 split -----------#

#-------- SMC: 1 split -----------#

#######enumeration##################

#######save as RData###############
