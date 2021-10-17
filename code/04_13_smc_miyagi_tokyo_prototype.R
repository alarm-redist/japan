#Urban Prefectures (Tokyo as an example)
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

#########Merge_gun: Create Gun Codes & retain original code##############
pref <- merge_gun(pref)
#or pref <- merge_gun(pref, merge_gun_exception)

##########Finalize pref object#############
#to merge specific guns
merge_gun_pref <- pref %>%
  dplyr::filter(gun_code %in% c(13380, 13400)) %>%
  dplyr::group_by(gun_code) %>%
  dplyr::summarize(geometry = sf::st_union(geometry),
                   pop = sum(pop),
                   subcode = subcode[1],
                   code = gun_code[1])
rest_of_pref <- pref %>%
  dplyr::filter(gun_code %in% c(13380, 13400) == FALSE)
pref <- dplyr::bind_rows(merge_gun_pref, rest_of_pref)

#to merge all gun
merge_gun_pref <- pref %>%
  dplyr::filter(gun_code >= 13300) %>%
  dplyr::group_by(gun_code) %>%
  dplyr::summarize(geometry = sf::st_union(geometry),
                   pop = sum(pop),
                   subcode = subcode[1],
                   code = gun_code[1])
rest_of_pref <- pref %>%
  dplyr::filter(gun_code <13300)
pref <- dplyr::bind_rows(merge_gun_pref, rest_of_pref)










#Rural Prefectures (Miyagi as an example)
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
pref_code <- 04
pref_name <- "miyagi"
ndists_new <- 5
ndists_old <- 6

#------- Specify municipality splits -------------
# enter `c()` if not applicable
# number of splits
nsplit <- 2
merge_gun_exception <- c(4401, 4421)

######### Download and Clean Census ############
# download census shp
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code)

# download 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
# Clean 2020 census
census2020 <- clean_2020_census(total = total, foreigner = foreigner)
#save(census2020, file = "census2020.RData")

pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)

# fix 富谷町 --> 富谷市 (2016 changes)
pref[which(pref$code == 4423), ]$pop <-  (census2020 %>% dplyr::filter(code == 4216, ))$pop_national
pref[which(pref$code == 4423), ]$code <- 4216

# download historical boundary data
old_boundary <- download_old_shp(pref_code = pref_code)

# populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

#########Merge_gun: Create Gun Codes & retain original code##############
pref <- merge_gun(pref)
#or pref <- merge_gun(pref, merge_gun_exception)

#########Split municipality (reflect old boundary) & Estimate 2020 Pop#############
old_code <- find_old_codes("04202", pop_by_old_boundary)
new_code <- "04202"
pref <- reflect_old_boundaries(pref, old_boundary, pop_by_old_boundary, old_code, new_code)
pref <- estimate_old_boundary_pop(old_code, new_code, pref, census2020)

##########Finalize pref object#############
#Merge gun
#Note that if a user specifies exceptions when using merge_gun(), a unique gun_code is
#given to those municipalities. Thus, running these lines do not result in merging together the exceptions.
pref <- pref %>%
  dplyr::group_by(gun_code) %>%
  dplyr::summarize(geometry = sf::st_union(geometry),
                   pop = sum(pop),
                   pre_gappei_code = gun_code[1],
                   code = gun_code[1])

