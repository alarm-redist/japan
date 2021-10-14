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
pref_10 <- pref$code[1] %/% 1000 #2-digit prefecture code
pref_code <- pref_10*1000

#Create a dataframe without any data
pref_interm <- pref
pref_separate <- pref_interm[ !(pref_interm$code %in% pref$code), ]

#Create a data frame that contains all the municipalities that belong to a "gun" that the user does not wish to group together
for(i in 1:length(merge_gun_exception)){
  mod <- merge_gun_exception %% pref_code
  div_twenty <- mod %/% 20
  separate_gun_code <- pref_code + div_twenty * 20
  pref_to_separate <- pref %>%
    filter(between(code, separate_gun_code[i], separate_gun_code[i] + 19))
  pref_separate <- dplyr::bind_rows(pref_to_separate, pref_separate)
}

#Create a data frame that includes the municipalities that need to be grouped together
pref_to_group <- setdiff(pref, pref_separate)
pref_to_group <- pref_to_group %>%
  mutate(gun_code = code %% pref_code)

#the municipality codes of all the municipalities that do not belong to "gun" are kept the same
for(i in 1:length(pref_to_group$gun_code)){
  if(pref_to_group$gun_code[i] < 300){
    pref_to_group$gun_code[i] <-  pref_to_group$code[i]

  }

  #group together the municipalities that belong to a "gun"
  else{
    pref_to_group$gun_code[i] <-  cut(pref_to_group$gun_code[i],
                                  breaks = c(300, 320, 340, 360, 380,
                                             400, 420, 440, 460, 480,
                                             500, 520, 540, 560, 580,
                                             600, 620, 640, 660, 680,
                                             700, 720, 740, 760, 780, 800))
  }
}


#relabel the municipality codes of the municipalities that belong to "gun"
for(i in 1:20){
  pref_to_group$gun_code[pref_to_group$gun_code == i] <- pref_code + 300 + 20*(i-1)
}

#merge back together the municipalities set as exceptions
pref_separate$gun_code <- pref_separate$code
pref <- bind_rows(pref_to_group, pref_separate)

#########Split municipality (reflect old boundary): retain original code#############
old_code <- find_old_codes("04202", pop_by_old_boundary)
old_code_char <- paste0(0, as.character(old_code))
new_code <- 4202
new_code_char <- paste0(0, as.character(new_code))

#Match CRS
sf::st_crs(old_boundary) <- sf::st_crs(pref)

#filter out the municipalities that will not be split
#(i.e. no need to take into account old boundaries)
post_gappei_except_for_designated_city <- pref %>%
  dplyr::filter(code != new_code)

#Clean the data on old boundaries
pre_gappei_geom <- old_boundary %>%
  dplyr::filter(N03_007 %in% old_code_char) %>%
  dplyr::select(N03_004, N03_007, geometry)
names(pre_gappei_geom) <- c("municipality", "pre_gappei_code", "geometry")
pre_gappei_geom <- sf::st_make_valid(pre_gappei_geom)
pre_gappei_geom <- pre_gappei_geom %>%
  dplyr::group_by(pre_gappei_code) %>%
  dplyr::summarise(geometry =  sf::st_union(geometry))

#get rid of unnecessary rows and columns
cleaned_pop_by_old_boundary <- pop_by_old_boundary %>%
  slice(-c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
  select(X, X.1, X.4, X.5)
#column X: municipality code; X.1:shikibetsu code(9 corresponds to the old municipalities)
#column X.4: name of old municipality; X.5: population as of 2015

#select the data we are interested in
pop_data <- cleaned_pop_by_old_boundary %>%
  filter(X %in% new_code_char) %>%
  filter(X.1 %in% 9) %>%
  #"9" corresponds to the old municipalities (cf.shikibetsu code)
  select(X, X.4, X.5)
names(pop_data) <- c("code", "municipality", "pop")
#obtain the last three digits of the old municipality codes
pop_data <- pop_data %>%
  separate(municipality, into = c("a", "pre_code", "b"), sep = " ") %>%
  select(code, pre_code, pop)
pop_data$code <- as.numeric(pop_data$code)

#generate functioning pre_gappei codes
pref_10<- pref$code[1] %/% 1000 #prefecture_code
a <- lapply(pref_10,  paste0, pop_data$pre_code)
b <- as.data.frame(a, col.names = "pre_gappei_code")

#final version of 2015(2020) population based on old boundaries
pop_data <- bind_cols(b, pop_data)
pop_data = subset(pop_data, select = -c(pre_code))
pop_data$pre_gappei_code <- as.character(paste0(0, as.character(pop_data$pre_gappei_code)))

#match with dataframes based on municipality code
old_joined <- merge(pre_gappei_geom, pop_data, by = "pre_gappei_code")
old_joined$pre_gappei_code <- as.numeric(old_joined$pre_gappei_code)
old_joined$pop <- as.numeric(old_joined$pop)
old_joined$gun_code <- old_joined$code
old_joined <- as_tibble(old_joined)
old_joined <- old_joined[, c(2, 3, 4, 5, 1)]

#merge with the data that excludes the designated city
post_gappei_except_for_designated_city$pre_gappei_code <- post_gappei_except_for_designated_city$code
merged <- bind_rows(old_joined, post_gappei_except_for_designated_city)

###########estimate 2020 pop#############
# population of Japanese nationals in municipality new_code for 2020
nat_2020 <- (census2020 %>% dplyr::filter(code == new_code, ))$pop_national
pop_2015 <- sum(merged[which(merged$code %in% old_code), ]$pop)

# conduct estimates using simple rounding proportional method
for (i in 1:length(old_code)) {
  merged[which(merged$code == old_code[i]), ]$pop <-
    round(merged[which(merged$code == old_code[i]), ]$pop / pop_2015 * nat_2020)
}

pref <- merged

############広域連携###################
ishinomaki_koiki <- c(4202, 4214, 4581)

pref$koiki_code <- pref$code

koiki_1 <- pref %>% filter(code %in% ishinomaki_koiki)
koiki_1$koiki_code <- rep(1, times = length((pref %>% filter(code %in% ishinomaki_koiki))$koiki_code))

remainder <- pref %>% filter(code %in% ishinomaki_koiki == FALSE)

pref <- dplyr::bind_rows(koiki_1, remainder)
present <- pref %>%
  select(code, pop, gun_code, pre_gappei_code, koiki_code)


