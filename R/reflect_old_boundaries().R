###########Notes(to be deleted once we establish workflow)#########################
#<How to Match Old Municipalities with New Merged Ones (after Gappei)>
#0. Run clean_jcdf on 2015(2020) census. Ex: toyama <- clean_jcdf(toyama_raw)
#1. Run merge_small on 2015(2020) census. Ex: toyama <- merge_small(toyama, split_codes = c(16201))
#2.Specify the codes of the old municipalities that presently belong to a *single* municipality.
#Use https://www.e-stat.go.jp/municipalities/cities to check the codes of the old municipalities.
#Run step 2 as many times as you need to, depending on how many municipalities you wish to split.
#3. Run merge_gun


#########Step 1: Download shapefile based on old boundaries##############
#from https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-v3_0.html
#unfortunately, it seems impossible to automate this process
old_boundary <- sf::st_read("data/N03-001001_16-g_AdministrativeBoundary.shp")

#########Step 2: Download the csv file with info on 2020 pop based on old boundaries######
#from https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031472195&fileKind=1
#not sure how to automate this process
pop_by_old_boundary <- read.csv("data/001_16.csv", fileEncoding = "shift-jis")

#########Step 3 This process worked with Toyama, will make it into a function##########
sf::st_crs(old_boundary) <- sf::st_crs(toyama)

old_code <- c(16201, 16301, 16302, 16361, 16362, 16363, 16364)
new_code <- 16201

pre_gappei_geom <- old_boundary %>%
  dplyr::filter(N03_007 %in% old_code) %>%
  dplyr::select(N03_004, N03_007, geometry)
names(pre_gappei_geom) <- c("municipality", "code", "geometry")

post_gappei_except_for_designated_city <- toyama %>%
  dplyr::filter(code != new_code) %>%
  dplyr::select(code, pop, geometry)

#get rid of unnecessary rows and columns
cleaned_pop_by_old_boundary <- pop_by_old_boundary %>%
  slice(-c(1, 2, 3, 4, 5, 6, 7, 8, 9)) %>%
  select(X, X.1, X.4, X.5)
#column X: municipality code; X.1:shikibetsu code(9 corresponds to the old municipalities) 
#column X.4: name of old municipality; X.5: population as of 2015

#select the data we are interested in
pop_data <- cleaned_pop_by_old_boundary %>%
  filter(X %in% new_code) %>%
  filter(X.1 %in% 9) %>%
  #9 corresponds to the old municipalities
  select(X, X.4, X.5)
names(pop_data) <- c("null_code", "municipality", "pop")
pop_data <- pop_data %>%
  separate(municipality, into = c("a", "old_code", "b"), sep = " ") %>%
  select(old_code, pop) 

#generate functioning municipality codes
pref_10<- toyama$code[1] %/% 1000 #prefecture_code
a <- lapply(pref_10,  paste0, pop_data$old_code)
b <- as.data.frame(a, col.names = "code")

#final version of 2015(2020) population based on old boundaries
pop_data <- bind_cols(b, pop_data)

#match with shapefile based on municipality code
old_joined <- merge(pre_gappei_geom, pop_data, by = "code")
old_joined_simp <- old_joined %>%
  select(code, pop, geometry)
old_joined_simp$code <- as.numeric(old_joined_simp$code)
old_joined_simp$pop <- as.numeric(old_joined_simp$pop)

#merge with the data that excludes the designated city
merged <- dplyr::bind_rows(old_joined_simp, post_gappei_except_for_designated_city)

#redist.plot.map(shp = merged) + theme_map() + theme(legend.position = 'none')
#mergedadj <- redist.adjacency(merged)
#toyama_map <- redist_map(merged, ndists=3, pop_tol= 0.30, total_pop = pop, adj = mergedadj)
#toyama_smc <- redist_smc(map = toyama_map, nsims = 10, compactness = 1.9) 
#redist.plot.plans(toyama_smc, draws = 1:6, geom = toyama_map)


