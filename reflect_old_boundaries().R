#<How to Match Old Municipalities with New Merged Ones (after Gappei)>
#0.Obtain latest census data. 
#1.Download old shapefile that reflects the boundaries as of 2000 using download_old_shp()
#2.We need the raw data in order for the function to run properly
#3.Specify the codes of the old municipalities that presently belong to a *single* municipality.
#Use https://www.e-stat.go.jp/municipalities/cities to check the codes of the old municipalities.
#Run step 2 as many times as you need to, depending on how many municipalities you wish to split.

#old_data <- download_old_shp(pref_code)
download_old_shp <- function(pref_code){
  # pad with two zeros
  pref_code <- sprintf('%02d', pref_code)
  
  # check if data/ folder exists in working directory
  if(!file.exists("data")){
    # if not, create data folder
    dir.create("data")
  }
  
  # download the files from the Japanese Government Statistics Portal (e-Stat)
  download.file(paste('https://www.e-stat.go.jp/gis/statmap-search/data?dlserveyId=A002005212000&code=',
                      as.character(pref_code), '&coordSys=1&format=shape&downloadType=5&datum=2000', sep = ''), 'data/shp_data_2000.zip')
  
  # unzip the downloaded zip file
  unzip("data/shp_data_2000.zip", exdir = "data")
  
  # remove the zip file after zip is decompressed
  file.remove("data/shp_data_2000.zip")
  
  # return the shp file
  pref_raw <- sf::st_read(paste("data/h12ka", as.character(pref_code), '.shp', sep = ''))
  return(pref_raw)
}


toyama_old_raw <- sf::st_read("data/h12ka16.shp")
toyama_new_raw <- sf::st_read("data/h27ka16.shp")

sf::st_crs(toyama_new_raw) <- sf::st_crs(toyama_old_raw)
toyama_old_raw<- sf::st_transform(toyama_old_raw, sf::st_crs(toyama_new_raw))

old_code <- c(16201, 16301, 16302, 16361, 16362, 16363, 16364)
new_code <- 16201

# filter out water surfaces and extraneous port data
toyama_old <- toyama_old_raw %>%
  dplyr::filter(toyama_old_raw$KIHON1 != "0000" & toyama_old_raw$HCODE != 8154, )  %>%
  dplyr::group_by(PREF, CITY, KIHON1, CITY_NAME) %>%
  # make smallest geopolitical subdivision to level 2
  dplyr::summarize(geometry = sf::st_union(geometry)) %>%
  # make 5 digit municipality code
  dplyr::mutate(code = 1000*as.numeric(PREF) + as.numeric(CITY)) %>%
  dplyr::select(code, geometry)

toyama_new <- toyama_new_raw %>%
  dplyr::filter(toyama_new_raw$KIHON1 != "0000" & toyama_new_raw$HCODE != 8154, )  %>%
  dplyr::group_by(PREF, CITY, KIHON1, CITY_NAME, S_NAME) %>%
  # make smallest geopolitical subdivision to level 2
  dplyr::summarize(geometry = sf::st_union(geometry), JINKO = sum(JINKO)) %>%
  # make 5 digit municipality code
  dplyr::mutate(code = 1000*as.numeric(PREF) + as.numeric(CITY)) %>%
  dplyr::select(code, CITY_NAME, S_NAME, JINKO, geometry) %>%
  #filter out specified city
  dplyr::filter(code %in% new_code) 

geom_old <- toyama_old %>%
  filter(code %in% old_code[6]) %>%
  group_by(code) %>%
  summarise(geometry = st_union(geometry)) %>%
  select(geometry)
redist.plot.map(shp = geom_old) +theme_map()


a <- st_intersection(toyama_new, geom_old)
View(a)
e <- st_intersects(toyama_new, geom_old)
View(e)

#Nearest neighbors
https://www.rdocumentation.org/packages/nngeo/versions/0.4.3/topics/st_nn
st_nn(toyama_new, geom_old)

b <- st_filter(toyama_new, geom_old)
View(b)
c <- st_crop(toyama_new, geom_old)
View(c)
#d <- st_difference(toyama_new, geom_old)
#View(d)
e <- st_filter(toyama_new, geom_old, .predicate = st_within)
View(e)



