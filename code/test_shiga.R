library(tidyverse)

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

# Clean data
pref_raw <- download_shp(25)
pref <- clean_jcdf(pref_raw = pref_raw)

pref <- remove_lake(pref, "琵琶湖")

total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")

census2020 <- clean_2020_census(total = total, foreigner = foreigner)

merged <- pref %>%
  group_by(code, CITY_NAME) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code'))

merged %>%
  ggplot() +
  geom_sf(fill = "red")
