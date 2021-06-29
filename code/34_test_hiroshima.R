library(tidyverse)

# pull functions from jcdf
# set working directory to the function folder
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
# set working directory back to `jcdf`
setwd("..")

# Clean data
pref_raw <- download_shp(34) #34:Hiroshima
pref <- clean_jcdf(pref_raw = pref_raw)
