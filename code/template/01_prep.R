###############################################################################
# Download and prepare data for `[TODO]` analysis
# © ALARM Project, November 2021
###############################################################################

suppressMessages({
    library(dplyr)
    library(readr)
    library(sf)
    library(redist)
    library(geomander)
    library(cli)
    library(here)
    library(tidyverse)
    devtools::load_all() # load utilities
})

set.seed(12345)

# Pull functions
setwd("R")
files.sources = list.files()
sapply(files.sources, source)
setwd("..")

# TODO: Define parameters for simulation
sim_type <- "smc"
nsims <- 25000
pref_code <- 0
pref_name <- ""
lakes_removed <- c() 
ndists_new <- 0
ndists_old <- 0
n_split <- 0

# Change time limit
options(timeout = 300) 

# Download census shapefile
pref_raw <- download_shp(pref_code)
dem_pops <- download_pop_demographics(pref_code)

# Download and clean 2020 census data
total <- download_2020_census(type = "total")
foreigner <- download_2020_census(type = "foreigner")
census2020 <- clean_2020_census(total = total, foreigner = foreigner)

# Download data from old boundaries (pre-平成の大合併)
old_boundary <- download_old_shp(pref_code = pref_code)

# Populations based on historical boundaries
pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)
