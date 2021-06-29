# libs ----
library(tidyverse)
library(redist)
library(here)

# read data ----
pref_map <- readRDS(here('pref_map.Rds'))


makecontent <- readLines(system.file('enumpart/Makefile', package = 'redist'))
makecontent[7] <- "\tg++ enumpart.cpp SAPPOROBDD/bddc.o SAPPOROBDD/BDD.o SAPPOROBDD/ZBDD.o -o enumpart -I$(TDZDD_DIR) -std=c++11 -O3 -DB_64 -DNDEBUG"
writeLines(text = makecontent, con = system.file('enumpart/Makefile', package = 'redist'))

# small map -- enumerate, don't sample!
if(TRUE){ # change to TRUE if you need to init
  redist.init.enumpart()
}

all_plans <- redist.enumpart(adj = get_adj(pref_map),
                             unordered_path = here('data/unord'),
                             ordered_path = here('data/ord'),
                             out_path = here('data/enum'),
                             ndists = 3, all = TRUE,
                             total_pop = pref_map[[attr(pref_map, 'pop_col')]])

good_plans <- all_plans[[1]][, all_plans[[2]] < get_pop_tol(pref_map)]

plans <- redist_plans(good_plans, pref_map, algorithm = 'enumpart')

