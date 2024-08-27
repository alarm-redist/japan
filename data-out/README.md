# `data-out` README

The `data-out` folder contains draft, unvalidated redistricting simulations and supporting files.
It is not tracked by git.  Only validated analyses should be saved to the repository.

- The `map/` subfolder contains `redist_map` objects, stored as RDS files,
  which contain geographic, adjacency, and demographic information on analyzed
  prefectures.
- The `adj/` subfolder contains `redist_adj` objects, stored as RDS files,
  which contain geographic adjacency information.
- The `shapefile/` subfolder contains `sf` objects, stored as RDS files, which is the cleaned shapefile from the original census data. 
- The `plans/` subfolder contains post-processed and randomly sampled 5000 `redist_plans` objects, stored as RDS files,
  which contain the sampled district assignments and populations. 
- The `smc-out/` subfolder contains the raw output of `redist_smc`, stored as RDS files,
  which contain the sampled district assignments and populations. 
- The `stats/` subfolder contains CSV files containing summary information
  on all of the sampled plans.
- The `environment/` subfolder contains Rdata files that save the R workspace with selected objects and variables. 
