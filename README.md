# jcdf

# Work Flow

## Download Data   
1. `download_shp`
2. `download_2020_census`

## Clean Data 
1. `remove_lake`(case by case)
2. `clean_jcdf`
3. `clean_2020_census`
4. Combine them. For example,   
   `combined <- pref %>%   
               dplyr::group_by(code, CITY_NAME) %>%  
               summarise(geometry = sf::st_union(geometry)) %>%  
               dplyr::left_join(census2020, by = c('code'))`
               
5. `get_municode`
6. `merge_small`
7. `merge_gun`
8. Test if the maps make sense
9. `status_quo_match`
10. `add_ferries`

## Simulation
1. `redist_smc` or `redist_enumration`
2. 

## Stat Summary

## Summary Plot
