# jcdf

# Work Flow

## Download Data   
1. `download_shp`
2. `download_2020_census`

## Clean Data 
1. `clean_jcdf`
2. `clean_2020_census`
3. Combine them. For example,   
   COMBINED <- pref %>%   
               dplyr::group_by(code, CITY_NAME) %>%  
               summarise(geometry = sf::st_union(geometry)) %>%  
               dplyr::left_join(census2020, by = c('code'))
  
## Add/Remove Elements
1. `add_ferries`
2. `remove_lake`
3. `status_quo_match` *Use this before grouping by the municipality*
