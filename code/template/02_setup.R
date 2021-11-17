# Clean census data
pref <- pref_raw %>%
  clean_jcdf() %>%
  dplyr::group_by(code, CITY_NAME) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  dplyr::left_join(census2020, by = c('code')) %>%
  dplyr::rename(pop = pop_national) %>%
  dplyr::select(code, pop, geometry)
  
# Add information about gun (郡)
pref <- merge_gun(pref)

# TODO Define the koiki-renkei areas (広域連携)
a_koiki <- c()
b_koiki <- c()
c_koiki <- c()
koiki <- c(a_koiki, b_koiki, c_koiki)

# Run simulations for n splits

