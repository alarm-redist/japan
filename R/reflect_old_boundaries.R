#' Split a municipality based on boundaries before 平成の大合併
#'
#' @param pref  sf object of cleaned census data for a prefecture
#' @param old_boundary  sf object of administrative boundaries as of the year 2000
#' @param census_mun_old_2020 the output of clean_2020_census()
#' @param new_code  the municipality code of the city that needs to be split
#'
#' @return a data frame object reflecting population of old municipalities
#'
#' @concept getdata
#'
#' @export
#'

reflect_old_boundaries <- function(pref, old_boundary, census_mun_old_2020, new_code){

  #code of municipality to be split: convert input into character; make sure that code is 5 digits
  if(nchar(new_code) == 4){
    new_code_char <- paste0(0, as.character(new_code))
  }else{
    new_code_char <- as.character(new_code)
  }

  #Match CRS
  sf::st_crs(old_boundary) <- sf::st_crs(pref)

  #filter out the municipalities that will not be split
  #(i.e. no need to take into account old boundaries)
  post_gappei_except_for_designated_city <- pref %>%
    dplyr::filter(code != as.numeric(new_code))

  #find the codes of old municipalities
  old_code <- as.character(
    (census_mun_old_2020 %>%
       dplyr::filter(code == as.numeric(new_code)) %>%
       #filter out old municipalities (code = "9")
       dplyr::filter(type_of_municipality == "9"))$pre_gappei_code
  )

  #codes of old_municipality: convert input into character; make sure that code is 5 digits
  if(nchar(old_code[1]) == 4){
    old_code_char <- paste0(0, as.character(old_code))
  }else{
    old_code_char <- as.character(old_code)
  }

  #Clean the data on old boundaries
  pre_gappei_geom <- old_boundary %>%
    dplyr::filter(N03_007 %in% old_code_char) %>%
    dplyr::select(N03_004, N03_007, geometry)
  names(pre_gappei_geom) <- c("municipality", "pre_gappei_code", "geometry")
  pre_gappei_geom <- pre_gappei_geom %>%
    sf::st_make_valid() %>%
    dplyr::group_by(pre_gappei_code) %>%
    dplyr::summarise(geometry =  sf::st_union(geometry))
  #convert to dbl
  pre_gappei_geom$pre_gappei_code <- as.numeric(pre_gappei_geom$pre_gappei_code)

  #filter out population data based on old_boundaries
  pre_gappei_pop <- census_mun_old_2020 %>%
    dplyr::filter(code == as.numeric(new_code)) %>%
    #filter out old municipalities (code = "9")
    dplyr::filter(type_of_municipality == "9")

  # Estimate size of Japanese population (defined as total - foreign)
  pop_total_minus_foreign <- pref[pref$code == new_code,]$pop
  pop_old_Japanese <- sum(pre_gappei_pop$pop)
  pre_gappei_pop <- pre_gappei_pop %>%
    mutate(pop = round(pop*pop_total_minus_foreign/pop_old_Japanese))

  #match geometry column with population data
  old_joint <- dplyr::left_join(pre_gappei_geom, pre_gappei_pop) %>%
    dplyr::select(pre_gappei_code, geometry, code, pop)

  #add columns and prepare to merge together with original data　frame
  if("gun_code" %in% colnames(post_gappei_except_for_designated_city)){
    old_joint$gun_code <- old_joint$pre_gappei_code
  }
  if("koiki_code" %in% colnames(post_gappei_except_for_designated_city)){
    old_joint$koiki_code <- rep(pref$koiki_code[which(pref$code == old_joint$code[1])],
                                times = length(old_joint$code))
  }
  old_joint <- as_tibble(old_joint)
  if("pre_gappei_code" %in% colnames(post_gappei_except_for_designated_city)){
    post_gappei_except_for_designated_city$pre_gappei_code <- post_gappei_except_for_designated_city$pre_gappei_code
  }else{
    post_gappei_except_for_designated_city$pre_gappei_code <- post_gappei_except_for_designated_city$code
  }

  #merge with the data that excludes the designated city
  merged <- dplyr::bind_rows(old_joint, post_gappei_except_for_designated_city)
  merged <- sf::st_as_sf(merged)

  return(merged)
}
