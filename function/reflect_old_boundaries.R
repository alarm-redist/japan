#' Split a municipality based on boundaries before 平成の大合併
#'
#' @param pref_mun  sf object of cleaned census data for a prefecture
#' @param old_mun  sf object of administrative boundaries as of the year 2020
#' @param census_mun_old_2020 the output of `clean_2020_census()`
#' @param split_code  the municipality code of the municipality that needs to be split
#'
#' @return a data frame object reflecting population of old municipalities
#'
#' @concept getdata
#'
#' @export
#'

reflect_old_boundaries <- function(pref_mun, old_mun, census_mun_old_2020, split_code){

  if(length(split_code) == 0){
    merged <- pref_mun
    merged$pre_gappei_code <- merged$code
    merged$old_mun_name <- NA
  }else{

    # Code of municipality to be split: convert input into character; make sure that code is 5 digits
    if(nchar(split_code) == 4){
      split_code_char <- paste0(0, as.character(split_code))
    }else{
      split_code_char <- as.character(split_code)
    }

    # Match CRS
    sf::st_crs(old_mun) <- sf::st_crs(pref_mun)

    # Filter out the municipalities that will not be split
    # (i.e. no need to take into account old boundaries)
    post_gappei_except_for_designated_city <- pref_mun %>%
      dplyr::filter(code != as.numeric(split_code))

    # Find the codes of the old municipalities
    old_code <- census_mun_old_2020 %>%
         dplyr::filter(code == as.numeric(split_code)) %>%
         # Filter out old municipalities (code = "9")
         dplyr::filter(type_of_municipality == "9") %>%
         dplyr::pull(pre_gappei_code)

    # Codes of old_municipality: convert input into character; make sure that code is 5 digits
    if(nchar(old_code[1]) == 4){
      old_code_char <- paste0(0, as.character(old_code))
    }else{
      old_code_char <- as.character(old_code)
    }

    # Clean the data on old boundaries
    pre_gappei_geom <- old_mun %>%
      dplyr::filter(N03_007 %in% old_code_char) %>%
      dplyr::select(N03_004, N03_007, geometry) %>%
      dplyr::mutate(N03_004 = paste0("旧", N03_004),
                    N03_007 = as.numeric(N03_007))
    names(pre_gappei_geom) <- c("old_mun_name", "pre_gappei_code", "geometry")
    pre_gappei_geom <- pre_gappei_geom %>%
      sf::st_make_valid() %>%
      dplyr::group_by(pre_gappei_code) %>%
      dplyr::summarise(geometry =  sf::st_union(geometry),
                       old_mun_name = old_mun_name[1])

    # Filter out population data based on old_boundaries
    pre_gappei_pop <- census_mun_old_2020 %>%
      dplyr::filter(code == as.numeric(split_code)) %>%
      # Filter out old municipalities
      dplyr::filter(type_of_municipality == "9")

    # Estimate size of Japanese population (defined as total - foreign)
    pop_total_minus_foreign <- pref_mun[pref_mun$code == split_code,]$pop
    pop_old_Japanese <- sum(pre_gappei_pop$pop)
    pre_gappei_pop <- pre_gappei_pop %>%
      mutate(pop = round(pop*pop_total_minus_foreign/pop_old_Japanese)) %>%
      select(-type_of_municipality)

    # Estimate baseline number of voters per party
    # Partisanship data
    pref_mun_partisan <- pref_mun %>%
      filter(code == split_code) %>%
      sf::st_drop_geometry() %>%
      select(tidyselect::starts_with("nv"))

    # Partisanship data in each "old" municipality
    pref_old_partisan <-  setNames(data.frame(matrix(ncol = ncol(pref_mun_partisan),
                                                     nrow = nrow(pre_gappei_pop))),
                                   colnames(pref_mun_partisan))
    # Estimate baseline number of voters per party in each "old" municipality
    for(i in 1:nrow(pre_gappei_pop)){
      for(j in 1:ncol(pref_mun_partisan)){
        pref_old_partisan[i,j] <-
          pref_mun_partisan[1,j] *
          pre_gappei_pop$pop[i]/ sum(pre_gappei_pop$pop)
      }
    }

    # Bind together with `pre_gappei_pop` and match with geometry column
    old_joint <- cbind(pre_gappei_pop, pref_old_partisan) %>%
      dplyr::full_join(pre_gappei_geom, by = "pre_gappei_code")

    # Add column `mun_name`
    old_joint$mun_name <- rep(pref_mun$mun_name[which(pref_mun$code == old_joint$code[1])],
                              times = length(old_joint$code))

    # Convert to tibble
    old_joint <- as_tibble(old_joint)

    # Add column `pre_gappei_code`
    if("pre_gappei_code" %in% colnames(post_gappei_except_for_designated_city)){
      post_gappei_except_for_designated_city$pre_gappei_code <- post_gappei_except_for_designated_city$pre_gappei_code
    }else{
      post_gappei_except_for_designated_city$pre_gappei_code <- post_gappei_except_for_designated_city$code
    }

    # Merge with the data that excludes the municipalities that are not split
    merged <- dplyr::bind_rows(old_joint, post_gappei_except_for_designated_city)
    merged <- sf::st_as_sf(merged)

  }

  return(merged)
}
