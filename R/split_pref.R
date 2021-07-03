#' Split prefectures with restrictions
#'
#' @param cleaned_census_shp shp object cleaned by `clean_jcdf` and added population data by `calc_kokumin`
#' @param census2020 data frame of 2020 census cleaned by `clean_2020_census`
#' @param nsplit numerical value of number of splits
#' @param split_codes a vector of the numeric code of the split municipalities (e.g., c(25201, 25203)). To be used with `merge_small`
#' @param intact_codes a vector of the numeric code of the intact_codes for `merge_small`
#' @param old_code a vector of the numeric code of the old code of the split municipalities. To be used with `merge_small`
#' @param merge_gun_exception a vector of the numeric code of the municipalities which are included in the exception list for `merge_gun`.
#'
#' @return a shape file object with the expected population of the split municipalities.
#'
#' @concept clean data for simulation
#'
#' @export
#'
#'
split_pref <- function(
  cleaned_census_shp,
  census2020,
  nsplit,
  split_codes,
  intact_codes,
  old_code,
  merge_gun_exception
){
  pref <- cleaned_census_shp %>%
    dplyr::rename(pop = JINKO)
  # merge small
  if(nsplit == 0){
    pref_n <- pref %>%
      dplyr::group_by(code, CITY_NAME) %>%
      dplyr::summarise(geometry = sf::st_union(geometry)) %>%
      dplyr::left_join(census2020, by = c('code')) %>%
      dplyr::select(code, pop, geometry)
  } else {
    pref_n <- pref %>%
      merge_small(pref = .,
                  split_codes = split_codes,
                  intact_codes = intact_codes)

    # download historical boundary data
    old_boundary <- download_old_shp(pref_code)
    # populations based on historical boundaries
    pop_by_old_boundary <- download_2015pop_old(pref_code = pref_code)

    # reflect old boundaries
    pref_n <- reflect_old_boundaries(pref_n,
                                     old_boundary = old_boundary,
                                     pop_by_old_boundary = pop_by_old_boundary,
                                     old_code = old_code,
                                     new_code = split_codes)

    # estimation of old-boundary level national populations
    for(k in 1:nsplit){

      nat_2020_split_codes <- census2020$pop_national[census2020$code == split_codes[k]]
      pop_2015_split_codes <- sum(pref_n$pop[pref_n$code == old_code[, k]])

      for (i in 1:length(old_code[, k])) {
        old_code_slice <- old_code[, k]
        pref_n$pop[pref_n$code == old_code_slice[i]] <- round(
          pref_n$pop[pref_n$code == old_code_slice[i]] / pop_2015_split_codes * nat_2020_split_codes
        )
      }
    }
  }

  # merge gun
  if(is_null(merge_gun_exception)){
    pref_n <- merge_gun(pref = pref_n)
  } else {
    pref_n <- merge_gun(pref = pref_n,
                        exception = merge_gun_exception)
  }

  return(pref_n)
}
