#' Split prefectures with restrictions
#'
#' @param pref a cleaned shapefile object of jcdf
#' @param census2020 a cleaned data frame of 2020 census
#' @param old_boundary an old jcdf
#' @param pop_by_old_boundary a data frame of 2015 census
#' @param nsplit a numerical value of number of splits
#' @param split_codes a vector of the numeric code of the split municipalities (e.g., c(25201, 25203)). To be used with `merge_small`
#' @param intact_codes a vector of the numeric code of the intact_codes for `merge_small`
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
  pref,
  census2020,
  old_boundary,
  pop_by_old_boundary,
  nsplit,
  split_codes,
  intact_codes,
  merge_gun_exception
){

  ######### wrangle data into for the municipality split#########
  # merge small
  if(nsplit == 0){

    # no split
    pref_n <- pref %>%
      merge_small(pref = ., intact_codes = intact_codes) %>%
      dplyr::left_join(census2020, by = c('code')) %>%
      dplyr::select(code, geometry, pop_national) %>%
      dplyr::rename(pop = pop_national)

    ifelse(is.null(merge_gun_exception),
           pref_n <- merge_gun(pref = pref_n),
           pref_n <- merge_gun(pref = pref_n,
                               exception = merge_gun_exception))

  } else {

    # n_split
    pref_n <- pref %>%
      merge_small(pref = .,
                  split_codes = split_codes,
                  intact_codes = intact_codes)

    # merge gun
    ifelse(is.null(merge_gun_exception),
           pref_n <- merge_gun(pref = pref_n),
           pref_n <- merge_gun(pref = pref_n,
                               exception = merge_gun_exception))

    # estimation of old-boundary level national populations
    for(k in 1:nsplit){

      old_code <- find_old_codes(new_code = split_codes[k],
                                 pop_by_old_boundary = pop_by_old_boundary)
      # reflect old boundaries
      pref_n <- reflect_old_boundaries(pref_n,
                                       old_boundary = old_boundary,
                                       pop_by_old_boundary = pop_by_old_boundary,
                                       old_code = old_code,
                                       new_code = split_codes[k])

      pref_n <- estimate_old_boundary_pop(old_codes = old_code,
                                          new_code = split_codes[k],
                                          pref = pref_n,
                                          census2020 = census2020)

    }

    # make geometry valid
    pref_n <- sf::st_make_valid(pref_n)

    row.names(pref_n) <- NULL

  }

  return(pref_n)
}
