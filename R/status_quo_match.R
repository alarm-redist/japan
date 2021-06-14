#' Match current destricts
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#' @param sf_district sf object of
#' @param pref_num prefectiural numeric code. 25 for Shiga, for example
#'
#' @return sf object with current legislative destrict code added
#'
#' @concept getdata
#'

status_quo_match <- function(pref, sf_district, pref_code){
  # subset to the interested prefecture
  sf_district <- sf_district %>%
    dplyr::filter(ken == pref_code)

  # Re-projecting df_ district to match df_pref's CRS.
  sf::st_crs(sf_district) <- sf::st_crs(pref)

  sf::st_intersection(pref, sf_district) %>%
    dplyr::select(town, pop, code, ku, geometry)

}
