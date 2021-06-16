#' Match current districts
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#' @param sf_district sf object for legislative districts
#' @param pref_num numeric prefix for prefecture (e.g. Hokkaido: 1, Shiga: 25).
#'
#' @return sf object with current legislative district code added
#'
#' @concept getdata
#'

status_quo_match <- function(pref, sf_district, pref_code){

  # subset to the interested prefecture
  sf_district <- sf_district %>%
    dplyr::filter(ken == pref_code)

  # Re-projecting sf_district to match pref's CRS.
  sf::st_crs(sf_district) <- sf::st_crs(pref)

  pref_ku <- sf::st_intersection(pref, sf_district) %>%
    dplyr::select(town, pop, code, ku, geometry)

  names(pref_ku) <- c("town", "pop", "code", "cd", "geometry")

  return(pref_ku)
}

