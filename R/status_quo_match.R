#' Match current districts
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#'
#' @return sf object with current legislative district code added
#'
#' @concept getdata
#'
#' @export
#'

status_quo_match <- function(pref){

  # find the prefecture name
  pref_num <- floor(pref$code[1]/1000)

  # improt SMD shapefile data
  sf_district <- data("district_data")

  # subset to the interested prefecture
  sf_district <- sf_district %>%
    dplyr::filter(ken == pref_num)

  # re-projecting sf_district to match pref's CRS.
  sf::st_crs(sf_district) <- sf::st_crs(pref)

  # find intersection to label the districts
  pref_ku <- sf::st_intersection(pref, sf_district) %>%
    dplyr::select(town, pop, code, ku, geometry)

  # relabel columns
  names(pref_ku) <- c("town", "pop", "code", "cd", "geometry")

  # return the result
  return(pref_ku)

}

