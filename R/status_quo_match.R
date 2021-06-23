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
  data("district_data")

  # subset to the interested prefecture
  district_data <- district_data %>%
    dplyr::filter(as.numeric(district_data$ken == pref_num), )

  # re-projecting sf_district to match pref's CRS.
  sf::st_crs(district_data) <- sf::st_crs(pref)

  # find intersection to label the districts
  pref_ku <- sf::st_intersection(pref, district_data) %>%
    dplyr::select(KIHON1, JINKO, code, ku, geometry)

  # return the result
  return(pref_ku)

}

