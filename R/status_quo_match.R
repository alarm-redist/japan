#' Match current districts
#'
#' @param pref sf object of cleaned, collated, census data for a prefecture
#'
#' @return sf object with current legislative district code added
#'
#' @concept getdata
#'
#' @importFrom dplyr %>%
#'
#' @export
#'

status_quo_match <- function(pref){

  # initialize object
  pref_ku <- pref

  # find the prefecture name
  pref_num <- floor(pref$code[1]/1000)

  # import district data
  data("district_data")

  # subset to the interested prefecture
  district_data <- district_data %>%
    dplyr::filter(district_data$ken == pref_num, )

  district_data <- sf::st_make_valid(district_data)

  # re-projecting sf_district to match pref's CRS.
  sf::st_crs(district_data) <- sf::st_crs(pref)

  pref_points <- sf::st_centroid(pref)

  # find intersection to label the districts
  ku <- district_data[unlist(nngeo::st_nn(pref_points, district_data)), ]$ku

  pref_ku$ku <- ku

  # return the result
  return(pref_ku)

}

