#' Match current districts
#'
#' @param pref_raw sf object of cleaned, collated, census data for a prefecture
#'
#' @return sf object with current legislative district code added
#'
#' @concept getdata
#'
#' @importFrom dplyr %>%
#'
#' @export
#'

status_quo_match <- function(pref_raw){

  # initialize object
  pref_ku <- pref_raw

  # find the prefecture name
  pref_num <- floor(as.numeric(pref_raw$KEY_CODE[1])/10000000)

  # import district data
  download.file('https://home.csis.u-tokyo.ac.jp/~nishizawa/senkyoku/senkyoku289polygon_detailed.zip', 'data-raw/senkyoku289polygon_detailed.zip')

  # unzip the downloaded zip file
  unzip("data-raw/senkyoku289polygon_detailed.zip", exdir = "data-raw/")

  # return the shp file
  district_data <- sf::st_read('data-raw/senkyoku289polygon_detailed/senkyoku289polygon_detailed.shp')

  # remove the zip file after zip is decompressed
  file.remove("data-raw/senkyoku289polygon_detailed.zip")

  # subset to the interested prefecture
  district_data <- district_data %>%
    dplyr::filter(district_data$ken == pref_num, )

  district_data <- sf::st_make_valid(district_data)

  # re-projecting sf_district to match pref's CRS.
  sf::st_crs(district_data) <- sf::st_crs(pref_raw)

  district_data <- district_data[which(sf::st_is_empty(district_data) != 1), ]
  pref_points <- sf::st_centroid(pref_raw)

  # find intersection to label the districts
  ku <- district_data[unlist(nngeo::st_nn(pref_points, district_data)), ]$ku

  pref_ku$ku <- ku

  # return the result
  return(pref_ku)

}

