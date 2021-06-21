#' Download JCDF (Japan Census Data File)
#'
#' @param path string containing path to the shapefile census data
#'
#' @return a sf object with cleaned, collated version of the census data
#'
#' @concept getdata
#'

clean_jcdf <- function(path) {

  pref_raw <- sf::st_read(path)
  pref_raw <- sf::st_make_valid(pref_raw)

  # filter out water surfaces and extraneous port data
  pref <- NA
  pref <- path %>%
    dplyr::filter(path$KIHON1 != "0000" & path$HCODE != 8154, )  %>%
    dplyr::group_by(PREF, CITY, KIHON1, CITY_NAME) %>%
    # make smallest geopolitical subdivision to level 2
    dplyr::slice(1) %>%
    dplyr::summarize(geometry = sf::st_union(geometry), JINKO = sum(JINKO), S_NAME) %>%
    # make 5 digit municipality code
    dplyr::mutate(code = 1000*as.numeric(PREF) + as.numeric(CITY)) %>%
    dplyr::select(code, CITY_NAME, S_NAME, KIHON1, JINKO, geometry)

  return(pref)
}
