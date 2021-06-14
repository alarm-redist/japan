#' Remove a Lake
#'
#' @param pref_raw sf object of census data for a prefecture
#' @param sf_lake sf object of GIS data of Japanese lakes
#' @param name_lake name of character vector. Use quotation marks.
#'
#' @return an array with the municipality name and corresponding codes
#'
#' @concept getdata
#'

# Function
remove_lake <- function(pref_raw, sf_lake, name_lake){

  # Re-projecting df_lake to match sf_pref's CRS.
  sf::st_crs(sf_lake) <- sf::st_crs(pref_raw)

  # Specify lake
  sf_lake <- sf_lake %>%
    dplyr::filter(W09_001 == name_lake) %>%
    sf::st_sf()

  # Difference
  sf::st_difference(pref_raw, sf_lake)
}

