#' Remove a Lake
#'
#' @param pref_raw sf object of census data for a prefecture
#' @param name_lake character vector containing name of lakes
#'
#' @return an array with the municipality name and corresponding codes
#'
#' @concept getdata
#'
#' @export
#'

# Function
remove_lake <- function(pref_raw, name_lake){

  # import lake port data
  load(file = "./data-raw/lake_data.rda")

  # re-projecting sf_lake to match pref_raw's CRS.
  sf::st_crs(lake_data) <- sf::st_crs(pref_raw)

  # specify lake
  lake_data <- lake_data %>%
    dplyr::filter(W09_001 %in% name_lake) %>%
    sf::st_sf()

  # find geographical difference
  pref_nolake <- sf::st_difference(pref_raw, lake_data) 

  # return result
  return(pref_nolake)

}

