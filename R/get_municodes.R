#' Get Municipality Codes
#'
#' @param pref_raw sf object of census data for a prefecture
#'
#' @return an array with the municipality name and corresponding codes
#'
#' @concept getdata
#'

get_municodes <- function(pref_raw){

  # find the unique municipality codes
  municodes <- unique(data.frame(code = 1000*as.numeric(pref_raw$PREF) + as.numeric(pref_raw$CITY),
                                 municipality = pref_raw$CITY_NAME))

  # reorder and reindex rows
  municodes <- municodes[order(municodes[, 1]), ]
  rownames(municodes) <- 1:nrow(municodes)

  # return result
  return(municodes)

}
