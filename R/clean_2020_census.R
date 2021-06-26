#' Clean latest 2020 data frame
#'
#' @param total downloaded data frame of the total population of 2020 census
#' @param foreigner downloaded data frame of the foreigner population data of 2020 census
#'
#' @return a data frame object of combined and cleaned 2020 census data
#'
#' @concept getdata
#'
#' @export
#'

clean_2020_census <- function(total, foreigner) {

  # clean total data set
  clean_total <- total %>%
    # remove non-data columns
    dplyr::slice(-(1:12)) %>%
    # rename the column names of the population
    dplyr::mutate(total_pop = ...4 ) %>%
    # separate prefecture code and name
    # R cannot treat Japanese character
    # thus, PREF_NAME will be removed later
    tidyr::separate(...2, into = c("PREF", "PREF_NAME")) %>%
    # repeat the process with city code
    tidyr::separate(...3, into = c("code", "CITY_NAME")) %>%
    # subset data
    dplyr::select(PREF, code, total_pop)

  # clean foreigner data set
  # rename first column name for convenience
  names(foreigner)[1] <- "...1"
  # same process with above
  clean_foreigner <- foreigner %>%
    # remove non-data columns
    dplyr::slice(-(1:2)) %>%
    # rename the column names of the population
    dplyr::mutate(foreigner_pop = ...3) %>%
    # separate prefecture code and name
    # R cannot treat Japanese character
    # thus, PREF_NAME will be removed later
    tidyr::separate(...1, into = c("PREF", "PREF_NAME")) %>%
    tidyr::separate(...2, into = c("code", "CITY_NAME")) %>%
    # subset data
    dplyr::select(code, foreigner_pop)

  # combine those two with using `code` column as key
  combined <- clean_total %>%
    left_join(clean_foreigner, by = c('code')) %>%
    # calculate the population of nationals
    dplyr::mutate(pop_national = as.numeric(total_pop) - as.numeric(foreigner_pop))

  # return final
  return(combined)
}
