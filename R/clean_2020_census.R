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
    dplyr::mutate(pop_total = ...4 ) %>%
    # separate prefecture code and name
    # R cannot treat Japanese character
    # thus, CITY_NAME will be removed later
    tidyr::separate(...3, into = c("code", "CITY_NAME")) %>%
    # subset data with converting objects to numeric
    dplyr::summarise(code = sprintf('%05d', as.numeric(code)),
                     pop_total = as.numeric(pop_total))

  # clean foreigner data set
  # same process with above
  clean_foreigner <- foreigner %>%
    # remove non-data columns
    dplyr::slice(-(1:2)) %>%
    # rename the column names of the population
    dplyr::mutate(pop_foreigner = ...3) %>%
    # separate prefecture code and name
    # R cannot treat Japanese character
    # thus, CITY_NAME will be removed later
    tidyr::separate(...2, into = c("code", "CITY_NAME")) %>%
    # subset data
    dplyr::summarise(code = sprintf('%05d', as.numeric(code)),
                     pop_foreigner = as.numeric(pop_foreigner))

  # combine those two with using `code` column as key
  combined <- clean_total %>%
    dplyr::left_join(clean_foreigner, by = c('code')) %>%
    # calculate the population of nationals
    dplyr::mutate(pop_national = pop_total - pop_foreigner)

  # return final
  return(combined)
}
