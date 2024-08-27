#' Find old municipality codes, given the new merged code after Heisei-Dai-Gappei
#'
#' @param new_code numeric code for new municipality (2020)
#' @param pop_by_old_boundary output of download_2015pop_old()
#'
#'
#' @return a vector of the old municipality codes making up new municipality
#'
#'
#' @concept find
#'
#' @export
#'

find_old_codes <- function(new_code, pop_by_old_boundary){

  # find entries for old code and extract
  result <- pop_by_old_boundary %>%
    dplyr::filter(X == new_code & X.3 == 2000 & X.1 == 9, )

  pref_code <- as.numeric(substr((result$X [1]), 1, 2))

  result <- 1000 * pref_code + readr::parse_number(result$X.4)

  # return result
  return(result)

}
