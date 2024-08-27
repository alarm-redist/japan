#' Estimate 2020 Populations For Old Boundary (before Heisei-Dai-Gappei) Municipalities
#'
#' @param old_codes old codes that make up new municipality
#' @param new_code new municipality code
#' @param pref an sf object with cleaned, collated version of the census data
#' @param census2020 output of clean_2020_census()
#'
#' @return pref object with modified pop column with estimates
#'
#' @concept estimatedata
#'
#' @export
#'

estimate_old_boundary_pop <- function(old_code, new_code, pref, census2020) {

  # initialize pref_new object
  pref_new <- pref

  # population of Japanese nationals in municipality new_code for 2020
  nat_2020 <- (census2020 %>% dplyr::filter(code == as.numeric(new_code), ))$pop_national
  pop_2015 <- sum(pref[which(pref$pre_gappei_code %in% old_code), ]$pop)

  # conduct estimates using simple rounding proportional method
  for (i in 1:length(old_code)) {
    pref_new[which(pref_new$pre_gappei_code == old_code[i]), ]$pop <-
      round(pref_new[which(pref_new$pre_gappei_code == old_code[i]), ]$pop / pop_2015 * nat_2020)
  }

  row.names(pref_new) <- NULL

  pref_new <- sf::st_as_sf(pref_new)

  # return result
  return(pref_new)

}
