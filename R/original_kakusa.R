#' Calculate Original Ippyo-no-kakusa (maximum weight disparity in voter power)
#'
#' @param pref an sf object with cleaned, collated version of the census data
#'
#' @return a vector with the ippyo-no-kakusa for the oiginal plan
#'
#' @concept calculate
#'

original_kakusa <- function(pref){

  orig <- pref %>%
    group_by(cd) %>%
    summarise(pop=sum(pop))

  original_kakusa <- max(orig$pop)/min(orig$pop)

  return(original_kakusa)
}
