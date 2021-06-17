#' Calculate Original Ippyo-no-kakusa (maximum weight disparity in voter power)
#'
#' @param pref an sf object with cleaned, collated version of the census data
#'
#' @return a vector with the ippyo no kakusa for the oiginal plan
#'
#' @concept calculate
#'

original_weight_disparity <- function(pref){

  orig <- pref %>%
    group_by(cd) %>%
    summarise(pop=sum(pop))

  original_weight_disparity <- max(orig$pop)/min(orig$pop)

  return(original_weight_disparity)
}
