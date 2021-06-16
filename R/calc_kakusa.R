#' Calculate Ippyo no Kakusa (maximum disparity in voter power)
#'
#' @param pref an sf object with cleaned, collated version of the census data
#' @param plans a redist_plans object, the output of one of the MCMC simulations
#' @param ndists the number of legislative districts in prefecture of choice
#'
#' @return a vector with the ippyo no kakusa for the vector of plans
#'
#' @concept calculate
#'

calc_kakusa <- function(pref, plans, ndists){

  # format the population for the plans
  plan_pops <- matrix(plans$total_pop, ncol = ndists, byrow = TRUE)

  # initialize the matrix for malapportionment
  kakusa <- c()

  # calculate the max/min ratio by row
  for (x in 1:dim(plan_pops)[1]){kakusa <- append(kakusa, max(plan_pops[x, ])/min(plan_pops[x, ]))}

  # return final
  return(kakusa)

}
