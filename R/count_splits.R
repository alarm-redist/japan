#' Count Number of Municipality Splits
#'
#' @param plans matrix of redistricting plans
#' @param counties vector of county assignments (key)
#'
#' @return a vector with the number of county splits per plan
#'
#' @concept getdata
#'
#' @export
#'

count_splits <- function(plans, counties){

  # find number of plans in matrix
  N <- ncol(plans)

  # initializing the vector containing the number of splits
  splits_vec <- vector(mode = "list", length = N)

  # establish unique counties
  orig <- unique(counties)

  # count splits
  for(n in 1:N){

    n_county <- 0.0
    for(i in 1:length(orig)){

      n_county <- n_county + length(unique(plans[which(counties$code == orig[i]), n]))

    }
    splits_vec[n] <- n_county - length(orig)

  }

  return(unlist(splits_vec))

}

